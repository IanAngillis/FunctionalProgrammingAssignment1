--------------------------------------------------------
-- Functional Programming: solutions lab session XII
--------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding (Product)
import Control.Parallel
import Control.Concurrent
import Control.Concurrent.STM

import Data.List

import System.Environment
import System.Random

import Text.Parsec hiding (State, token)
import Text.Parsec.String
import Text.Parsec.Char


main = mainPhilosopher

-------------------------------------------------------
-- Section 1. Monads

-- 1.
type Pos = (Int,Int)
type Dir = (Int,Int)
type Commands = String

bibop1 :: Commands -> Pos -> Dir -> Writer [Pos] Pos
bibop1 [] p d = return p
bibop1 ('F' : cs) (x,y) (dx,dy) = bibop1 cs (x+dx,y+dy) (dx,dy)
bibop1 ('B' : cs) (x,y) (dx,dy) = bibop1 cs (x-dx,y-dy) (dx,dy)
bibop1 ('L' : cs) p     (dx,dy) = bibop1 cs p (-dy,dx)
bibop1 ('R' : cs) p     (dx,dy) = bibop1 cs p (dy,-dx)
bibop1 ('M' : cs) p     d       = tell [p] >> bibop1 cs p d


bibop2 :: Commands -> Pos -> Dir -> State [Pos] (Maybe Pos)
bibop2 [] p d = return (Just p)
bibop2 ('F' : cs) (x,y) (dx,dy) = bibop2 cs (x+dx,y+dy) (dx,dy)
bibop2 ('B' : cs) (x,y) (dx,dy) = bibop2 cs (x-dx,y-dy) (dx,dy)
bibop2 ('L' : cs) p     (dx,dy) = bibop2 cs p (-dy,dx)
bibop2 ('R' : cs) p     (dx,dy) = bibop2 cs p (dy,-dx)
bibop2 ('M' : cs) p     d       = do
  ms <- get
  if p `elem` ms
    then return Nothing
    else put (p:ms) >> bibop2 cs p d

-- 2.
data Expr = Const Float
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Var String
          | Let String Expr Expr
          deriving (Eq, Show)

type Env = [(String,Float)]

-- 2.a
eval :: Expr -> Reader Env Float
eval (Const r) = return r
eval (Plus e1 e2) = do r1 <- eval e1
                       r2 <- eval e2
                       return $ r1 + r2
eval (Minus e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Times e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = (/) <$> eval e1 <*> eval e2
eval (Var v) = do
  env <- ask
  case lookup v env of
    Nothing -> return 0
    Just r -> return r
eval (Let v ev e) = do
  x <- eval ev
  local ((v,x):) $ eval e

-- 2.b
eval' :: Expr -> ReaderT Env (Either String) Float
eval' (Const r) = return r
eval' (Plus e1 e2) = (+) <$> eval' e1 <*> eval' e2
eval' (Minus e1 e2) = (-) <$> eval' e1 <*> eval' e2
eval' (Times e1 e2) = (*) <$> eval' e1 <*> eval' e2
eval' (Div e1 e2) = (/) <$> eval' e1 <*> eval' e2
eval' (Var v) = do
  env <- ask
  case lookup v env of
    Nothing -> lift $ throwError ("Variable " ++ v ++ " is not in scope.")
    Just r -> return r
eval' (Let v ev e) = do
  x <- eval' ev
  local ((v,x):) $ eval' e

-- 2.c
eval'' :: (MonadReader Env m, MonadError String m) => Expr -> m Float
eval'' (Const r) = return r
eval'' (Plus e1 e2) = (+) <$> eval'' e1 <*> eval'' e2
eval'' (Minus e1 e2) = (-) <$> eval'' e1 <*> eval'' e2
eval'' (Times e1 e2) = (*) <$> eval'' e1 <*> eval'' e2
eval'' (Div e1 e2) = (/) <$> eval'' e1 <*> eval'' e2
eval'' (Var v) = do
  env <- ask
  case lookup v env of
    Nothing -> throwError ("Variable " ++ v ++ " is not in scope.")
    Just r -> return r
eval'' (Let v ev e) = do
  x <- eval'' ev
  local ((v,x):) $ eval'' e

test1 = runReaderT (eval'' $ Plus (Const 2) (Times (Const 3) (Const 4))) [] :: Either String Float
test2 = runReaderT (eval'' $ Let "x" (Const 3) (Times (Var "x") (Var "y"))) [("y",6)] :: Either String Float
test3 = runReaderT (eval'' $ Let "x" (Const 3) (Times (Var "x") (Var "y"))) [] :: Either String Float


--------------------------------------------------------
-- Section 2. Writing Parsers with Parsec

-- 3.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
x <:> xs = (:) <$> x <*> xs



number = many1 digit



word :: Parser String
word = many1 letter



negative = char '-' <:> number

int :: Parser String
int = negative <|> number

float :: Parser Float
float = do
  w <- int
  d <- option "" $ char '.' <:> number
  e <- option "" $ oneOf "eE" <:> int
  return . read $ w ++ d ++ e


--parseRequest::Parser String
--parseRequest = try addTeacherRequest <|> try addStudentRequest

--addTeacherRequest::Parser String
--addTeacherRequest = do
--  ss <- keyword "add-teacher"
--  tt <- word
--  return (ss ++ " " ++tt)

--addStudentRequest::Parser String
--addStudentRequest = string "add-student"

--word::Parser Char -> Parser String
--word = token . many1 letter

--request::Parser String
--request = do
--   parseRequest

-- Some helpers for exercise 4.
token :: Parser a -> Parser a
token p = try $ spaces >> p

keyword :: String -> Parser String
keyword = token . string

symbol = keyword

-- 4.
addop = do {symbol "+" ; return Plus} <|>
        do {symbol "-" ; return Minus}

mulop = do {symbol "*" ; return Times} <|>
        do {symbol "/" ; return Div}

identifier = do
  idf <- token $ lower <:> many alphaNum
  guard . not $ idf `elem` ["let", "in"]
  return idf

letParser = do
  keyword "let"
  var <- label identifier "valid identifier"
  symbol "="
  varExpr <- expr
  keyword "in"
  bodyExpr <- expr
  return $ Let var varExpr bodyExpr

constParser = token float >>= return . Const

varParser = identifier >>= return . Var

term = factor `chainl1` mulop

factor = choice
  [ constParser
  , varParser
  , between (symbol "(") (symbol ")") expr
  ]

expr :: Parser Expr
expr = try letParser <|> (term `chainl1` addop)

evalParseReader :: String -> ReaderT Env (Either String) Float
evalParseReader s = lift (either (Left . show) Right $ parse expr "" s) >>= eval'

evalParse :: String -> Either String Float
evalParse s = runReaderT (evalParseReader s) []

--------------------------------------------------------
-- Section 3. Parallel Programming in Haskell

-- 6.
-- We present many different variations.
-- You can test for every version how the changes influence performance
-- by using the profiling techniques from lab session IV.
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =
  let lower = quicksort $ filter (<  x) xs
      upper = quicksort $ filter (>= x) xs
  in lower ++ (x : upper)

parquicksort1 :: [Int] -> [Int]
parquicksort1 [] = []
parquicksort1 (x:xs) =
  let lower = parquicksort1 $ filter (<  x) xs
      upper = parquicksort1 $ filter (>= x) xs
  in lower `par` upper `pseq` lower ++ (x : upper)

forceList :: [a] -> ()
forceList xs = go xs `pseq` ()
  where
    go (_:xs) = go xs
    go [] = 1

parquicksort2 :: [Int] -> [Int]
parquicksort2 [] = []
parquicksort2 (x:xs) =
  let lower = parquicksort2 $ filter (<  x) xs
      upper = parquicksort2 $ filter (>= x) xs
  in forceList lower `par` forceList upper `pseq` lower ++ (x : upper)

parquicksort3 :: Int -> [Int] -> [Int]
parquicksort3 n [] = []
parquicksort3 0 xs = quicksort xs
parquicksort3 n (x:xs) =
  let n' = n - 1
      lower = parquicksort3 n' $ filter (<  x) xs
      upper = parquicksort3 n' $ filter (>= x) xs
  in forceList lower `par` forceList upper `pseq` lower ++ (x : upper)

parquicksort4 :: Int -> [Int] -> [Int]
parquicksort4 n [] = []
parquicksort4 0 xs = quicksort xs
parquicksort4 n (x:xs) =
  let n' = n - 1
      lower = parquicksort4 n' $ filter (<  x) xs
      upper = parquicksort4 n' $ filter (>= x) xs
  in lower `par` upper `pseq` lower ++ (x : upper)

-- 7.
grep :: String -> String -> [String]
grep keyword = map showPair . filter (\x -> keyword `isInfixOf` snd x) . zip [1..] . lines
  where showPair (n, s) = show n ++ ":" ++ s

-- We use TMVars to communicate between threads.
-- These are MVars implemented using TVars (with the blocking behaviour from TVars).
grepWorker :: TMVar String -> String -> FilePath -> IO ()
grepWorker tmv keyword fp = do
  contents <- readFile fp
  let matchLines = grep keyword contents
      result = unlines $ map ((fp ++ "@") ++) matchLines
  atomically $ putTMVar tmv result
  return()

mainGrep = do
  (keyword:fps) <- getArgs
  tmvs <- replicateM (length fps) newEmptyTMVarIO
  mapM_ (\(tmv,fp) -> forkIO $ grepWorker tmv keyword fp) $ zip tmvs fps
  mapM_ (\tmv -> atomically (takeTMVar tmv) >>= putStr) tmvs

-- 8.
type Product = String
type Order = [(Product,Int)]
type Store = [(Product,TVar Int)]


order1 :: (Product,Int) -> Store -> STM Bool
order1 (prod,quantity) store = case lookup prod store of
  Nothing -> return False
  Just v -> do
    nrAvailable <- readTVar v
    check $ quantity <= nrAvailable
    writeTVar v $ nrAvailable - quantity
    return True

order :: Order -> Store -> STM Bool
order mo store = and <$> mapM (\p -> p `order1` store) mo

supply1 :: (Product,Int) -> Store -> STM ()
supply1 (prod,quantity) store = case lookup prod store of
  Nothing -> return ()
  Just v -> modifyTVar v (+quantity)

supply :: Order -> Store -> STM ()
supply mo store = mapM_ (`supply1` store) mo

catalogue = ["book", "headphones", "board game", "vaccine", "Christmas tree"]

randomDelay :: (Int,Int) -> IO ()
randomDelay = randomRIO >=> threadDelay

randomSubList :: [a] -> IO [a]
randomSubList [] = return []
randomSubList (x:xs) = do
  b <- randomIO
  rs <- randomSubList xs
  return $ if b then x:rs else rs

-- Strictly speaking, it could happen that two clients or suppliers try to
-- print a message to stdout at the same time, but this is unlikely because
-- of the random delay.
-- However, it is good to keep in mind that putStr (and hence putStrLn and print)
-- is not thread-safe. You can work around this by using a mutex (e.g. implemented
-- as an MVar).
client :: Int -> Store -> IO ()
client n store = forever $ do
  items <- randomSubList catalogue
  toOrder <- zip items <$> replicateM (length items) (randomRIO (1,5))
  putStrLn $ "Client " ++ show n ++ " wants to make the following order: " ++ show toOrder
  atomically $ order toOrder store
  putStrLn $ "The order of client " ++ show n ++ " has been processed successfully."
  randomDelay (2 * 1000 * 1000, 7 * 1000 * 1000)

supplier :: Store -> IO ()
supplier store = forever $ do
  toSupply <- zip catalogue <$> replicateM (length catalogue) (randomRIO (5,10))
  atomically $ supply toSupply store
  putStrLn $ "Supply has been replenished with the following items: " ++ show toSupply
  randomDelay (10 * 1000 * 1000, 20 * 1000 * 1000)
mainStore = do
  store <- zip catalogue <$> replicateM (length catalogue) (newTVarIO 10)
  forM_ [1..5] $ forkIO . flip client store
  forkIO $ supplier store
  getLine
  return ()

-- 9.
philosophers = ["Aristotle", "Plato", "Confucius", "Descartes", "Kant"]

-- The same remark as in the previous exercise applies here, now concerning
-- two philosophers that might want to print to stdout simultaneously.
philosopherAction :: String -> String -> IO()
philosopherAction phil action = do
  putStrLn $ phil ++ " is " ++ action
  randomDelay (1 * 1000 * 1000, 5 * 1000 * 1000)

type Fork = TVar Bool

takeFork :: Fork -> STM ()
takeFork f = do
  b <- readTVar f
  check b
  writeTVar f False

putFork :: Fork -> STM ()
putFork f = do
  b <- readTVar f
  if b
    then error "This should never happen."
    else writeTVar f True

philosopher :: (String, (Fork, Fork)) -> IO ()
philosopher (phil, (f1, f2)) = forever $ do
  philosopherAction phil "hungry"
  atomically $ takeFork f1 >> takeFork f2
  philosopherAction phil "eating"
  atomically $ putFork f1 >> putFork f2
  philosopherAction phil "thinking"

mainPhilosopher = do
  fs <- replicateM (length philosophers) (newTVarIO True)
  mapM_ (forkIO . philosopher) $ zip philosophers $ zip fs $ tail fs ++ [head fs]
  getLine
  return ()


test p = parse p ""

