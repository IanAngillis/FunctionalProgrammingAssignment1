import Doodle
import Data.List
import qualified Data.Map as Map

type Name = String
type Participants = [Name]
data  Timeslot t = Timeslot t t Participants deriving (Show, Ord)


-- gaat er vanuit dat t1 < t2 en t3 < t4 als de timslots ergens overloppen zijn ze equal
instance Ord t => Eq (Timeslot t) where
    (Timeslot t1 t2 _) == (Timeslot t3 t4 _) = (t2 > t3 && t1 <= t3) || (t4 > t1 && t3 <= t1)


type Title = String
data Table t = Table Title [Timeslot t] deriving (Show, Eq)

type TimeTable = Table Int
type IntPool = DoodlePool Int TimeTable

newDoodle :: IntPool
newDoodle = wrap Map.empty

newTimeslot :: t -> t -> Timeslot t
newTimeslot s e = Timeslot s e []

getStartTime :: Timeslot t -> t
getStartTime (Timeslot s _ _) = s

getEndTime :: Timeslot t -> t
getEndTime (Timeslot _ e _) = e

getParticipants :: Timeslot t -> Participants
getParticipants (Timeslot _ _ ps) = ps

addParticipant :: Name -> Timeslot t -> Timeslot t
addParticipant n (Timeslot s e ps) = Timeslot s e (n:ps)

removeParticipant :: Name -> Timeslot t -> Timeslot t
removeParticipant n (Timeslot s e ps) = Timeslot s e (delete n ps)

updateParticipants :: (Participants -> Participants) -> Timeslot t -> Timeslot t
updateParticipants f (Timeslot s e ps) = Timeslot s e (f ps)


getTitle :: Table t -> Title
getTitle (Table tit _) = tit

getSlot :: Table t -> Int -> Timeslot t
getSlot (Table _ slots) = (!!) slots 

updateSlot :: Int -> (Timeslot t -> Timeslot t) -> Table t -> Table t
updateSlot index f (Table t slots) = Table t $ updateIndex index f slots

-- dit is een alternatief
--  removeIndex :: Int -> [a] -> [a]
--  removeIndex i l = do
--          e <- lookup i $ zip [0..] l
--          delete e

removeIndex :: Int -> [a] -> [a]
removeIndex _ []     = []   
removeIndex 0 (x:xs) = xs
removeIndex i (x:xs) = x : removeIndex (i-1) xs

updateIndex :: Int -> (a -> a) -> [a] -> [a]
updateIndex _ _ [] = error "index out of bound"
updateIndex 0 f (x:xs) = (f x) : xs
updateIndex i f (x:xs) = x : updateIndex (i-1) f xs

replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i el = updateIndex i (\_ -> el)


toggleList :: (Eq a) => a -> [a] -> [a]
toggleList el [] = [el]                 -- element not in list -> add element
toggleList el (x:xs)
 |  el == x   = xs                    -- remove element
 |  otherwise = x : toggleList el xs  -- keep searching

instance Doodle Table where
    initialize s              = Table s []
    add (ts, te) old@(Table t ls) = let nt = newTimeslot ts te in if not $ elem nt ls then Table t (nt : ls) else old -- throw error hier als dat gewenst is
    remove i (Table t ls)     = Table t $ removeIndex i ls
    toggle name index ds      = updateSlot index (updateParticipants $ toggleList name) ds



data DoodlePool k t = DoodlePool (Map.Map k t)

unwrap :: DoodlePool k t -> Map.Map k t
unwrap (DoodlePool m) = m

wrap :: Map.Map k t -> DoodlePool k t
wrap m = DoodlePool m

-- expects an ascending ordered list
newKey :: (Ord k, Enum k) => [k] -> k
newKey = toEnum . newKey' . map fromEnum 
  where
    newKey' :: [Int] -> Int
    newKey' [] = 0
    newKey' [key]
      | key /= 0  = 0
      | otherwise = 1 
    newKey' (x:xs) = newKey'' x xs

    newKey'' keylow [] = keylow + 1
    newKey'' keylow (keyhigh : keys)
      |  (keyhigh - keylow) > 1 = keylow + 1
      |  otherwise              = newKey'' keyhigh keys

instance Pool DoodlePool where
    freshKey = newKey . Map.keys . unwrap
    set k v  = wrap . Map.insert k v . unwrap
    get k    = Map.lookup k . unwrap


