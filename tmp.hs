import Doodle
import Data.List
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Time (UTCTime)

-- Type synonyms to increase readability
type Title = String
type Participants = [String]

-- New data types
-- t is timezone
data Timeslot t = Timeslot t t [String]
data MyDoodle t = MyDoodle Title [Timeslot t]

instance Eq ZonedTime where
    x == y  = zonedTimeToUTC x == zonedTimeToUTC y

instance Ord ZonedTime where
    compare x y = zonedTimeToUTC x `compare` zonedTimeToUTC y
    x <= y  = zonedTimeToUTC x <= zonedTimeToUTC y
    x < y   = zonedTimeToUTC x < zonedTimeToUTC y
    x > y   = zonedTimeToUTC x > zonedTimeToUTC y
    x >= y  = zonedTimeToUTC x >= zonedTimeToUTC y


instance Show t => Show (MyDoodle t) where
    show (MyDoodle title timeslots) = "------------------------------" ++ "\n" ++ show title ++ "\n" ++ showSlots timeslots ++ "\n"
        where
        showSlots [] = ""
        showSlots (x:xs) = "\n" ++ show x ++ showSlots xs


instance Show t => Show (Timeslot t) where
    show (Timeslot s e participants) = "\n| " ++ show s ++ " | " ++ show e ++  " | "  ++ showParticipants participants ++ " |"
        where   showParticipants [] = ""
                showParticipants (x:xs) = " " ++ show x ++ showParticipants xs

instance Doodle MyDoodle where
    --Create an empty doodle with a given title
    initialize title = MyDoodle title []

    -- Add a timeslot to the doodle 
    add (start, end) doodle@(MyDoodle title participants) =
        if all (\(Timeslot s2 e2 participants) -> doesNotOverlap s2 e2 start end) participants then  MyDoodle title (Timeslot start end [] : participants) else doodle

    -- Remove a timeslot from the doodle
    remove idx (MyDoodle title timeslots) = MyDoodle title $ removeIndex idx timeslots

    -- Toggle a name to a timeslot
    toggle name idx (MyDoodle title timeslots) = MyDoodle title $ updateTimeslot name toggleParticipant idx timeslots



newtype MyPool k t = MyPool (Map.Map k t)



--instance Pool (MyPool k t) where
--    freshKey

-- Functions to access and de-acces the pool
-- Unnessecary??
unwrap::MyPool k t -> Map.Map k t
unwrap (MyPool m) = m

wrap::Map.Map k t -> MyPool k t
wrap  = MyPool

instance Pool MyPool where
    freshKey    (MyPool pool) = newKey $ Map.keys pool
    set k v     (MyPool pool) = MyPool (Map.insert k v pool)
    get k       (MyPool pool) =  Map.lookup k pool

--newKey :: (Enum a1, Ord a1) => Map.Map a1 a2 -> a1
--newKey pool = if null (Map.keys pool) then 0 else succ $ maximum $ Map.keys pool

-- expects an ascending ordered list
-- expects an ascending ordered list
newKey :: (Ord k, Enum k) => [k] -> k
newKey = toEnum . (newKey' 0) . map fromEnum 
  where
    newKey' :: Int -> [Int] -> Int
    newKey' keylow []         = keylow
    newKey' keylow (keyhigh : keys)
         |  keylow /= keyhigh = keylow
         |  otherwise         = newKey' (keylow + 1) keys
         
doesNotOverlap::Ord t =>t -> t -> t -> t -> Bool
doesNotOverlap s1 e1 s2 e2 = (s1 < s2 && e1 <= s2) ||  (s1 >= e2 && e1 > e2)


removeIndex :: (Eq t, Num t) => t -> [a] -> [a]
removeIndex _ [] = []
removeIndex 0 (x:xs) = xs
removeIndex n (x:xs) = x : removeIndex (n-1) xs

updateTimeslot::String -> (String -> [String]->[String]) -> Int -> [Timeslot t] -> [Timeslot t]
updateTimeslot name f _ [] = []
updateTimeslot name f 0 ((Timeslot s e participants) : xs) = Timeslot s e (f name participants) : xs
updateTimeslot name f n (x:xs) = x : updateTimeslot name f (n-1) xs

toggleParticipant::String -> [String] ->  [String]
toggleParticipant participant [] = [participant]
toggleParticipant participant (x : xs)
    | participant == x = xs
    | otherwise = x : toggleParticipant participant xs



-- Testing variables
x = MyDoodle "Christmas Part!" []
x' = add (2, 3) x
x'' = add (1, 2) x'
x''' = add (3, 4) x''
y = remove 0 x'''
y' = toggle "Alice" 0 y
y'' = toggle "Alice" 0 y'

-- Just (Left ("2022-12-25T12:00+01:00", "2022-12-25T17:00+01:00"))
-- Just (Left ("2022-12-24T18:00+01:00", "2022-12-25T00:00+01:00"))

emptyPool :: MyPool Int (MyDoodle ZonedTime)
emptyPool = MyPool Map.empty

emptyIntPool::MyPool Int (MyDoodle Int)
emptyIntPool = MyPool Map.empty

emptyUCTPool :: MyPool Int(MyDoodle ZonedTime)
emptyUCTPool = MyPool Map.empty 

main = run emptyPool


--Just(Right(2022-12-25 16:00:00 +01:00, 2022-12-25 18:00:00 +01:00))

