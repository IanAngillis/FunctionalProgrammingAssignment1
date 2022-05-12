import Doodle
import Data.List
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Time (UTCTime)

type Title = String
type Participants = [String]

data Timeslot t = Timeslot t t [String]
data MyDoodle t = MyDoodle Title [Timeslot t]

-- ZonedTime does not have instance for Eq and Ord so we create them ourselves based on the implementation of UTCTime
instance Eq ZonedTime where
    x == y  = zonedTimeToUTC x == zonedTimeToUTC y

instance Ord ZonedTime where
    compare x y = zonedTimeToUTC x `compare` zonedTimeToUTC y
    x <= y  = zonedTimeToUTC x <= zonedTimeToUTC y
    x < y   = zonedTimeToUTC x < zonedTimeToUTC y
    x > y   = zonedTimeToUTC x > zonedTimeToUTC y
    x >= y  = zonedTimeToUTC x >= zonedTimeToUTC y


instance Show t => Show (MyDoodle t) where
    show (MyDoodle title timeslots) = if null timeslots then "+" ++ createDashedLine width ++ "+" ++ "\n| " ++ show title ++ "|\n+" ++ createDashedLine width ++ "+" else "+" ++ createDashedLine width ++ "+" ++ "\n| " ++ show title ++ "|\n+" ++ createDashedLine width ++ "+"  ++ showSlots timeslots
        where
        width = length title + 2
        longestslotLength = longestTimeslotLength timeslots
        slotWidth = longestTimeslotLength timeslots - 4
        showSlots [] = "\n+" ++ createDashedLine slotWidth ++ "+"
        showSlots (x:xs) = 
            let timeslotString = padTimeslotString (show x) longestslotLength
                in 
            "\n" ++ "+" ++ createDashedLine slotWidth ++ "+" ++ timeslotString ++ showSlots xs


instance Show t => Show (Timeslot t) where
    show (Timeslot s e participants) = "\n| " ++ show s ++ " | " ++ show e ++  " | "  ++ showParticipants participants ++ " |"
        where   showParticipants [] = ""
                showParticipants (x:xs) = " " ++ show x ++ showParticipants xs

padTimeslotString :: [Char] -> Int -> [Char]
padTimeslotString str width
    | length str == width = str
    | otherwise = let   diff = width - length str
                        newstr = init str
                        in newstr ++ createSpacedLine diff ++ "|"

longestTimeslotLength::(Show t) => [Timeslot t] -> Int
longestTimeslotLength [] = 0
longestTimeslotLength timeslots = maximum $ map (length . show) timeslots

-- CreateSpacedLine and createDashedLine can become one function that has the same functionality
createSpacedLine :: (Ord t, Num t) => t -> [Char]
createSpacedLine n
    | n <= 0 = ""
    | otherwise = " " ++ createSpacedLine (n-1)

createDashedLine::(Ord t, Num t) => t -> [Char]
createDashedLine n
    | n <= 0     = "-"
    | otherwise = "-" ++ createDashedLine (n-1)

--createCharLine :: [Char] -> Int -> [Char]
--createCharLine char 0 = ""
--createCharLine char n = char ++ createCharLine char (n-1)


instance Doodle MyDoodle where
    initialize title = MyDoodle title []
    add (start, end) doodle@(MyDoodle title participants) = if all (\(Timeslot s2 e2 participants) -> doesNotOverlap s2 e2 start end) participants then  MyDoodle title (Timeslot start end [] : participants) else doodle
    remove idx (MyDoodle title timeslots) = MyDoodle title $ removeIndex idx timeslots
    toggle name idx (MyDoodle title timeslots) = MyDoodle title $ updateTimeslot name toggleParticipant idx timeslots

newtype MyPool k t = MyPool (Map.Map k t)

instance Pool MyPool where
    freshKey    (MyPool pool) = newKey $ Map.keys pool
    set k v     (MyPool pool) = MyPool (Map.insert k v pool)
    get k       (MyPool pool) =  Map.lookup k pool

--- expects an ascending ordered list
newKey:: (Ord k, Enum k) => [k] -> k
newKey keys = if null keys then toEnum 0 else succ $ last keys

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

emptyPool :: MyPool Int (MyDoodle ZonedTime)
emptyPool = MyPool Map.empty

main = run emptyPool


-- some example input
-- main
-- Left "Christmas Party"
-- Just(Right(2022-12-25 16:00:00 +01:00, 2022-12-25 18:00:00 +01:00))
-- Just(Right(2022-12-25 18:00:00 +01:00, 2022-12-25 20:00:00 +01:00))