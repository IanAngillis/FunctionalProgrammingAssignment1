import Doodle
import Data.Time.LocalTime
import Data.Set

-- Type synonyms for existing types
type Title = String 
type Name = String 
type Participants = [Name]

-- New Data types
-- Mydoodle t, t = timezone, takes a string
data Timeslot t = Timeslot t t Participants deriving (Show, Ord)
data MyDoodle t = MyDoodle Title [Timeslot t] deriving (Show)

instance Ord t => Eq (Timeslot t) where
    (Timeslot t1 t2 _) == (Timeslot t3 t4 _) = (t2 > t3 && t1 <= t3) || (t4 > t1 && t3 <= t1)
-- Class instances
instance Doodle MyDoodle where 
    -- Create an empty doodle with a given title
    initialize title = MyDoodle title []
    -- add a timeslot to the doodle
    add (start_time, end_time) (MyDoodle title timeslots) = MyDoodle title (Timeslot start_time end_time [] : timeslots)
    -- Remove a timeslot from the doodle
    remove idx (MyDoodle title timeslots) = MyDoodle title $ removeIndex idx timeslots
    -- toggle a name to the timeslot (add/remove)
    toggle name idx (MyDoodle title timeslots) = MyDoodle title timeslots

-- Hulp functions
removeIndex::Int -> [a] -> [a]
removeIndex _ [] = []
removeIndex 0 (x:xs) = xs
removeIndex i (x:xs) = x : removeIndex (i-1) xs

getAt::Int->[a]->a
getAt idx [] = error "out of bounds"
getAt 0 (x:xs) = x
getAt idx (x:xs) = getAt (idx-1) xs

toggleName::Name->[Name]->[Name]
toggleName name [] = [name] -- Element NOT in list, toggle on
toggleName name (x:xs)
    | name == x     = xs    -- Element IN list, toggle off
    | otherwise     = x : toggleName name xs 