import Doodle (Doodle)

data DDoodle n t = ADoodle String [(ZonedTime, ZonedTime)]

instance Doodle MyDoodle where 
    initialize s = ADoodle s []
    add = 
    remove = 
    toggle = 

