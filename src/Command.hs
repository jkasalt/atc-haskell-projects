module Command where

import Task (Priority, TaskId)

data Command
    = Help
    | Next
    | Exit
    | List
    | ListDone
    | New (Maybe Priority) String
    | Update (TaskId Int) UpdateMode
    deriving (Show)

data UpdateMode = Complete | Delete | Edit String
    deriving (Show)
