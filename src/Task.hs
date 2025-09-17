{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Task where

import Data.Aeson (
    FromJSON,
    ToJSON,
    defaultOptions,
    genericToEncoding,
    toEncoding,
 )
import Data.List (find)
import Display (Display, display, (<+>))
import GHC.Generics (Generic)

newtype TaskId a = TaskId a
    deriving (Eq, Ord, Show, Generic, Functor)

instance (Show a) => Display (TaskId a) where
    display (TaskId n) = show n

instance (ToJSON a) => ToJSON (TaskId a) where
    toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (TaskId a)

data Priority = Low | Medium | High
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Priority

data Task = Task
    { taskId :: TaskId Int
    , description :: String
    , completed :: Bool
    , priority :: Maybe Priority
    }
    deriving
        (Show, Generic)

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Task

instance Display Task where
    display (Task (TaskId taskid) desc compl prio) =
        show taskid <+> complString <+> "-" <+> prioString <> ":" <+> desc
      where
        complString = if compl then "DONE" else "TODO"
        prioString = case prio of
            Nothing -> "___"
            Just Low -> "low"
            Just Medium -> "med"
            Just High -> "hig"

findById :: TaskId Int -> [Task] -> Either String Task
findById taskid tasks =
    case find ((== taskid) . taskId) tasks of
        Just t -> Right t
        Nothing -> Left $ "There is no task with id " ++ display taskid

notCompleted :: Task -> Either String Task
notCompleted task =
    if completed task
        then Left $ "Task " ++ display (taskId task) ++ " is already completed."
        else Right task

modifyTask :: TaskId Int -> (Task -> Task) -> [Task] -> [Task]
modifyTask taskid modification = map (\t -> if taskId t == taskid then modification t else t)

editTaskDescription :: String -> Task -> Task
editTaskDescription newDescritption (Task taskid _ compl prio) = Task taskid newDescritption compl prio

completeTask :: Task -> Task
completeTask (Task taskid desc _ prio) = Task taskid desc True prio
