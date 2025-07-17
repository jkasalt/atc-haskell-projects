{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (unless)
import Data.Aeson (
    FromJSON,
    ToJSON (toEncoding),
    decode,
    defaultOptions,
    encode,
    genericToEncoding,
 )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (hFlush, readFile', stdout)
import Text.Read (readEither)

helpText :: String
helpText =
    unlines
        [ "help - print the help text"
        , "exit - exit the repl"
        , "list - list the tasks"
        , "new {text} - create a new command with {text} as description"
        , "complete {id} - mark task with id {id} as completed"
        , "delete {id} - delete task with id {id}"
        , "edit {id} {text} - replace the description of task with id {id} with {text}"
        ]

taskfile :: FilePath
taskfile = "tasks.txt"

newtype TaskId = TaskId Int
    deriving (Eq, Show, Generic)

getTaskId :: TaskId -> Int
getTaskId (TaskId taskid) = taskid

instance ToJSON TaskId where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TaskId

data Command
    = Help
    | Exit
    | List
    | New String
    | Update TaskId UpdateMode
    deriving (Show)

data UpdateMode = Complete | Delete | Edit String
    deriving (Show)

data Task
    = Task
    { taskId :: TaskId
    , description :: String
    , completed :: Bool
    }
    deriving
        (Show, Generic)

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Task

missingNumError :: String -> String
missingNumError command =
    "For the `"
        ++ command
        ++ "` command, you need to specify the id. For example `"
        ++ command
        ++ " 4`. Try the `list` command to see all tasks and their id."

explainErr :: Either a b -> String -> Either String b
explainErr (Left _) msg = Left msg
explainErr (Right a) _ = Right a

readTaskId :: String -> Either String TaskId
readTaskId n = TaskId <$> explainErr (readEither n) errMsg
  where
    errMsg = "Failed to parse task id: " ++ n

parseCommand :: String -> Either String Command
parseCommand s = case words s of
    "help" : _ -> pure Help
    "exit" : _ -> pure Exit
    "list" : _ -> pure List
    ["new"] -> Left "Missing description of new command."
    ["complete"] -> Left $ missingNumError "complete"
    ["delete"] -> Left $ missingNumError "delete"
    ["edit"] -> Left $ missingNumError "edit"
    "complete" : n : _ -> Update <$> readTaskId n <*> pure Complete
    "delete" : n : _ -> Update <$> readTaskId n <*> pure Delete
    "edit" : n : rest -> Update <$> readTaskId n <*> pure (Edit $ unwords rest)
    "new" : rest -> pure $ New $ unwords rest
    _ -> Left $ "Unknown command `" ++ s ++ "` (try `help` command)"

main :: IO ()
main = do
    putStrLn helpText
    fileExists <- doesFileExist taskfile
    unless fileExists $ writeFile taskfile "[]"
    fileContent <- readFile' taskfile
    let tasks = decode $ BL.pack fileContent
    loop $ fromMaybe [] tasks

loop :: [Task] -> IO ()
loop tasks = do
    putStr "Enter command: "
    hFlush stdout
    input <- getLine
    case parseCommand input of
        Left err -> do
            putStrLn err
            loop tasks
        Right c -> case handleCommand tasks c of
            DoExit -> do
                let encoded = BL.unpack $ encode tasks
                writeFile taskfile encoded
                return ()
            Print s -> do
                putStrLn s
                loop tasks
            Replace t -> loop t

data HandleAction = DoExit | Print String | Replace [Task]

handleCommand :: [Task] -> Command -> HandleAction
handleCommand tasks command = case command of
    Exit -> DoExit
    Help -> Print helpText
    List -> Print $ show tasks
    New s ->
        let newTaskId = TaskId $ (+ 1) $ Prelude.foldr (max . getTaskId . taskId) 0 tasks
            newTask = Task newTaskId s False
         in Replace $ newTask : tasks
    Update taskid mode ->
        let
            idCheck = idExists taskid tasks
            action = case mode of
                Complete -> editTaskList taskid completeTask
                Delete -> filter (\t -> taskId t /= taskid)
                Edit s -> editTaskList taskid (editTaskDescription s)
            checks = case mode of
                Complete -> idCheck >>= notCompleted taskid
                Delete -> idCheck
                Edit _ -> idCheck
         in
            case checks of
                Left err -> Print err
                Right t -> Replace $ action t

idExists :: TaskId -> [Task] -> Either [Char] [Task]
idExists taskid tasks =
    if taskid `elem` map taskId tasks
        then Right tasks
        else Left $ "There is no task with id " ++ show (getTaskId taskid)

notCompleted :: TaskId -> [Task] -> Either String [Task]
notCompleted taskid tasks = case find (\t -> taskId t == taskid) tasks of
    Nothing -> Left $ "There is no task with id " ++ show (getTaskId taskid)
    Just t ->
        if completed t
            then Left $ "Task " ++ show (getTaskId taskid) ++ " is already completed."
            else Right tasks

editTaskList :: TaskId -> (Task -> Task) -> [Task] -> [Task]
editTaskList taskid modification = map (\t -> if taskId t == taskid then modification t else t)

editTaskDescription :: String -> Task -> Task
editTaskDescription newDescritption (Task taskid _ compl) = Task taskid newDescritption compl

completeTask :: Task -> Task
completeTask (Task taskid desc _) = Task taskid desc True
