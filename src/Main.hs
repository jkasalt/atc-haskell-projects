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
import Data.Maybe (fromMaybe, isNothing)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hFlush, readFile', stdout)
import Text.Read (readEither)

helpText :: String
helpText =
    unlines
        [ "help - Print the help text"
        , "exit - Exit the repl"
        , "list - List the tasks"
        , "next - List uncompleted tasks (\"next actions\")"
        , "list-done - List completed tasks"
        , "new {text} - Create a new command with {text} as description"
        , "           - you may set a priority by writing \"priority={low|medium|high}\" as the first word"
        , "complete {id} - Mark task with id {id} as completed"
        , "delete {id} - Delete task with id {id}"
        , "edit {id} {text} - Replace the description of task with id {id} with {text}"
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
    | Next
    | Exit
    | List
    | ListDone
    | New (Maybe Priority) String
    | Update TaskId UpdateMode
    deriving (Show)

data UpdateMode = Complete | Delete | Edit String
    deriving (Show)

data Priority = Low | Medium | High
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Priority where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Priority

data Task = Task
    { taskId :: TaskId
    , description :: String
    , completed :: Bool
    , priority :: Maybe Priority
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
    "list-done" : _ -> pure ListDone
    "next" : _ -> pure Next
    ["new"] -> Left "Missing description of new command."
    ["complete"] -> Left $ missingNumError "complete"
    ["delete"] -> Left $ missingNumError "delete"
    ["edit"] -> Left $ missingNumError "edit"
    "complete" : n : _ -> Update <$> readTaskId n <*> pure Complete
    "delete" : n : _ -> Update <$> readTaskId n <*> pure Delete
    "edit" : n : rest -> Update <$> readTaskId n <*> pure (Edit $ unwords rest)
    "new" : prio : rest ->
        let
            p = case prio of
                "priority=low" -> Just Low
                "priority=medium" -> Just Medium
                "priority=high" -> Just High
                _ -> Nothing
            desc = unwords (if isNothing p then prio : rest else rest)
         in
            pure $ New p desc
    _ -> Left $ "Unknown command `" ++ s ++ "` (try `help` command)"

main :: IO ()
main = do
    args <- getArgs
    if "-h" `elem` args || "--help" `elem` args
        then putStrLn helpText
        else initLoop

initLoop :: IO ()
initLoop = do
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
    ListDone -> Print $ show $ filter completed tasks
    Next -> Print $ show $ filter (not . completed) tasks
    New prio desc ->
        let newTaskId = TaskId $ (+ 1) $ Prelude.foldr (max . getTaskId . taskId) 0 tasks
            newTask = Task newTaskId desc False prio
         in Replace $ newTask : tasks
    Update taskid mode ->
        let
            idCheck = findById taskid tasks
            check = case mode of
                Complete -> idCheck >>= notCompleted
                Delete -> idCheck
                Edit _ -> idCheck
            action = case mode of
                Complete -> modifyTask taskid completeTask
                Delete -> filter ((/= taskid) . taskId)
                Edit s -> modifyTask taskid (editTaskDescription s)
         in
            case check of
                Left err -> Print err
                Right _ -> Replace $ action tasks

findById :: TaskId -> [Task] -> Either String Task
findById taskid tasks =
    case find ((== taskid) . taskId) tasks of
        Just t -> Right t
        Nothing -> Left $ "There is no task with id " ++ show (getTaskId taskid)

notCompleted :: Task -> Either String Task
notCompleted task =
    if completed task
        then Left $ "Task " ++ show (getTaskId $ taskId task) ++ " is already completed."
        else Right task

modifyTask :: TaskId -> (Task -> Task) -> [Task] -> [Task]
modifyTask taskid modification = map (\t -> if taskId t == taskid then modification t else t)

editTaskDescription :: String -> Task -> Task
editTaskDescription newDescritption (Task taskid _ compl prio) = Task taskid newDescritption compl prio

completeTask :: Task -> Task
completeTask (Task taskid desc _ prio) = Task taskid desc True prio
