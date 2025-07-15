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
import qualified Data.Maybe
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
    | Complete TaskId
    | Delete TaskId
    | Edit TaskId String
    deriving (Show, Generic)

data Task
    = Task
    { taskId :: TaskId
    , description :: String
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
    "complete" : n : _ -> Complete <$> readTaskId n
    "delete" : n : _ -> Delete <$> readTaskId n
    "edit" : n : rest -> Edit <$> readTaskId n <*> pure (unwords rest)
    "new" : rest -> pure $ New $ unwords rest
    _ -> Left $ "Unknown command `" ++ s ++ "` (try `help` command)"

main :: IO ()
main = do
    putStrLn helpText
    fileExists <- doesFileExist taskfile
    unless fileExists $ writeFile taskfile "[]"
    fileContent <- readFile' taskfile
    let tasks = decode $ BL.pack fileContent
    loop $ Data.Maybe.fromMaybe [] tasks

loop :: [Task] -> IO ()
loop tasks = do
    putStr "Enter command: "
    hFlush stdout
    input <- getLine
    case parseCommand input of
        Left err -> do
            putStrLn err
            loop tasks
        Right c -> handleCommand tasks c >>= mapM_ loop

handleCommand :: [Task] -> Command -> IO (Maybe [Task])
handleCommand tasks command = case command of
    Exit -> do
        let encodedTasks = BL.unpack $ encode tasks
        writeFile "tasks.txt" encodedTasks
        return Nothing
    Help -> do
        putStrLn helpText
        return $ Just tasks
    List -> do
        print tasks
        return $ Just tasks
    New s -> do
        let newTaskId = TaskId $ Prelude.foldr (max . getTaskId . taskId) 0 tasks
        let newTask = Task newTaskId s
        return $ Just $ newTask : tasks
    Complete taskid -> return $ Just $ filter (\t -> taskId t /= taskid) tasks
    Delete taskid -> return $ Just $ filter (\t -> taskId t /= taskid) tasks
    Edit taskid s -> do
        let newTasks = map (\t -> if taskId t == taskid then Task taskid s else t) tasks
        return $ Just newTasks
