{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Command (Command (..), UpdateMode (..))
import Control.Monad (unless, void)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Display (Display (display))
import Parsers (parseCommand)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hFlush, readFile', stdout)
import Task (Task (Task, completed, priority, taskId), TaskId (TaskId), completeTask, editTaskDescription, findById, modifyTask, notCompleted)

taskfile :: FilePath
taskfile = "tasks.txt"

helpText :: String
helpText =
    unlines
        [ "help             - Print the help text"
        , "exit             - Exit the repl"
        , "list             - List the tasks"
        , "next             - List uncompleted tasks (\"next actions\")"
        , "list-done        - List completed tasks"
        , "new {text}       - Create a new command with {text} as description"
        , "                   you may set a priority by writing \"priority={low|medium|high}\" as the first word"
        , "complete {id}    - Mark task with id {id} as completed"
        , "delete {id}      - Delete task with id {id}"
        , "edit {id} {text} - Replace the description of task with id {id} with {text}"
        ]

main :: IO ()
main = do
    args <- getArgs
    if "-h" `elem` args || "--help" `elem` args
        then putStrLn helpText
        else do
            tasks <- readTaskfile
            case tasks of
                Just tasks -> void . loop $ tasks
                Nothing -> do
                    error "Failed to read taskfile"

readTaskfile :: IO (Maybe [Task])
readTaskfile = do
    fileExists <- doesFileExist taskfile
    unless fileExists $ writeFile taskfile "[]"
    fileContent <- readFile' taskfile
    return $ decode $ BL.pack fileContent

writeTaskfile :: [Task] -> IO ()
writeTaskfile tasks =
    let
        encoded = BL.unpack $ encode tasks
     in
        writeFile taskfile encoded

loop :: [Task] -> IO [Task]
loop tasks = do
    putStr "Enter command: "
    hFlush stdout
    input <- getLine
    case parseCommand input of
        Left err -> do
            putStrLn $ display err
            loop tasks
        Right c -> case handleCommand tasks c of
            DoExit tasks -> do
                writeTaskfile tasks
                return tasks
            Print s -> do
                putStrLn s
                loop tasks
            Replace t -> loop t

data HandleAction = DoExit [Task] | Print String | Replace [Task]

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = foldr (max . Just) Nothing

handleCommand :: [Task] -> Command -> HandleAction
handleCommand tasks command = case command of
    Exit -> DoExit tasks
    Help -> Print helpText
    List -> printTasksPretty $ sortOn (Down . priority) tasks
    ListDone -> printTasksPretty $ filter completed tasks
    Next -> printTasksPretty $ filter (not . completed) tasks
    New prio desc ->
        let newTaskId = (+ 1) <$> fromMaybe (TaskId 0) (safeMaximum (taskId <$> tasks))
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
  where
    printTasksPretty l = Print $ unlines $ display <$> l
