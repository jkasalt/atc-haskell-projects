{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout)
import Text.Read (readEither)

newtype TaskId = TaskId Int
  deriving (Show, Generic)

instance ToJSON TaskId

data Command
  = Help
  | Exit
  | List
  | New String
  | Complete TaskId
  | Delete TaskId
  | Edit TaskId String
  deriving (Show, Generic)

instance ToJSON Command

data Task = Task
  { taskId :: TaskId,
    description :: String
  }

missingNumError :: String -> String
missingNumError command =
  "For the `"
    ++ command
    ++ "` command, you need to specify the id. For example `"
    ++ command
    ++ " 4`. Try the `list` command to see all tasks and their id."

(<?>) :: Either a b -> String -> Either String b
Left _ <?> msg = Left msg
Right a <?> _ = Right a

readTaskId :: String -> Either String TaskId
readTaskId n = TaskId <$> readEither n <?> errMsg
  where
    errMsg = "Failed to parse task id: " ++ n

parseCommand :: String -> Either String Command
parseCommand s = case words s of
  "help" : _ -> pure Help
  "exit" : _ -> pure Exit
  "list" : _ -> pure List
  ["new"] -> Left "Missing description of new command."
  ["complete"] -> Left $ missingNumError "complete"
  ["delete"] -> Left $ missingNumError "complete"
  ["edit"] -> Left $ missingNumError "complete"
  "complete" : n : _ -> Complete <$> readTaskId n
  "delete" : n : _ -> Delete <$> readTaskId n
  "edit" : n : rest -> Edit <$> readTaskId n <*> pure (unwords rest)
  "new" : rest -> pure $ New $ unwords rest
  _ -> Left $ "Unknown command `" ++ s ++ "` (try `help` command)"

helpText :: String
helpText =
  unlines
    [ "help - print the help text",
      "exit - exit the repl",
      "list - list the tasks",
      "new {text} - create a new command with {text} as description",
      "complete {id} - mark task with id {id} as completed",
      "delete {id} - deletes task with id {id}",
      "edit {id} {text} - replaces the description of task with id {id} with {text}"
    ]

main :: IO ()
main = do
  putStrLn helpText
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  let command = parseCommand input
  case command of
    Left err -> do
      putStrLn err
      loop
    Right c -> do
      case c of
        Exit -> putStrLn "goodbye!"
        Help -> putStrLn helpText
        List -> putStrLn "listing"
        New s -> putStrLn $ "creating new " ++ s
        Complete taskid -> putStrLn $ "completing task " ++ show taskid
        Delete taskid -> putStrLn $ "deleting task " ++ show taskid
        Edit taskid s -> putStrLn $ "editing task " ++ show taskid ++ " with " ++ s
      ending c
      where
        ending Exit = return ()
        ending _ = loop
