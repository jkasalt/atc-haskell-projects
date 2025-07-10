module Main where

import System.IO (hFlush, stdout)
import Text.Read (readEither)

newtype TaskId = TaskId Int
  deriving (Show)

data Command = Help | Exit | List | Complete TaskId | Delete TaskId | Edit TaskId String
  deriving (Show)

data Task = Task
  { taskId :: TaskId,
    description :: String
  }

missingNumError :: String -> String
missingNumError command = "For `" ++ command ++ "` you need to specify the id. For example `" ++ command ++ " 4`. Try the `list` command to see all tasks and their id"

(<?>) :: Either a b -> String -> Either String b
Left _ <?> msg = Left msg
Right a <?> _ = Right a

readNum :: (Read a) => String -> Either String a
readNum n = readEither n <?> errMsg
  where
    errMsg = "Failed to parse task id: " ++ n

parseCommand :: String -> Either String Command
parseCommand s = case words s of
  "help" : _ -> Right Help
  "exit" : _ -> Right Exit
  "list" : _ -> Right List
  "complete" : n : _ -> Complete . TaskId <$> readNum n
  "delete" : n : _ -> Delete . TaskId <$> readNum n
  "edit" : n : rest -> (Edit . TaskId <$> readNum n) <*> pure (unwords rest)
  ["complete"] -> Left $ missingNumError "complete"
  ["delete"] -> Left $ missingNumError "complete"
  ["edit"] -> Left $ missingNumError "complete"
  _ -> Left $ "Unknown command `" ++ s ++ "` (try `help` command)"

helpText :: String
helpText =
  unlines
    [ "help - prints the help text",
      "exit - exits the repl",
      "list - lists the tasks",
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
    Right Exit -> do
      putStrLn "goodbye!"
      return ()
    Right c -> do
      print c
      loop
