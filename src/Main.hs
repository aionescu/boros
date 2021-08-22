module Main where

import System.Environment(getArgs)

import Preprocess(comments, codeBlocks, applyComments, Code, inlineComments)
import Parser(program, parse)
import Val
import Eval
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)

getCode :: IO String
getCode = do
  [path] <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

runOnce :: Code -> ExceptT EvalError IO Code
runOnce input = do
  comms <- parse comments input
  codes <- parse codeBlocks input

  let code = inlineComments comms codes
  expr <- parse program code

  (newComms, val) <- evalWithComments comms expr
  case val of
    Unit -> pure ()
    _ -> liftIO $ print val

  pure $ applyComments newComms codes

runAll :: Code -> IO ()
runAll code = do
  result <- runExceptT $ runOnce code
  case result of
    Left "Halt" -> pure ()
    Left err -> putStrLn $ "Runtime error: " ++ err ++ "."
    Right newCode -> runAll newCode

main :: IO ()
main = getCode >>= runAll
