module Main where

import System.Environment(getArgs)

import Preprocess(parse, comments, codeBlocks, applyComments, Code, inlineComments)
import Parser(program)
import Val
import Eval

getCode :: IO String
getCode = do
  [path] <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

runOnce :: Code -> IO (Either EvalError Code)
runOnce input = do
  case parse comments input of
    Left err -> pure $ Left err
    Right comms ->
      case parse codeBlocks input of
        Left err -> pure $ Left err
        Right codes -> do
          let code = inlineComments comms codes
          case parse program code of
            Left err -> pure $ Left err
            Right expr -> do
              result <- evalWithComments comms expr
              case result of
                Left err -> pure $ Left err
                Right (newComms, val) -> do
                  print val
                  pure $ pure $ applyComments newComms codes

runAll :: Code -> IO ()
runAll code = do
  result <- runOnce code
  case result of
    Left err -> putStrLn err
    Right newCode -> runAll newCode

-- mainRun :: IO ()
-- mainRun = do
--   ast' <- parse program <$> getCode
--   case ast' of
--     Left err -> putStrLn err
--     Right ast -> do
--       val' <- eval ast
--       case val' of
--         Left "Halt" -> putStrLn "Execution explicitly halted."
--         Left err -> putStrLn $ "Evaluation error: " ++ err
--         Right Unit -> pure ()
--         Right val -> print val

main :: IO ()
main = getCode >>= runAll
