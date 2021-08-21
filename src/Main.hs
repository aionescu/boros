module Main where

import System.Environment(getArgs)

import Preprocess(parse)
import Parser(program)
import Eval(eval)

getCode :: IO String
getCode = do
  [path] <- getArgs
  case path of
    "-" -> getContents
    _ -> readFile path

main :: IO ()
main = do
  ast' <- parse program <$> getCode
  case ast' of
    Left err -> putStrLn err
    Right ast -> do
      val' <- eval ast
      case val' of
        Left err -> putStrLn $ "Evaluation error: " ++ err
        Right val -> print val
