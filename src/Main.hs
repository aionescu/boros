module Main where

import System.Environment(getArgs)

import Utils(parse)
import Language.Boros.Preprocess(comments, codeBlocks, applyComments, Code, inlineComments)
import Language.Boros.Parser(program)
import Language.Boros.Val
import Language.Boros.Eval
import Control.Monad.Except (ExceptT, MonadIO (liftIO), runExceptT)
import Control.Monad (join)
import System.IO (hSetBuffering, stdin, BufferMode (NoBuffering), stdout)

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

getCode :: IO Text
getCode = do
  (path : _) <- getArgs
  case path of
    "-" -> T.IO.getContents
    _ -> T.IO.readFile path

runOnce :: Args ->  Code -> ExceptT EvalError IO (Maybe Code)
runOnce args input = do
  comms <- parse comments input
  codes <- parse codeBlocks input

  let code = inlineComments comms codes
  expr <- parse program code

  (newComms, val) <- evalWithComments args comms expr
  case val of
    Unit -> pure ()
    _ -> liftIO $ print val

  pure
    if comms == newComms
    then Nothing
    else Just $ applyComments newComms codes

runAll :: Args -> Code -> IO ()
runAll args code = do
  result <- runExceptT $ runOnce args code
  case result of
    Left "Halt" -> pure ()
    Left err -> T.IO.putStrLn err
    Right Nothing -> pure ()
    Right (Just newCode) -> runAll args newCode

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  join $ runAll <$> ((T.pack <$>) . tail <$> getArgs) <*> getCode
