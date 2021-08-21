module Val where

import Control.Monad.Except (MonadError)
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef(IORef, readIORef)
import Control.Monad.Reader (MonadReader)
import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (intercalate)

import qualified GHC.Exts as E

import Syntax
import Unsafe.Coerce (unsafeCoerce)
import Data.Void (Void)

data Val
  = UnitVal
  | NumVal Integer
  | BoolVal Bool
  | StrVal String
  | ArrayVal (IORef [Val])
  | RecVal (IORef (Map Ident Val))
  | LamVal Env Ident Expr

type Env = Map Ident Val
type EvalError = String
type MonadEval m = (MonadError EvalError m, MonadReader Env m, MonadIO m)

type SeenVals = [IORef Void]

showArr :: SeenVals -> [Val] -> String
showArr seen a = "[" ++ intercalate ", " (showVal seen <$> a) ++ "]"

showField :: SeenVals -> Ident -> Val -> String
showField seen f v = f ++ " = " ++ showVal seen v

showRec :: SeenVals -> Map Ident Val -> String
showRec seen m
  | M.null m = "{ }"
  | otherwise = "{ " ++ intercalate ", " (uncurry (showField seen) <$> M.toList m) ++ " }"

guardCycle :: SeenVals -> IORef a -> IO String -> String
guardCycle seen r s =
  if unsafeCoerce r `elem` seen
  then "<cycle>"
  else unsafePerformIO s

addSeen :: IORef a -> SeenVals -> SeenVals
addSeen = (:) . unsafeCoerce

showVal :: SeenVals -> Val -> String
showVal _ UnitVal = "()"
showVal _ (NumVal n) = show n
showVal _ (BoolVal b) = toLower <$> show b
showVal _ (StrVal s) = show s
showVal seen (ArrayVal a) = guardCycle seen a $ showArr (addSeen a seen) <$> readIORef a
showVal seen (RecVal r) = guardCycle seen r $ showRec (addSeen r seen) <$> readIORef r
showVal _ LamVal{} = "<λ>"

-- TODO: Don't print cyclical values
instance Show Val where
  show = showVal []

truthy :: MonadEval m => Val -> m Bool
truthy UnitVal = pure True -- Is () truthy?
truthy (NumVal n) = pure $ n /= 0
truthy (BoolVal b) = pure b
truthy (StrVal s) = pure $ not $ null s
truthy (ArrayVal a) = liftIO $ not . null <$> readIORef a
truthy (RecVal r) = liftIO $ not . M.null <$> readIORef r
truthy LamVal{} = pure True -- Lambdas are truthy in Python so...
