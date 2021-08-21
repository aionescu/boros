module Eval where

import Control.Monad.Except (throwError, ExceptT, runExceptT, liftEither)
import qualified Data.Map as M
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Reader (MonadReader (ask, local), asks, ReaderT, runReaderT)
import Data.Functor (($>))

import Syntax
import Val

(!?) :: (Num i, Ord i) => [a] -> i -> Maybe a
(a : _) !? 0 = Just a
(_ : as) !? n | n > 0 = as !? (n - 1)
_ !? _ = Nothing

replaceAt :: (Num i, Ord i) => [a] -> i -> a -> Maybe [a]
replaceAt (_ : as) 0 v = Just $ v : as
replaceAt (a : as) i v | i > 0 = (a :) <$> replaceAt as i v
replaceAt _ _ _ = Nothing

ref :: MonadIO m => a -> m (IORef a)
ref = liftIO . newIORef

unref :: MonadIO m => IORef a -> m a
unref = liftIO . readIORef

eval' :: MonadEval m => Expr -> m Val
eval' (NumLit n) = pure $ NumVal n
eval' (BoolLit b) = pure $ BoolVal b
eval' (StrLit s) = pure $ StrVal s
eval' UnitLit = pure UnitVal
eval' (ArrayLit es) = ArrayVal <$> (ref =<< traverse eval' es)
eval' (RecLit fs) = RecVal <$> (ref . M.fromList =<< traverse (traverse eval') fs)

eval' (RecMember r f) = do
  record <- eval' r
  case record of
    RecVal m' -> do
      m <- unref m'
      case m M.!? f of
        Just e -> pure e
        Nothing -> throwError $ "Field " ++ show f ++ " does not exist."
    _ -> throwError "Non-record in RecMember."

eval' (Index e idx) = do
  arr <- eval' e
  i <- eval' idx

  case i of
    NumVal n ->
      case arr of
        ArrayVal vs' -> do
          vs <- unref vs'
          case vs !? n of
            Just v -> pure v
            Nothing -> throwError "Index out of range."
        _ -> throwError "Non-array in Index."
    _ -> throwError "Non-numeric array index."

eval' (Var i) = do
  var <- asks (M.!? i)
  case var of
    Just v -> pure v
    Nothing -> throwError $ "Variable " ++ show i ++ " not defined."

eval' (Let bs e) = mdo
  vs <- local (M.union vs) $ traverse eval' $ M.fromList bs
  local (M.union vs) $ eval' e

eval' (Lam i e) = do
  env <- ask
  pure $ LamVal env i e

eval' (App f a) = do
  f' <- eval' f
  a' <- eval' a

  case f' of
    LamVal env i e -> runEval' (M.insert i a' env) (eval' e)
    _ -> throwError "Non-function in App."

eval' (And a b) = do
  a' <- eval' a
  t <- truthy a'

  if not t
  then pure a'
  else eval' b

eval' (Or a b) = do
  a' <- eval' a
  t <- truthy a'

  if t
  then pure a'
  else eval' b

eval' (Assign (Index e i) v) = do
  arr <- eval' e
  i' <- eval' i
  v' <- eval' v

  case arr of
    ArrayVal a' ->
      case i' of
        NumVal n -> do
          a <- liftIO $ readIORef a'
          case replaceAt a n v' of
            Just newA -> liftIO (writeIORef a' newA) $> UnitVal
            Nothing -> throwError "Index out of range."
        _ -> throwError "Non-number index in Assign Index."
    _ -> throwError "Non-array LHS in Assign Index."

eval' (Assign (RecMember e f) v) = do
  r <- eval' e
  v' <- eval' v

  case r of
    RecVal fs' -> do
      fs <- liftIO $ readIORef fs'
      liftIO (writeIORef fs' $ M.insert f v' fs) $> UnitVal
    _ -> throwError "Non-record LHS in Assign Member."

eval' (Assign _ _) = throwError "Non-member expression in Assign. This shouldn't have parsed."

eval' (If c t e) = do
  c' <- truthy =<< eval' c
  eval'
    if c'
    then t
    else e

eval' (Seq a b) = eval' a *> eval' b

runEval :: Env -> ReaderT Env (ExceptT EvalError IO) a -> IO (Either EvalError a)
runEval env m = runExceptT (runReaderT m env)

runEval' :: MonadEval m => Env -> ReaderT Env (ExceptT EvalError IO) a -> m a
runEval' env m = liftEither =<< liftIO (runEval env m)

eval :: Expr -> IO (Either EvalError Val)
eval = runEval M.empty . eval'
