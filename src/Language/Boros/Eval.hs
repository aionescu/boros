module Language.Boros.Eval(Args, evalWithComments) where

import Control.Monad.Except(ExceptT(ExceptT), throwError, runExceptT, liftEither)
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Reader(MonadReader(ask, local), ReaderT, asks, runReaderT)
import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Text(Text)
import Data.Text qualified as T

import Utils
import Language.Boros.Syntax
import Language.Boros.Preprocess
import Language.Boros.Val
import Language.Boros.Intrinsics

type Env = Map Ident Val
type EvalCtx = ReaderT Env (ExceptT EvalError IO)

type Args = Vector Text

runEval :: Env -> EvalCtx a -> IO (Either EvalError a)
runEval env m = runExceptT (runReaderT m env)

liftCtx :: IO (Either EvalError a) -> EvalCtx a
liftCtx m = liftEither =<< liftIO m

ref :: MonadIO m => a -> m (IORef a)
ref = liftIO . newIORef

unref :: MonadIO m => IORef a -> m a
unref = liftIO . readIORef

eval' :: Expr -> EvalCtx Val
eval' (NumLit n) = pure $ Num n
eval' (BoolLit b) = pure $ Bool b
eval' (CharLit c) = pure $ Char c
eval' (StrLit s) = pure $ Str s
eval' UnitLit = pure Unit
eval' (ListLit es) = List <$> (ref =<< traverse eval' es)
eval' (RecLit fs) = Rec <$> (ref . M.fromList =<< traverse (traverse eval') fs)

eval' (RecMember r f) = do
  record <- eval' r
  case record of
    Rec m' -> do
      m <- unref m'
      case m M.!? f of
        Just e -> pure e
        Nothing -> throwError $ "Inexistent record field  " <> showT f
    _ -> throwError "Tried to access member of a non-record"

eval' (Index e idx) = do
  list <- eval' e
  i <- eval' idx

  case i of
    Num n ->
      case list of
        Str s ->
          let n' = fromInteger n in
            if n' < 0 || n' >= T.length s
            then throwError "String index out of range"
            else pure $ Char $ T.index s n'
        List vs' -> do
          vs <- unref vs'
          case vs V.!? fromInteger n of
            Just v -> pure v
            Nothing -> throwError "List index out of range"
        _ -> throwError "Can only index into lists and strings"
    _ -> throwError "Index is not of type Num"

eval' (Var i) = do
  var <- asks (M.!? i)
  case var of
    Just v -> pure v
    Nothing -> throwError $ "Variable " <> showT i <> " is not defined"

eval' (Let bs e) = do
  let
    isLam Lam{} = True
    isLam _ = False

  if all (isLam . snd) bs
    then mdo
      fs <- local (M.union fs) $ traverse eval' $ M.fromList bs
      local (M.union fs) $ eval' e
    else do
      vs <- traverse eval' $ M.fromList bs
      local (M.union vs) $ eval' e

eval' (Lam i e) = do
  env <- ask
  pure $ Fn \v -> runEval (M.insert i v env) $ eval' e

eval' (App f a) = do
  f' <- eval' f
  a' <- eval' a

  case f' of
    Fn fn -> liftCtx $ fn a'
    _ -> throwError "Tried to apply a non-function"

eval' (And a b) = do
  a' <- eval' a

  if not $ truthy a'
  then pure a'
  else eval' b

eval' (Or a b) = do
  a' <- eval' a

  if truthy a'
  then pure a'
  else eval' b

eval' (Assign (Index e i) v) = do
  list <- eval' e
  i' <- eval' i
  v' <- eval' v

  case list of
    List l' ->
      case i' of
        Num n -> do
          let n' = fromInteger n
          l <- liftIO $ readIORef l'

          if n' < 0 || n' >= V.length l
          then throwError "Tried to assign to out-of-rangee index"
          else
            let newL = V.modify (\mv -> MV.write mv n' v') l
            in liftIO (writeIORef l' newL) $> Unit

        _ -> throwError "Tried to assign to non-numeric index"
    Str _ -> throwError "Strings are immutable"
    _ -> throwError "Tried to assign an index in a non-list"

eval' (Assign (RecMember e f) v) = do
  r <- eval' e
  v' <- eval' v

  case r of
    Rec fs' -> do
      fs <- liftIO $ readIORef fs'
      liftIO (writeIORef fs' $ M.insert f v' fs) $> Unit
    _ -> throwError "Tried to assign a field in a non-record"

eval' (Assign _ _) = throwError "PANIC: Non-member expression in Assign"

eval' (If c t e) = do
  c' <- eval' c
  eval'
    if truthy c'
    then t
    else e

eval' (Seq a b) = eval' a *> eval' b

ofComm :: Val -> Comment
ofComm (Str s) = s
ofComm v = showT v

showEvalError :: EvalError  -> Text
showEvalError "Halt" = "Halt"
showEvalError ee
  | "User-thrown" `T.isPrefixOf` ee = ee
  | otherwise = "Runtime error: " <> ee

evalWithComments :: Args -> Vector Comment -> Expr -> ExceptT EvalError IO (Vector Comment, Val)
evalWithComments args comms expr = do
  commsVal@(List commsRef) <- pure $ toVal comms
  let argsVal = toVal args

  let vars = M.fromList [("args", argsVal), ("comments", commsVal)]

  v <- ExceptT $ first showEvalError <$> runEval (M.union vars intrinsics) (eval' expr)
  newComms :: Vector Comment <- liftIO $ (ofComm <$>) <$> readIORef commsRef
  pure (newComms, v)
