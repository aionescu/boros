module Language.Boros.Intrinsics(ToVal(..), OfVal(..), intrinsics) where

import Control.Monad.Except(ExceptT(ExceptT), runExceptT)
import Data.Foldable(traverse_)
import Data.Function(fix)
import Data.Functor(($>))
import Data.IORef(newIORef, readIORef, writeIORef)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Vector qualified as V
import Data.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)
import Text.Read(readMaybe)

import Utils
import Language.Boros.Syntax
import Language.Boros.Val
import Language.Boros.FFI

unErr :: (Val -> IO (Either EvalError Val)) -> Val -> Val
unErr f v = case unsafePerformIO $ f v of
  Left e -> error $ "Runtime error: " ++ T.unpack e
  Right val -> val

fixVal :: (Val -> IO (Either EvalError Val)) -> Val
fixVal = fix . unErr

halt :: () -> Either EvalError Val
halt _ = Left "Halt"

plus :: Val -> Val -> IO (Either EvalError Val)
plus (Num a) (Num b) = pure $ pure $ Num $ a + b
plus (Str a) (Str b) = pure $ pure $ Str $ a <> b
plus (List a) (List b) = pure . List <$> (newIORef =<< (<>) <$> readIORef a <*> readIORef b)
plus Num{} v = pure $ Left $ "Expected RHS of (+) to be of type Num, but found " <> valType v
plus Str{} v = pure $ Left $ "Expected RHS of (+) to be of type Str, but found " <> valType v
plus List{} v = pure $ Left $ "Expected RHS of (+) to be of type List, but found " <> valType v
plus _ _ = pure $ Left "Invalid values in (+)"

quot' :: Integer -> Integer -> Either EvalError Integer
quot' _ 0 = Left "Division by zero"
quot' a b = Right $ a `quot` b

read' :: Text -> Either EvalError Val
read' s =
  case readMaybe $ T.unpack s of
    Nothing -> Left "Ill-formatted string in `read`"
    Just v -> pure v

getLine' :: () -> IO Text
getLine' _ = T.IO.getLine

getChar' :: () -> IO Word8
getChar' _ = fromIntegral . fromEnum <$> getChar

putChar' :: Word8 -> IO ()
putChar' = putChar . toEnum . fromIntegral

pop :: Val -> IO (Either EvalError Val)
pop (List l') = do
  l <- readIORef l'
  case V.uncons l of
    Nothing -> pure $ Left "Empty list in `pop`"
    Just (a, as) -> writeIORef l' as $> Right a
pop _ = pure $ Left "Non-list in `pop`"

length' :: Val -> IO (Either EvalError Int)
length' (Str s) = pure $ pure $ T.length s
length' (List l) = pure . length <$> readIORef l
length' _ = pure $ Left "Can only take the length of lists and strings"

reverse' :: Val -> IO (Either EvalError Val)
reverse' (Str s) = pure $ pure $ Str $ T.reverse s
reverse' (List l) = pure . List <$> (newIORef . V.reverse =<< readIORef l)
reverse' _ = pure $ Left "Can only reverse lists and strings"

contains' :: Val -> Val -> IO (Either EvalError Bool)
contains' (Char c) (Str s) = pure $ pure $ T.elem c s
contains' v (List l) = pure . V.elem v <$> readIORef l
contains' _ _ = pure $ Left "Invalid values in `reverse`"

throw :: Val -> Either EvalError Val
throw v = Left $ "User-thrown exception: " <> showT v

iter :: (Val -> IO (Either EvalError Val)) -> Val -> IO (Either EvalError ())
iter f (List l) = (runExceptT . V.mapM_ (ExceptT . f)) =<< readIORef l
iter f (Str s) = runExceptT $ traverse_ (ExceptT . f) $ Char <$> T.unpack s
iter _ _ = pure $ Left "Can only `iter` over lists and strings"

intrinsics :: Map Ident Val
intrinsics =
  M.fromList
  [ ("halt", toVal halt)
  , ("throw", toVal throw)
  , ("fix", toVal fixVal)

  , ("print", toVal $ print @Val)
  , ("putStr", toVal T.IO.putStr)
  , ("putStrLn", toVal T.IO.putStrLn)
  , ("getLine", toVal getLine')

  , ("getChar", toVal getChar')
  , ("putChar", toVal putChar')

  , ("readFile", toVal $ T.IO.readFile . T.unpack)

  , ("not", toVal $ not . truthy)

  , ("+", toVal plus)
  , ("-", toVal $ (-) @Integer)
  , ("*", toVal $ (*) @Integer)
  , ("/", toVal quot')
  , ("%", toVal $ rem @Integer)
  , ("^", toVal $ (^) @Integer @Integer)

  , ("truthy", toVal truthy)
  , ("type", toVal valType)

  , ("$", toVal \(f :: Val -> IO (Either EvalError Val)) a -> f a)
  , ("|>", toVal \a (f :: Val -> IO (Either EvalError Val)) -> f a)

  , ("==", toVal $ (==) @Val)
  , ("!=", toVal $ (/=) @Val)
  , ("==#", toVal physEqVal)
  , ("!=#", toVal $ not ... physEqVal)

  , ("compare", toVal $ (fromEnum <$>) ... compareVal)
  , ("<", toVal $ ((== LT) <$>) ... compareVal)
  , (">", toVal $ ((== GT) <$>) ... compareVal)
  , ("<=", toVal $ ((/= GT) <$>) ... compareVal)
  , (">=", toVal $ ((/= LT) <$>) ... compareVal)

  , ("show", toVal $ showT @Val)
  , ("read", toVal read')

  , ("length", toVal length')
  , ("explode", toVal $ V.fromList . T.unpack)
  , ("implode", toVal $ T.pack . V.toList)
  , ("pop", toVal pop)
  , ("reverse", toVal reverse')
  , ("contains", toVal contains')
  , ("replicate", toVal $ V.replicate @Val)

  , ("iter", toVal iter)
  , ("range", toVal $ V.enumFromN @Integer 0)
  ]
