module Language.Boros.Intrinsics where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Utils
import Language.Boros.Syntax
import Language.Boros.Val
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable, typeRep, Proxy (Proxy))
import Data.Function (fix)
import Text.Read (readMaybe)
import Data.Word (Word8)
import Data.Functor (($>))

import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

class ToVal a where
  toVal :: a -> Val

class OfVal a where
  ofVal :: Val -> Maybe a

instance ToVal Val where
  toVal = id

instance OfVal Val where
  ofVal = Just

instance ToVal () where
  toVal _ = Unit

instance OfVal () where
  ofVal Unit = Just ()
  ofVal _ = Nothing

instance ToVal Integer where
  toVal = Num

instance OfVal Integer where
  ofVal (Num i) = Just i
  ofVal _ = Nothing

instance ToVal Int where
  toVal = Num . toInteger

instance OfVal Int where
  ofVal (Num i) = Just $ fromInteger i
  ofVal _ = Nothing

instance ToVal Word8 where
  toVal = Num . toInteger

instance OfVal Word8 where
  ofVal (Num i) = Just $ fromInteger i
  ofVal _ = Nothing

instance ToVal Bool where
  toVal = Bool

instance OfVal Bool where
  ofVal (Bool b) = Just b
  ofVal _ = Nothing

instance ToVal Char where
  toVal = Char

instance OfVal Char where
  ofVal (Char c) = Just c
  ofVal _ = Nothing

instance ToVal Text where
  toVal = Str

instance OfVal Text where
  ofVal (Str s) = Just s
  ofVal _ = Nothing

instance ToVal a => ToVal [a] where
  toVal l = unsafePerformIO $ List <$> newIORef (toVal <$> l)

instance OfVal a => OfVal [a] where
  ofVal (List l) = unsafePerformIO $ traverse ofVal <$> readIORef l
  ofVal _ = Nothing

instance (Typeable a, OfVal a, ToVal b) => ToVal (a -> b) where
  toVal f = Fn \v ->
    pure case ofVal v of
      Nothing -> Left $ "Expected something that looks like '" <> showT (typeRep $ Proxy @a) <> "', but found " <> showT v
      Just a -> pure $ toVal $ f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> IO b) where
  toVal f = Fn \v ->
    case ofVal v of
      Nothing -> pure $ Left $ "Expected something that looks like '" <> showT (typeRep $ Proxy @a) <> "', but found " <> showT v
      Just a -> pure . toVal <$> f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> Either EvalError b) where
  toVal f = Fn \v ->
    pure case ofVal v of
      Nothing -> Left $ "Expected something that looks like '" <> showT (typeRep $ Proxy @a) <> "', but found " <> showT v
      Just a -> toVal <$> f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> IO (Either EvalError b)) where
  toVal f = Fn \v ->
    case ofVal v of
      Nothing -> pure $ Left $ "Expected something that looks like '" <> showT (typeRep $ Proxy @a) <> "', but found " <> showT v
      Just a -> (toVal <$>) <$> f a

instance OfVal (Val -> IO (Either EvalError Val)) where
  ofVal (Fn f) = Just f
  ofVal _ = Nothing

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
  case l of
    [] -> pure $ Left "Empty list in `pop`"
    (a : as) -> writeIORef l' as $> Right a
pop _ = pure $ Left "Non-list in `pop`"

length' :: Val -> IO (Either EvalError Int)
length' (Str s) = pure $ pure $ T.length s
length' (List l) = pure . length <$> readIORef l
length' _ = pure $ Left "Can only take the length of lists and strings"

reverse' :: Val -> IO (Either EvalError Val)
reverse' (Str s) = pure $ pure $ Str $ T.reverse s
reverse' (List l) = pure . List <$> (newIORef . reverse =<< readIORef l)
reverse' _ = pure $ Left "Can only reverse lists and strings"

contains' :: Val -> Val -> IO (Either EvalError Bool)
contains' (Char c) (Str s) = pure $ pure $ T.elem c s
contains' v (List l) = pure . elem v <$> readIORef l
contains' _ _ = pure $ Left "Invalid values in `reverse`"

throw :: Val -> Either EvalError Val
throw v = Left $ "User-thrown exception: " <> showT v

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
  , ("explode", toVal T.unpack)
  , ("implode", toVal T.pack)
  , ("pop", toVal pop)
  , ("reverse", toVal reverse')
  , ("contains", toVal contains')
  ]
