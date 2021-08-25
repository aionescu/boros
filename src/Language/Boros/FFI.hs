module Language.Boros.FFI where

import Data.IORef(readIORef, newIORef)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Typeable(Proxy(Proxy), Typeable, typeRep)
import Data.Vector(Vector)
import Data.Word(Word8)
import System.IO.Unsafe(unsafePerformIO)

import Utils
import Language.Boros.Val

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

instance ToVal a => ToVal (Vector a) where
  toVal l = unsafePerformIO $ List <$> newIORef (toVal <$> l)

instance OfVal a => OfVal (Vector a) where
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

instance ToVal a => OfVal (a -> IO (Either EvalError Val)) where
  ofVal (Fn f) = Just $ f . toVal
  ofVal _ = Nothing

instance ToVal a => OfVal (a -> IO Val) where
  ofVal (Fn f) = Just $ (either (error . T.unpack) id <$>) . f . toVal
  ofVal _ = Nothing
