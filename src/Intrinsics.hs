module Intrinsics where

import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M

import Syntax
import Val
import Data.IORef (newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable, typeRep, Proxy (Proxy))
import Data.Function (fix)

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

instance ToVal Bool where
  toVal = Bool

instance OfVal Bool where
  ofVal (Bool b) = Just b
  ofVal _ = Nothing

instance {-# OVERLAPPING #-} ToVal String where
  toVal = Str

instance {-# OVERLAPPING #-} OfVal String where
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
      Nothing -> Left $ "Expected something that looks like '" ++ show (typeRep $ Proxy @a) ++ "', but found: " ++ show v ++ "."
      Just a -> pure $ toVal $ f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> IO b) where
  toVal f = Fn \v ->
    case ofVal v of
      Nothing -> pure $ Left $ "Expected something that looks like '" ++ show (typeRep $ Proxy @a) ++ "', but found: " ++ show v ++ "."
      Just a -> pure . toVal <$> f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> Either EvalError b) where
  toVal f = Fn \v ->
    pure case ofVal v of
      Nothing -> Left $ "Expected something that looks like '" ++ show (typeRep $ Proxy @a) ++ "', but found: " ++ show v ++ "."
      Just a -> toVal <$> f a

instance {-# OVERLAPPING #-} (Typeable a, OfVal a, ToVal b) => ToVal (a -> IO (Either EvalError b)) where
  toVal f = Fn \v ->
    case ofVal v of
      Nothing -> pure $ Left $ "Expected something that looks like '" ++ show (typeRep $ Proxy @a) ++ "', but found: " ++ show v ++ "."
      Just a -> (toVal <$>) <$> f a

instance OfVal (Val -> IO (Either EvalError Val)) where
  ofVal (Fn f) = Just f
  ofVal _ = Nothing

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

unErr :: (Val -> IO (Either EvalError Val)) -> Val -> Val
unErr f v = case unsafePerformIO $ f v of
  Left e -> error $ "Runtime error: " ++ e
  Right val -> val

fixVal :: (Val -> IO (Either EvalError Val)) -> Val
fixVal = fix . unErr

halt :: () -> Either EvalError Val
halt _ = Left "Halt"

plus :: Val -> Val -> IO (Either EvalError Val)
plus (Num a) (Num b) = pure $ pure $ Num $ a + b
plus (Str a) (Str b) = pure $ pure $ Str $ a ++ b
plus (List a) (List b) = pure . List <$> (newIORef =<< (++) <$> readIORef a <*> readIORef b)
plus _ _ = pure $ Left ""

intrinsics :: Map Ident Val
intrinsics =
  M.fromList
  [ ("halt", toVal halt)
  , ("fix", toVal fixVal)

  , ("print", toVal $ print @Val)
  , ("not", toVal not)

  , ("+", toVal plus)
  , ("-", toVal $ (-) @Integer)
  , ("*", toVal $ (*) @Integer)
  , ("/", toVal $ quot @Integer)
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
  ]
