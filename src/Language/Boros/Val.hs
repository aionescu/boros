{-# LANGUAGE StrictData #-}

module Language.Boros.Val(Val(..), EvalError, compareVal, physEqVal, truthy, valType) where

import Data.Bifunctor(second)
import Data.Foldable(toList)
import Data.Functor(($>))
import Data.IORef(IORef, readIORef, newIORef)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Typeable(Typeable)
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Void(Void)
import GHC.Exts(reallyUnsafePtrEquality#)
import System.IO.Unsafe(unsafePerformIO)
import Text.Parsec(getInput, parse, choice, try, eof)
import Unsafe.Coerce(unsafeCoerce)

import Utils hiding (parse)
import Language.Boros.Syntax
import Language.Boros.Parser

data Val
  = Unit
  | Num Integer
  | Bool Bool
  | Char Char
  | Str Text
  | List (IORef (Vector Val))
  | Rec (IORef (Map Ident Val))
  | Fn (Val -> IO (Either EvalError Val))
  deriving stock Typeable

type EvalError = Text

type SeenVals = [IORef Void]

showList' :: SeenVals -> Vector Val -> Text
showList' seen l = "[" <> T.intercalate ", " (showVal seen <$> V.toList l) <> "]"

showField :: SeenVals -> Ident -> Val -> Text
showField seen f v = f <> " = " <> showVal seen v

showRec :: SeenVals -> Map Ident Val -> Text
showRec seen m
  | M.null m = "{ }"
  | otherwise = "{ " <> T.intercalate ", " (uncurry (showField seen) <$> M.toList m) <> " }"

guardCycle :: SeenVals -> IORef a -> IO Text -> Text
guardCycle seen r s =
  if unsafeCoerce r `elem` seen
  then "<∞>"
  else unsafePerformIO s

addSeen :: IORef a -> SeenVals -> SeenVals
addSeen = (:) . unsafeCoerce

escapeComms :: Char -> Text
escapeComms c
  | c `elem` ['{', '-', '}'] = T.pack ['\\', c]
  | otherwise = T.singleton c

showVal :: SeenVals -> Val -> Text
showVal _ Unit = "()"
showVal _ (Num n) = showT n
showVal _ (Bool b) = T.toLower $ showT b
showVal _ (Char c) = T.concatMap escapeComms $ showT c
showVal _ (Str s) = T.concatMap escapeComms $ showT s
showVal seen (List a) = guardCycle seen a $ showList' (addSeen a seen) <$> readIORef a
showVal seen (Rec r) = guardCycle seen r $ showRec (addSeen r seen) <$> readIORef r
showVal _ Fn{} = "<λ>"

instance Show Val where
  show = T.unpack . showVal []

compareVal :: Val -> Val -> Either EvalError Ordering
compareVal Unit Unit = pure EQ
compareVal (Num a) (Num b) = pure $ compare a b
compareVal (Bool a) (Bool b) = pure $ compare a b
compareVal (Char a) (Char b) = pure $ compare a b
compareVal (Str a) (Str b) = pure $ compare a b
compareVal _ _ = Left "Invalid values in comparison"

eqVal :: Val -> Val -> Bool
eqVal Unit Unit = True
eqVal (Num a) (Num b) = a == b
eqVal (Bool a) (Bool b) = a == b
eqVal (Char a) (Char b) = a == b
eqVal (Str a) (Str b) = a == b

eqVal (List a') (List b') = a' == b' || unsafePerformIO do
  a <- readIORef a'
  b <- readIORef b'
  pure $ length a == length b && a == b

eqVal (Rec a') (Rec b') = a' == b' || unsafePerformIO do
  a <- M.toList <$> readIORef a'
  b <- M.toList <$>  readIORef b'

  pure $ a == b

eqVal _ _ = False

instance Eq Val where
  (==) = eqVal

physEqVal :: Val -> Val -> Bool
physEqVal (List a) (List b) = a == b
physEqVal (Rec a) (Rec b) = a == b
physEqVal (Fn a) (Fn b) = boolFromInt# (reallyUnsafePtrEquality# a b)
physEqVal a b = eqVal a b

truthy :: Val -> Bool
truthy Unit = True
truthy (Num n) = n /= 0
truthy (Bool b) = b
truthy (Char c) = fromEnum c /= 0
truthy (Str s) = not $ T.null s
truthy (List a) = unsafePerformIO $ not . null <$> readIORef a
truthy (Rec r) = unsafePerformIO $ not . M.null <$> readIORef r
truthy Fn{} = True

valType :: Val -> Text
valType Unit = "()"
valType Num{} = "Num"
valType Bool{} = "Bool"
valType Char{} = "Char"
valType Str{} = "Str"
valType List{} = "List"
valType Rec{} = "Rec"
valType Fn{} = "Fn"

unitVal :: Parser Val
unitVal = unit $> Unit

numVal :: Parser Val
numVal = Num <$> intRaw

boolVal :: Parser Val
boolVal = Bool <$> boolRaw

charVal :: Parser Val
charVal = Char <$> charRaw

strVal :: Parser Val
strVal = Str <$> strRaw

listVal :: Parser Val
listVal = list (List . unsafePerformIO . newIORef) pVal

recVal :: Parser Val
recVal = rec' (Rec . unsafePerformIO . newIORef . M.fromList) pVal

pVal' :: Parser Val
pVal' = choice $ try <$> [recVal, listVal, strVal, charVal, boolVal, numVal, unitVal]

pVal :: Parser Val
pVal = ws *> pVal' <* ws <* eof

withRest :: Parser a -> Parser (a, Text)
withRest p = (,) <$> p <*> getInput

instance Read Val where
  readsPrec _ = (second T.unpack <$>) . toList . parse (withRest pVal) "" . T.pack
