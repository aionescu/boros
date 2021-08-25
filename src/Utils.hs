module Utils where

import Control.Monad.Except(MonadError, liftEither)
import Data.Bifunctor(first)
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Text(Text)
import Data.Text qualified as T
import GHC.Exts(Int#)
import System.IO.Unsafe(unsafePerformIO)
import Text.Parsec(Parsec, runParser)

showT :: Show a => a -> Text
showT = T.pack . show

type Parser = Parsec Text ()

parse :: MonadError Text m => Parser a -> Text -> m a
parse p = liftEither . first (("Parser error:\n" <>) . showT) . runParser p () ""

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
infixr 9 ...

(!?) :: (Num i, Ord i) => [a] -> i -> Maybe a
(a : _) !? 0 = Just a
(_ : as) !? n | n > 0 = as !? (n - 1)
_ !? _ = Nothing

replaceAt :: (Num i, Ord i) => [a] -> i -> a -> Maybe [a]
replaceAt (_ : as) 0 v = Just $ v : as
replaceAt (a : as) i v | i > 0 = (a :) <$> replaceAt as (i - 1) v
replaceAt _ _ _ = Nothing

boolFromInt# :: Int# -> Bool
boolFromInt# 0# = False
boolFromInt# _ = True

vecOverlaps :: Vector a -> Vector a -> Bool
vecOverlaps a b = unsafePerformIO $ VM.overlaps <$> V.unsafeThaw a <*> V.unsafeThaw b

vecCopy :: Vector a -> Vector a
vecCopy = unsafePerformIO . V.freeze . unsafePerformIO . V.unsafeThaw
