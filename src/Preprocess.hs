module Preprocess where

import Text.Parsec hiding (parse)
import Data.Bifunctor (first)

type Code = String
type Comment = String

type Parser = Parsec String ()

comment :: Parser Comment
comment = try $ manyTill anyChar (try $ string "-}")

preComment :: Parser Code
preComment = try $ manyTill anyChar (try $ string "{-")

postComment :: Parser Code
postComment = try $ manyTill anyChar eof

comments :: Parser [Comment]
comments = many (preComment *> comment) <* postComment

codes :: Parser [Code]
codes = (\l a -> l ++ [a]) <$> many (preComment <* comment) <*> postComment

parse :: Parser a -> String -> Either String a
parse p = first show . runParser p () ""

applyComments :: [Comment] -> [Code] -> Code
applyComments comms (c : cs) = concat $ c : zipWith (++) comms cs
applyComments _ _ = error "applyComments"
