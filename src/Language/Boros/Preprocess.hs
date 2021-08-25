module Language.Boros.Preprocess where

import Text.Parsec hiding (parse)

import Utils

type Code = String
type Comment = String

comment :: Parser Comment
comment = try $ manyTill anyChar (try $ string "-}")

preComment :: Parser Code
preComment = try $ manyTill anyChar (try $ string "{-")

postComment :: Parser Code
postComment = try $ manyTill anyChar eof

comments :: Parser [Comment]
comments = many (preComment *> comment) <* postComment

codeBlocks :: Parser [Code]
codeBlocks = (\l a -> l ++ [a]) <$> many (preComment <* comment) <*> postComment

appendComm :: String -> String -> String
appendComm comm code = "{-" ++ comm ++ "-}" ++ code

applyComms' :: (String -> String -> String) -> [Comment] -> [Code] -> Code
applyComms' append comms (c : cs) = concat $ c : zipWith append comms cs
applyComms' _ _ _ = error "applyComments"

applyComments :: [Comment] -> [Code] -> Code
applyComments = applyComms' appendComm

inlineComments :: [Comment] -> [Code] -> Code
inlineComments = applyComms' (++)
