module Language.Boros.Preprocess where

import Text.Parsec hiding (parse)

import Utils
import Data.Text (Text)
import qualified Data.Text as T

type Code = Text
type Comment = Text

comment :: Parser Comment
comment = try $ T.pack <$> manyTill anyChar (try $ string "-}")

preComment :: Parser Code
preComment = try $ T.pack <$>  manyTill anyChar (try $ string "{-")

postComment :: Parser Code
postComment = try $ T.pack <$> manyTill anyChar eof

comments :: Parser [Comment]
comments = many (preComment *> comment) <* postComment

codeBlocks :: Parser [Code]
codeBlocks = (\l a -> l ++ [a]) <$> many (preComment <* comment) <*> postComment

appendComm :: Text -> Text -> Text
appendComm comm code = "{-" <> comm <> "-}" <> code

applyComms' :: (Text -> Text -> Text) -> [Comment] -> [Code] -> Code
applyComms' append comms (c : cs) = T.concat $ c : zipWith append comms cs
applyComms' _ _ _ = error "applyComments"

applyComments :: [Comment] -> [Code] -> Code
applyComments = applyComms' appendComm

inlineComments :: [Comment] -> [Code] -> Code
inlineComments = applyComms' (<>)
