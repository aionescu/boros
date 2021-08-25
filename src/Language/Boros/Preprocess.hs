module Language.Boros.Preprocess(Code, Comment, comments, codeBlocks, applyComments, inlineComments) where

import Data.Foldable(fold)
import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V
import Text.Parsec hiding (parse)

import Utils

type Code = Text
type Comment = Text

comment :: Parser Comment
comment = try $ T.pack <$> manyTill anyChar (try $ string "-}")

preComment :: Parser Code
preComment = try $ T.pack <$>  manyTill anyChar (try $ string "{-")

postComment :: Parser Code
postComment = try $ T.pack <$> manyTill anyChar eof

comments :: Parser (Vector Comment)
comments = V.fromList <$> many (preComment *> comment) <* postComment

codeBlocks :: Parser (Vector Code)
codeBlocks = V.snoc <$> (V.fromList <$> many (preComment <* comment)) <*> postComment

appendComm :: Text -> Text -> Text
appendComm comm code = "{-" <> comm <> "-}" <> code

applyComms' :: (Text -> Text -> Text) -> Vector Comment -> Vector Code -> Code
applyComms' append comms (V.uncons -> Just (c, cs)) = fold $ V.cons c $ V.zipWith append comms cs
applyComms' _ _ _ = error "applyComments"

applyComments :: Vector Comment -> Vector Code -> Code
applyComments = applyComms' appendComm

inlineComments :: Vector Comment -> Vector Code -> Code
inlineComments = applyComms' (<>)
