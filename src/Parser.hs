module Parser where

import Data.Bifunctor(first)
import Data.Functor((<&>), ($>))
import Data.List(nub, foldl')
import Control.Monad.Except(liftEither, MonadError)
import Text.Parsec hiding (parse)

import Syntax

type Parser = Parsec String ()

reserved :: [String]
reserved = ["let", "in", "if", "then", "else", "true", "false"]

comma, colon, equals, shebang, ws :: Parser ()
comma = ws <* char ',' <* ws
colon = ws <* char ':' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
ws = spaces *> skipMany (comment *> spaces)

multiLine :: Parser String
multiLine = try $ string "{-" *> manyTill anyChar (try $ string "-}")

singleLine :: Parser String
singleLine = try $ string "--" *> manyTill anyChar (eof <|> endOfLine $> ())

comment :: Parser String
comment = singleLine <|> multiLine

commentExpr :: Parser Expr
commentExpr = CommentExpr <$> comment

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

boolRaw :: Parser Bool
boolRaw = choice [string "true" $> True, string "false" $> False]

strRaw :: Parser String
strRaw = between quote quote $ many ch
  where
    unescape '\\' = '\\'
    unescape '"' = '"'
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape a = a

    escaped = char '\\' *> oneOf "\\\"0nrvtbf" <&> unescape
    regular = noneOf "\\\"\0\n\r\v\t\b\f"
    ch = regular <|> escaped
    quote = char '"'

num :: Parser Expr
num = NumLit <$> intRaw

bool :: Parser Expr
bool = BoolLit <$> boolRaw

str :: Parser Expr
str = StrLit <$> strRaw

simpleLit :: Parser Expr
simpleLit = choice [try str, try num, bool]

ident :: Parser String
ident = notReserved =<< (:) <$> fstChar <*> many sndChar
  where
    fstChar = letter
    sndChar = choice [letter, digit, char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

var :: Parser Expr
var = Var <$> ident

if' :: Parser Expr
if' =
  If
  <$> (string "if" *> ws *> expr <* ws)
  <*> (string "then" *> ws *> expr <* ws)
  <*> (string "else" *> ws *> expr <* ws)

expr :: Parser Expr
expr = undefined
