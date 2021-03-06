module Language.Boros.Parser(ws, unit, intRaw, boolRaw, charRaw, strRaw, list, rec', program) where

import Data.Foldable(foldl')
import Data.Functor((<&>), ($>))
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V
import Text.Parsec

import Utils
import Language.Boros.Syntax

reserved :: [Text]
reserved = ["let", "and", "in", "if", "then", "else", "true", "false"]

reservedOps :: [Text]
reservedOps = ["=", "->", "<-", ".", "||", "&&"]

comma, equals, shebang, ws :: Parser ()
comma = ws <* char ',' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
ws = spaces

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

unit :: Parser Expr
unit = char '(' *> ws <* char ')' <* ws $> UnitLit

parensExpr :: Parser Expr
parensExpr = try unit <|> parens '(' ')' exprFull

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

boolRaw :: Parser Bool
boolRaw = choice [string "true" $> True, string "false" $> False]

charRaw :: Parser Char
charRaw = between quote quote ch
  where
    unescape '\\' = '\\'
    unescape '\'' = '\''
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape '{' = '{'
    unescape '-' = '-'
    unescape '}' = '}'
    unescape a = a

    escaped = char '\\' *> oneOf "\\'0nrvtbf{-}" <&> unescape
    regular = noneOf "\\'\0\n\r\v\t\b\f"
    ch = regular <|> escaped
    quote = char '\''

strRaw :: Parser Text
strRaw = T.pack <$> between quote quote (many ch)
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
    unescape '{' = '{'
    unescape '-' = '-'
    unescape '}' = '}'
    unescape a = a

    escaped = char '\\' *> oneOf "\\\"0nrvtbf{-}" <&> unescape
    regular = noneOf "\\\"\0\n\r\v\t\b\f"
    ch = regular <|> escaped
    quote = char '"'

numLit :: Parser Expr
numLit = NumLit <$> intRaw

boolLit :: Parser Expr
boolLit = BoolLit <$> boolRaw

charLit :: Parser Expr
charLit = CharLit <$> charRaw

strLit :: Parser Expr
strLit = StrLit <$> strRaw

simpleLit :: Parser Expr
simpleLit = choice [try strLit, try charLit, try numLit, boolLit]

list :: (Vector a -> a) -> Parser a -> Parser a
list mk el = parens '[' ']' $ mk . V.fromList <$> el `sepEndBy` comma

unique :: String -> [(Ident, a)] -> Parser [(Ident, a)]
unique lbl es
  | S.size (S.fromList $ fst <$> es) == length es = pure es
  | otherwise = fail $ lbl ++ " must be unique"

rec' :: ([(Ident, a)] -> a) -> Parser a -> Parser a
rec' mk el = mk <$> parens '{' '}' (unique "Record fields" =<< field `sepEndBy` comma)
  where
    field = (,) <$> (ident <* ws) <*> (equals *> el <* ws)

varIdent :: Parser Text
varIdent = notReserved =<< T.pack ... (:) <$> fstChar <*> many sndChar <* ws
  where
    fstChar = choice [letter, char '_']
    sndChar = choice [letter, digit, char '_', char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

opChar :: Parser Char
opChar = oneOf "~!@#$%^&*-=+\\|:<.>/?"

notReservedOp :: Text -> Parser Text
notReservedOp ((`elem` reservedOps) -> True) = fail "Reserved operator"
notReservedOp i = pure i

opIdent :: Parser Text
opIdent = notReservedOp . T.pack =<< many1 opChar

opIdentWith :: [Char] -> Parser Text
opIdentWith cs = notReservedOp  =<< T.pack ... (:) <$> oneOf cs <*> many opChar

ident :: Parser Ident
ident = try (parens '(' ')' opIdent) <|> varIdent

var :: Parser Expr
var = Var <$> ident

if' :: Parser Expr
if' =
  If
  <$> (string "if" *> ws *> expr <* ws)
  <*> (string "then" *> ws *> exprNoSeq <* ws)
  <*> option UnitLit (string "else" *> ws *> exprNoSeq <* ws)

unrollLam :: [Ident] -> Expr -> Expr
unrollLam as e = foldr Lam e as

lam :: Parser Expr
lam = unrollLam <$> (many1 ident <* string "->" <* ws) <*> exprFull

exprNoMember :: Parser Expr
exprNoMember = choice (try <$> [if', lam, rec' RecLit expr, list ListLit expr, simpleLit, var, parensExpr]) <* ws

member :: Parser Expr -> Parser Expr
member lhs = foldl' unroll <$> lhs <*> many (char '.' *> (Left <$> index <|> Right <$> ident) <* ws)
  where
    index = try $ parens '[' ']' expr
    unroll lhs' (Left idx) = Index lhs' idx
    unroll lhs' (Right ident') = RecMember lhs' ident'

exprMember :: Parser Expr
exprMember = try $ member exprNoMember

exprNoOps :: Parser Expr
exprNoOps = exprMember <|> exprNoMember

exprApp :: Parser Expr
exprApp = chainl1 exprNoOps (ws $> App)

data Assoc = L | R

precedenceTable :: [([Char], Assoc)]
precedenceTable =
  [ ("^", R)
  , ("*/%", L)
  , ("+-&", L)
  , (":@.", R)
  , ("<>", R)
  , ("=!|~", L)
  , ("$@\\?", R)
  ]

chain :: Assoc -> Parser a -> Parser (a -> a -> a) -> Parser a
chain L = chainl1
chain R = chainr1

op :: [Char] -> Parser (Expr -> Expr -> Expr)
op cs = binOp <$> try (opIdentWith cs <* ws)
  where
    binOp :: Text -> Expr -> Expr -> Expr
    binOp op' a = App (App (Var op') a)

exprOps :: Parser Expr
exprOps = foldl' (\p (o, a) -> chain a p $ op o) exprApp precedenceTable

exprAnd :: Parser Expr
exprAnd = chainr1 exprOps $ string "&&" *> ws $> And

exprOr :: Parser Expr
exprOr = chainr1 exprAnd $ string "||" *> ws $> Or

expr :: Parser Expr
expr = exprOr

binding :: Parser (Ident, Expr)
binding = unrollBinding <$> ident <*> many ident <*> (equals *> try exprFull <* ws)
  where
    unrollBinding i is e = (i, unrollLam is e)

bindingGroup :: Parser [(Ident, Expr)]
bindingGroup = unique "Bindings" =<< binding `sepBy1` (string "and" *> ws)

let' :: Parser Expr
let' =
  Let
  <$> (string "let" *> ws *> bindingGroup)
  <*> (string "in" *> ws *> exprFull)

assign :: Parser Expr
assign = Assign <$> (exprMember <* ws <* string "<-" <* ws) <*> exprNoSeq

exprNoSeq :: Parser Expr
exprNoSeq = choice [try let' <|> try assign <|> expr]

exprFull :: Parser Expr
exprFull = option UnitLit exprNoSeq `chainr1` (char ';' *> ws $> Seq)

program :: Parser Expr
program = option () shebang *> ws *> exprFull <* eof
