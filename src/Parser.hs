module Parser where

import Data.Foldable(foldl')
import Data.Functor((<&>), ($>))
import Data.List(nub)
import Text.Parsec

import Syntax

type Parser = Parsec String ()

reserved :: [String]
reserved = ["let", "in", "if", "then", "else", "true", "false", "and", "or"]

reservedOps :: [String]
reservedOps = ["=", "->", "<-", "."]

comma, colon, equals, shebang, ws :: Parser ()
comma = ws <* char ',' <* ws
colon = ws <* char ':' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
ws = spaces

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

parensExpr :: Parser Expr
parensExpr = try unit <|> parens '(' ')' exprFull
  where
    unit = char '(' *> ws <* char ')' <* ws $> UnitLit

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

arrayLit :: Parser Expr
arrayLit = parens '[' ']' $ ArrayLit <$> expr `sepEndBy` comma

recLit :: Parser Expr
recLit = RecLit <$> parens '{' '}' (unique =<< field `sepEndBy` comma)
  where
    field = (,) <$> (ident <* ws) <*> (equals *> expr <* ws)
    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Fields in a record must be unique"

varIdent :: Parser String
varIdent = notReserved =<< (:) <$> fstChar <*> many sndChar <* ws
  where
    fstChar = choice [letter, char '_']
    sndChar = choice [letter, digit, char '_', char '\'']
    notReserved ((`elem` reserved) -> True) = fail "Reserved identifier"
    notReserved i = pure i

opChar :: Parser Char
opChar = oneOf "~!@#$%^&*-=+\\|:<.>/?"

notReservedOp :: String -> Parser String
notReservedOp ((`elem` reservedOps) -> True) = fail "Reserved operator"
notReservedOp i = pure i

opIdent :: Parser String
opIdent = notReservedOp =<< many1 opChar

opIdentWith :: String -> Parser String
opIdentWith cs = notReservedOp  =<< (:) <$> oneOf cs <*> many opChar

ident :: Parser String
ident = try (parens '(' ')' opIdent) <|> varIdent

var :: Parser Expr
var = Var <$> ident

if' :: Parser Expr
if' =
  If
  <$> (string "if" *> ws *> expr <* ws)
  <*> (string "then" *> ws *> exprNoSeq <* ws)
  <*> (string "else" *> ws *> exprNoSeq <* ws)

unrollLam :: [String] -> Expr -> Expr
unrollLam as e = foldr Lam e as

lam :: Parser Expr
lam = unrollLam <$> (many1 ident <* string "->" <* ws) <*> exprNoSeq

exprNoMember :: Parser Expr
exprNoMember = choice (try <$> [if', lam, recLit, arrayLit, simpleLit, var, parensExpr]) <* ws

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

precedenceTable :: [(String, Assoc)]
precedenceTable =
  [ ("*/%", L)
  , ("+-&", L)
  , (":@^.", R)
  , ("<>", R)
  , ("=!|~", L)
  , ("$@\\?", R)
  ]

chain :: Assoc -> Parser a -> Parser (a -> a -> a) -> Parser a
chain L = chainl1
chain R = chainr1

op :: String -> Parser (Expr -> Expr -> Expr)
op cs = binOp <$> try (opIdentWith cs <* ws)
  where
    binOp :: String -> Expr -> Expr -> Expr
    binOp op' a = App (App (Var op') a)

exprOps :: Parser Expr
exprOps = foldl' (\p (o, a) -> chain a p $ op o) exprApp precedenceTable

opAndOr :: Parser (Expr -> Expr -> Expr)
opAndOr =
  choice
  [ string "and" $> And
  , string "or" $> Or
  ]
  <* ws

exprLogic :: Parser Expr
exprLogic = chainr1 exprOps opAndOr

unrollLet :: Ident -> [Ident] -> Expr -> Expr -> Expr
unrollLet i args v = Let i (unrollLam args v)

let' :: Parser Expr
let' =
  unrollLet
  <$> (string "let" *> ws *> ident)
  <*> many ident
  <*> (equals *> exprNoSeq <* ws)
  <*> (string "in" *> ws *> exprFull)

expr :: Parser Expr
expr = exprLogic

assign :: Parser Expr
assign = Assign <$> (exprMember <* ws <* string "<-" <* ws) <*> exprNoSeq

exprNoSeq :: Parser Expr
exprNoSeq = choice [try let' <|> try assign <|> expr]

exprFull :: Parser Expr
exprFull = exprNoSeq `chainr1` (char ';' *> ws $> Seq)

program :: Parser Expr
program = option () shebang *> ws *> exprFull <* eof
