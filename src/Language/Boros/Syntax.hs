{-# LANGUAGE StrictData #-}

module Language.Boros.Syntax where

import Data.Text (Text)

type Ident = Text

data Expr
  = NumLit Integer
  | BoolLit Bool
  | StrLit Text
  | UnitLit
  | ListLit [Expr]
  | RecLit [(Ident, Expr)]
  | RecMember Expr Ident
  | Index Expr Expr
  | Var Ident
  | Let [(Ident, Expr)] Expr
  | Lam Ident Expr
  | App Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Assign Expr Expr
  | If Expr Expr Expr
  | Seq Expr Expr
