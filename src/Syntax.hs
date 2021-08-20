module Syntax where

type Ident = String

data Val
  = NumVal Integer
  | BoolVal Bool
  | StrVal String
  | ArrayVal [Val]
  | UnitVal

data Expr
  = NumLit Integer
  | BoolLit Bool
  | StrLit String
  | UnitLit
  | BinOp Expr String Expr
  | FunCall Ident [Expr]
  | Var Ident
  | Let Ident Expr Expr
  | LetFn Ident [Ident] Expr Expr
  | Assign Ident [Expr] Expr
  | If Expr Expr Expr
  | While Expr Expr
  | Seq Expr Expr
  | Index Expr Expr
