module AST

open System

type Statement =
  | Natural of int
  | Atom of string
  | Cons of Statement * Statement
  | Car of Statement
  | Cdr of Statement
  | Plus of Statement * Statement
  | Add1 of Statement
  

type Prog = Statement list

