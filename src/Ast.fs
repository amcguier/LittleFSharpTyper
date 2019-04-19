module AST

open System

type Statement =
  | Atom of string
  | Cons of Statement * Statement
  

type Prog = Statement list

