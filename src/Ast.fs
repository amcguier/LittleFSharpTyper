module AST

open System

type Statement =
  | Atom of string
  | Cons of Statement * Statement
  | Car of Statement
  | Cdr of Statement 
  

type Prog = Statement list

