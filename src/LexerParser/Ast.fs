module AST

open System


type Type =
  | Nat
  | Atom
  | Pair of Type * Type

type Statement =
  | Zero
  | Atom of string
  | Cons of Statement * Statement
  | Car of Statement
  | Cdr of Statement
  | Plus of Statement * Statement
  | Add1 of Statement
  | Claim of string * Type
  | Define of string * Statement
  

type Prog = Statement list

