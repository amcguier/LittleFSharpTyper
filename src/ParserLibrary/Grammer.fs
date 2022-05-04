namespace ParserLibrary

module Grammer =

    type Types =
    | ATOM
    | PAIR of Types * Types
    | NAT
    | CLAIM of Types
    | DEFINITION of Types
    
    type AST =
        | ATOM of string
        | CONS of AST * AST
        | CAR of AST
        | CDR of AST
        | ZERO
        | ADD1 of AST
        | DIGITS of int64
        | CLAIM_EXP of string * Types
        | DEFINE_EXP of string * AST
        | PLUS of AST * AST

    type TypedAST =
        AST * Types

    type Progn = AST list
