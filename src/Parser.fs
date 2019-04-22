// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

  open AST

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | L_PAREN
  | R_PAREN
  | CONS
  | CAR
  | CDR
  | PLUS
  | ADD1
  | NAT of (int)
  | ATOM of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_L_PAREN
    | TOKEN_R_PAREN
    | TOKEN_CONS
    | TOKEN_CAR
    | TOKEN_CDR
    | TOKEN_PLUS
    | TOKEN_ADD1
    | TOKEN_NAT
    | TOKEN_ATOM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_value
    | NONTERM_both
    | NONTERM_stmntbody
    | NONTERM_stmnt
    | NONTERM_prog

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | L_PAREN  -> 1 
  | R_PAREN  -> 2 
  | CONS  -> 3 
  | CAR  -> 4 
  | CDR  -> 5 
  | PLUS  -> 6 
  | ADD1  -> 7 
  | NAT _ -> 8 
  | ATOM _ -> 9 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_L_PAREN 
  | 2 -> TOKEN_R_PAREN 
  | 3 -> TOKEN_CONS 
  | 4 -> TOKEN_CAR 
  | 5 -> TOKEN_CDR 
  | 6 -> TOKEN_PLUS 
  | 7 -> TOKEN_ADD1 
  | 8 -> TOKEN_NAT 
  | 9 -> TOKEN_ATOM 
  | 12 -> TOKEN_end_of_input
  | 10 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_value 
    | 3 -> NONTERM_value 
    | 4 -> NONTERM_both 
    | 5 -> NONTERM_both 
    | 6 -> NONTERM_stmntbody 
    | 7 -> NONTERM_stmntbody 
    | 8 -> NONTERM_stmntbody 
    | 9 -> NONTERM_stmntbody 
    | 10 -> NONTERM_stmntbody 
    | 11 -> NONTERM_stmntbody 
    | 12 -> NONTERM_stmnt 
    | 13 -> NONTERM_prog 
    | 14 -> NONTERM_prog 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 12 
let _fsyacc_tagOfErrorTerminal = 10

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | L_PAREN  -> "L_PAREN" 
  | R_PAREN  -> "R_PAREN" 
  | CONS  -> "CONS" 
  | CAR  -> "CAR" 
  | CDR  -> "CDR" 
  | PLUS  -> "PLUS" 
  | ADD1  -> "ADD1" 
  | NAT _ -> "NAT" 
  | ATOM _ -> "ATOM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | L_PAREN  -> (null : System.Object) 
  | R_PAREN  -> (null : System.Object) 
  | CONS  -> (null : System.Object) 
  | CAR  -> (null : System.Object) 
  | CDR  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | ADD1  -> (null : System.Object) 
  | NAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ATOM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 8us; 65535us; 8us; 6us; 9us; 6us; 11us; 6us; 13us; 6us; 15us; 6us; 16us; 6us; 18us; 6us; 20us; 7us; 7us; 65535us; 8us; 9us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 16us; 17us; 18us; 19us; 1us; 65535us; 20us; 21us; 9us; 65535us; 0us; 23us; 8us; 5us; 9us; 5us; 11us; 5us; 13us; 5us; 15us; 5us; 16us; 5us; 18us; 5us; 23us; 23us; 2us; 65535us; 0us; 2us; 23us; 24us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 12us; 20us; 22us; 32us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 2us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 13us; 1us; 13us; 1us; 14us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; |]
let _fsyacc_action_rows = 26
let _fsyacc_actionTableElements = [|2us; 32768us; 0us; 25us; 1us; 20us; 0us; 49152us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 0us; 16388us; 0us; 16389us; 0us; 16390us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 0us; 16391us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 0us; 16392us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 0us; 16393us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 0us; 16394us; 3us; 32768us; 1us; 20us; 8us; 4us; 9us; 3us; 0us; 16395us; 7us; 32768us; 3us; 8us; 4us; 11us; 5us; 13us; 6us; 15us; 7us; 18us; 8us; 4us; 9us; 3us; 1us; 32768us; 2us; 22us; 0us; 16396us; 2us; 32768us; 0us; 25us; 1us; 20us; 0us; 16397us; 0us; 16398us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 3us; 4us; 5us; 6us; 7us; 8us; 9us; 10us; 14us; 18us; 19us; 23us; 24us; 28us; 29us; 33us; 37us; 38us; 42us; 43us; 51us; 53us; 54us; 57us; 58us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 1us; 1us; 1us; 3us; 2us; 2us; 3us; 2us; 3us; 2us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 6us; 6us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 16386us; 16387us; 16388us; 16389us; 16390us; 65535us; 65535us; 16391us; 65535us; 16392us; 65535us; 16393us; 65535us; 65535us; 16394us; 65535us; 16395us; 65535us; 65535us; 16396us; 65535us; 16397us; 16398us; |]
let _fsyacc_reductions ()  =    [| 
# 139 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AST.Prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 148 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 12 "Parser.fsy"
                                   _1 
                   )
# 12 "Parser.fsy"
                 : AST.Prog));
# 159 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 17 "Parser.fsy"
                                Atom(_1) 
                   )
# 17 "Parser.fsy"
                 : 'value));
# 170 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 18 "Parser.fsy"
                               Natural(_1) 
                   )
# 18 "Parser.fsy"
                 : 'value));
# 181 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'stmnt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 "Parser.fsy"
                                 _1 
                   )
# 21 "Parser.fsy"
                 : 'both));
# 192 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 22 "Parser.fsy"
                                 _1 
                   )
# 22 "Parser.fsy"
                 : 'both));
# 203 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                _1
                   )
# 26 "Parser.fsy"
                 : 'stmntbody));
# 214 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                          Cons(_2, _3) 
                   )
# 27 "Parser.fsy"
                 : 'stmntbody));
# 226 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                     Car(_2) 
                   )
# 28 "Parser.fsy"
                 : 'stmntbody));
# 237 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                     Cdr(_2) 
                   )
# 29 "Parser.fsy"
                 : 'stmntbody));
# 248 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                           Plus( _2, _3) 
                   )
# 30 "Parser.fsy"
                 : 'stmntbody));
# 260 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'both)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                      Add1(_2) 
                   )
# 31 "Parser.fsy"
                 : 'stmntbody));
# 271 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'stmntbody)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                                     _2 
                   )
# 35 "Parser.fsy"
                 : 'stmnt));
# 282 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'stmnt)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                      List.rev (_1::_2) 
                   )
# 40 "Parser.fsy"
                 : 'prog));
# 294 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                               [] 
                   )
# 41 "Parser.fsy"
                 : 'prog));
|]
# 305 "Parser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 13;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : AST.Prog =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
