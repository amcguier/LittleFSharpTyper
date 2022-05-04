namespace ParserLibrary

module TypeChecker =
    open Grammer
    open FsToolkit.ErrorHandling

    type TypeCheckError =
        | Invalid of string
        | Undefined of string

    let rec typecheck: AST -> Result<TypedAST, TypeCheckError> =
        function
        | AST.ATOM s -> Ok(AST.ATOM s, Types.ATOM)
        | CONS (x, y) -> Result.map2 (fun (v1,t1) (v,t2) -> (CONS(x,y), PAIR(t1, t2))) (typecheck y) (typecheck x)
        | CAR at ->
            result {
                let! rs = typecheck at

                match rs with
                | (_,PAIR (t1, _)) -> return (CAR at, t1)
                | (_,t) -> return! Error((Invalid $"Must apply car to a PAIR got {t}"))
            }
        | CDR at ->
            result {
                let! rs = typecheck at

                match rs with
                | _, PAIR (_, t2) -> return CDR at,t2
                | _, t -> return! Error(Invalid $"Must apply car to a PAIR, got {t}")
            }
        | ZERO -> Ok(ZERO, NAT)
        | DIGITS(x) -> Ok(DIGITS x, NAT)
        | CLAIM_EXP(lbl,tp) ->
            match tp with
                | CLAIM _ -> Error(Invalid "Claim cannot be another claim")
                | DEFINITION _ -> Error(Invalid "Claim cannot contain a definition")
                | _ -> Ok(CLAIM_EXP(lbl,tp), CLAIM tp)
        | DEFINE_EXP(s,ast) -> result {
              match! typecheck ast with
              | _,CLAIM _ -> return! Error(Invalid "Can't define a claim")
              | _, DEFINITION _ -> return! Error(Invalid "Can't define another definition")
              | x, t -> return DEFINE_EXP(s,ast), DEFINITION t
          }
        | ADD1 at ->
            result {
                match! typecheck at with
                | _,NAT -> return ADD1 at, NAT
                | _,x -> return! Error(Invalid $"Add requires a NAT, got {x}")
            }
        | x -> Error(Undefined $"Type undefined {x}")
