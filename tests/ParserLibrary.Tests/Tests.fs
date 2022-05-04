namespace ParserLibrary.Tests

open Expecto
open ParserLibrary
open FParsec

open Grammer
open Parser

module AtomTests =


    let run = FParsec.CharParsers.run
        

    let expectFailure = function | Failure(_) -> () | x -> failtestf "Expected failure got: %A" x

    let expectSuccess = function | Success _ -> () | x -> failtestf "Expected success got: %A" x
    let expectSuccessV v = function | Success (x,_,_) -> Expect.equal x x "Success values should match" | x -> failtestf "Expected success got: %A" x

    
    [<Tests>]
    let atomTests = testList "Atoms" [
        testCase "missingMark" <|
           fun _ ->
              run Parser.atomParser "hello"
              |> expectFailure

        testCase "regularShouldWork" <| fun _ ->
            run Parser.atomParser "'identifier"
            |> expectSuccessV (ATOM "identifier")

        testCase "digits shouldn't work" <| fun _ ->
            run Parser.atomParser "'1blah1bla"
            |> expectFailure
        testCase "duplicate ' fail" <| fun _ ->
            run Parser.atomParser "''blah" |> expectFailure

        testCase "digit in the middle fails" <| fun _ ->
            run Parser.atomParser "'blah1bleep" |> expectFailure
        testCase "repeats fail" <| fun _ ->
            run Parser.atomParser "'blah'bleep" |> expectFailure
        testCase "Greek and such" <| fun _ ->
            run Parser.atomParser "'ἄτομον" |> expectSuccess
    ]


    [<Tests>]
    let pairTests = testList "Cons" [
      testCase "double atom passes" <| fun _ ->
          run Parser.consParser "(cons 'atom    'atomsagain)"
          |> expectSuccess
      testCase "single answer doesn't work" <| fun _ ->
          run Parser.consParser "(cons 'atom)"
          |> expectFailure
      testCase "nested works" <| fun _ ->
          run Parser.consParser "(cons (cons 'atom 'atomagain) 'topatom)"
          |> expectSuccessV (CONS(CONS(ATOM "atom", ATOM "atomagain"),ATOM "topatom"))
      
    ]


    [<Tests>]
    let cdrTests = testList "Cdr" [
       testCase "basic" <| fun _ ->
           run Parser.cdrParser "(cdr (cons 'atom 'atom))"
           |> expectSuccess

       testCase "singleOnly" <| fun _ ->
           run Parser.cdrParser "(cdr (cons 'atom 'atom) 'atom)"
           |> expectFailure

       testCase "atoms works" <| fun _ ->
           run Parser.cdrParser "(cdr 'atom)"
           |> expectSuccess


    ]

    [<Tests>]
    let carTests = testList "Car" [
       testCase "basic" <| fun _ ->
           run Parser.carParser "(car (cons 'atom 'atom))"
           |> expectSuccess

       testCase "singleOnly" <| fun _ ->
           run Parser.cdrParser "(car (cons 'atom 'atom) 'atom)"
           |> expectFailure

       testCase "atom works" <| fun _ ->
           run Parser.cdrParser "(car 'atom)"
           |> expectFailure
    ]


    [<Tests>]
    let addTests = testList "AddOne" [
        testCase "basic" <| fun _ ->
            run Parser.addOneParser "(add1 ZERO)"
            |> expectSuccessV (ADD1 ZERO)

        testCase "one expression" <| fun _ ->
            run Parser.addOneParser "(add1 zero zero)"
            |> expectFailure
    ]

    [<Tests>]
    let digitTests = testList "Digits" [
        testCase "basic digit" <| fun _ ->
            run Parser.digitParser "1345"
            |> expectSuccessV (DIGITS 1345L)
    ]

    [<Tests>]
    let typeParser = testList "typeParser" [
        testCase "atom" <| fun _ ->
            run TypeParser.typeParser "ATOM"
            |> expectSuccessV (Types.ATOM)

        testCase "nat" <| fun _ ->
            run TypeParser.typeParser "NAT"
            |> expectSuccessV (Types.NAT)
    ]
