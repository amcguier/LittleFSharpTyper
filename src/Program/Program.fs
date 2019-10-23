// Learn more about F# at http://fsharp.org

open System
open FSharp.Text.Lexing



let printLex (lexbuff :  LexBuffer<char>)  =
      printfn "\n"
      while not lexbuff.IsPastEndOfStream  do
        printf "%A " (Lexer.tokenStream lexbuff)
      printfn "\n"
      
let testParser str =
  let lexbuf = LexBuffer<char>.FromString str
  try
    //printLex (LexBuffer<char>.FromString str)
    Parser.start Lexer.tokenStream lexbuf |> Result.Ok
  with
    | Lexer.SyntaxError(s) -> sprintf "Syntax error %s" s |> Result.Error
    | exn  -> //(sprintf "%A %A" (exn.GetType()) exn)
              Lexer.tokenStream lexbuf              
              |> sprintf "Parser Error \n\t%A"
              |> Result.Error




let validPrograms =
  [ "(cons `Atomy (`Atomx))"
    "(`WOot)"
    "(19)"
    "(zero)"
    "(0)"
    "(+ 0 (add1 zero))"
    "(add1 zero)"
    "(`blah)"
    "(claim blah Nat)"
    "(define fft `atoma)"
    "(lambda (x y z) (add1 5)\n(`atomicFire))"]


let invalidPrograms =
  [
      "nonsense"
  ]
 





[<EntryPoint>]
let main argv =
  printfn "Valid programs \n\n"
  validPrograms
  |> List.iter (fun (prog) -> printfn "Program:\n\t%s\n Produced:\n\t%A" prog (testParser prog))

  printfn "\n\nInvalid Programs\n\n"

  invalidPrograms
  |> List.iter (fun (prog) -> printfn "Program:\n\t%s\n Produced:\n\t%A" prog (testParser prog))
  
  0
  
