// Learn more about F# at http://fsharp.org

open System
open FSharp.Text.Lexing


let testParser str =
  let lexbuf = LexBuffer<char>.FromString str
  try
    Parser.start Lexer.tokenStream lexbuf |> Some
    
  with
    | exn  -> printfn "%A %A" (exn.GetType()) exn
              None


  

[<EntryPoint>]
let main argv =
  let testProgram =
    """(cons `Atomy (`Atomx))"""
  printfn "%A" (testParser testProgram)
  printfn "%A" (testParser "(`valid-tokenβ)")

  try
    printfn "%A" (testParser "nonsense")
  with
    |  exn -> printfn "%A" exn
  0 // return an integer exit code
