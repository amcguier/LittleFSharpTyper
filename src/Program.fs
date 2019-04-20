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



  




let programs =
  [ "(cons `Atomy (`Atomx))"
    "nonsense"
    "(19)"
    "(zero)"
    "(0)"
    "(+ 0 17)"
    "(add1 zero)"]





[<EntryPoint>]
let main argv =
  programs
  |> List.map (fun x -> (x,testParser x))
  |> List.iter (fun ((prog, ast)) -> printfn "Program:\n\t%s\n Produced:\n\t%A" prog ast)

  0
  
