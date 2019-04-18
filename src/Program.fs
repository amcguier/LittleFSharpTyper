// Learn more about F# at http://fsharp.org

open System
open FSharp.Text.Lexing


let testParser str =
  let lexbuf = LexBuffer<char>.FromString str
  try
    Parser.start Lexer.tokenStream lexbuf
  with
    | Lexer.SyntaxError(str)  -> printfn "%A" str
                                 None
  

[<EntryPoint>]
let main argv =
  let valid = testParser "`valid-toβ" |> Option.get
  printfn "%s" valid
  printfn "%A" (valid.Length)
  printfn "%A" (testParser "`valid-tokenβ")
  
  printfn "%A" (testParser "nonsense")
  0 // return an integer exit code
