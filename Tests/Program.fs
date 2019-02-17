// Learn more about F# at http://fsharp.org

open System
open Fuchu
open Tests

open ChessTest

[<EntryPoint>]
let main argv =
    
    let result = run ChessTest.ChessTests
    Console.ReadLine() |> ignore
    result
