// Learn more about F# at http://fsharp.org

open System
open Chess
open Board
[<EntryPoint>]
let main argv =
    let grid = setup4By4PawnGame
    
    playConsoleGame grid |> ignore
    Console.ReadLine() |> ignore
    0 // return an integer exit code
