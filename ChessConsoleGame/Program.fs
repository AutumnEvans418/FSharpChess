// Learn more about F# at http://fsharp.org

open System
open Chess
open Board
[<EntryPoint>]
let main argv =
    let grid = create4by4PawnGame
    let rec playGame grid =
        let game = takeTurnConsole grid
        playGame game
    playGame grid
    Console.ReadLine() |> ignore
    0 // return an integer exit code
