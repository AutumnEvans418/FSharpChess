// Learn more about F# at http://fsharp.org

open System
open Fuchu
open NUnit
open NUnit.Framework
open Chess
open Pieces

let ChessTests =
    testList "this is a test" 
            [
                testCase "this is true" <| fun _ -> Assert.AreEqual(0,0)
                testCase "pawn created" <| fun _ -> 
                                               let pawn = pawnStart White 0 0
                                               Assert.AreEqual(White,pawn.Color)
                                               Assert.AreEqual(2, pawn.Attacks.Length)
            ]

[<EntryPoint>]
let main argv =
    let result = run ChessTests
    Console.ReadLine() |> ignore
    result
