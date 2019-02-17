// Learn more about F# at http://fsharp.org

open System
open Fuchu
open NUnit
open NUnit.Framework
open Chess
open Pieces
open Chess
open Board
let ChessTests =
    let grid = create4by4PawnGame
    
    testList "this is a test" 
            [
                testCase "this is true" <| fun _ -> Assert.AreEqual(0,0)
                testCase "pawn created" <| fun _ -> 
                                               let pawn = pawnStart White 0 0
                                               Assert.AreEqual(White,pawn.Color)
                                               Assert.AreEqual(2, pawn.Attacks.Length)
                testCase "create 4 by 4 grid" <| fun _ ->
                                               Assert.AreEqual(8,grid.Pieces.Length)
                testCase "move pawn" <| fun _ -> 
                                        let canMove = canMovePiece grid.Pieces.Head 2 2
                                        let gridr = movePieceOnGrid grid canMove
                                        match gridr with
                                        | Some r -> Assert.AreEqual(8,r.Pieces.Length)
                                        | None _ -> Assert.Fail()
                testCase "Show grid" <| fun _ -> showConsoleGrid grid
                
                                        
            ]

[<EntryPoint>]
let main argv =
    
    let result = run ChessTests
    Console.ReadLine() |> ignore
    result
