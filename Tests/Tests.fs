namespace Tests
open Chess
open Pieces
open Chess
open Board
open Fuchu
open NUnit.Framework
open System.Linq

module ChessTest =

    let ChessTests =
        let grid = create4by4PawnGame
        let someAssert (option:Option<'a>) action msg =
            match option with
            | Some r -> action r
            | None -> Assert.Fail(msg)
        let piecesHaveOwnSpot grid =
            grid.Pieces |> List.groupBy(fun t -> (t.X,t.Y)) |> List.iter(fun (key,t) -> Assert.AreEqual(1, t.Length))
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
                                            let canMove = canMovePiece grid (Piece grid.Pieces.Head) 2 2
                                            let gridr = movePieceOnGrid grid canMove
                                            someAssert gridr (fun r -> Assert.AreEqual(8,r.Pieces.Length); piecesHaveOwnSpot r) "could not move piece"
                    testCase "Show grid" <| fun _ -> showConsoleGrid grid
                    testCase "All pieces have their own spot" <| fun _ -> piecesHaveOwnSpot grid
                    testCase "move pawn from 1,1 to 1,2 move up and 8 pieces still exist" <| fun _ -> 
                        let action = canMovePiece grid (Pos (1,1)) 1 2
                        let ngrid = movePieceOnGrid grid action 
                        match ngrid with 
                        | Some r -> 
                            Assert.AreEqual(8,r.Pieces.Length) 
                            let get = getPiece r 1 2
                            someAssert get (fun r -> Assert.AreEqual(1,r.X);Assert.AreEqual(2,r.Y)) "there was no piece"
                        | None -> Assert.Fail()
                        ()
                                        
                ]
