namespace Tests
open Chess
open Pieces
open Chess
open Board
open NUnit.Framework
open System.Linq
open FsUnit

[<TestFixture>]
type test() = class
        [<Test>]
        member self.test() =
            create4By4Grid.Squares  |> should be unique
            
            let getPieces grid =
                grid.Squares  |> List.map(fun r -> r.Piece) |> List.choose id
            getPieces create4By4Grid |> should haveLength 0

            getPieces setup4By4PawnGame |> should haveLength 8

            let move = movePiece setup4By4PawnGame {From=(1,1);To=(1,2)}

            getPieces move |> should haveLength 8
            

            ()
    end

    
    //let ChessTests =
    //    let grid = create4by4PawnGame
    //    let ass ex ac =
    //        Assert.AreEqual(ex,ac)
    //    let someAssert (option:Option<'a>) action msg =
    //        match option with
    //        | Some r -> action r
    //        | None -> Assert.Fail(msg)
    //    let piecesHaveOwnSpot grid =
    //        grid.Pieces |> List.groupBy(fun t -> (t.X,t.Y)) |> List.iter(fun (key,t) -> Assert.AreEqual(1, t.Length))
    //    testList "this is a test" 
    //            [
    //                testCase "replace a piece" <| fun _ -> 
    //                                                let l = [1;2;3]
    //                                                let t = [for r in l do if r = 1 then yield r + 1 else yield r]
    //                                                ass 3 t.Length
    //                                                ass 2 t.Head
    //                                                ass 3 t.[t.Length-1]
    //                                                ()
    //                testCase "replace chess piece" <| fun _ ->
    //                                                    let lt = {grid with Pieces=replacePiece grid grid.Pieces.Head 1 2}
    //                                                    piecesHaveOwnSpot lt
                                                        
    //                testCase "this is true" <| fun _ -> Assert.AreEqual(0,0)
    //                testCase "pawn created" <| fun _ -> 
    //                                               let pawn = pawnStart White 0 0
    //                                               Assert.AreEqual(White,pawn.Color)
    //                                               Assert.AreEqual(2, pawn.Attacks.Length)
    //                testCase "move a piece" <| fun _ ->
    //                                            let pawn = pawnStart White 0 0
    //                                            let m = movePiece pawn 1 0
    //                                            ass 1 m.X
    //                                            ass 0 m.Y
    //                                            ass White m.Color
    //                                            ass Pawn m.Type

    //                testCase "create 4 by 4 grid" <| fun _ ->
    //                                               Assert.AreEqual(8,grid.Pieces.Length)
    //                testCase "move pawn" <| fun _ -> 
    //                                        let canMove = canMovePiece grid (Piece grid.Pieces.Head) 2 2
    //                                        let gridr = movePieceOnGrid grid canMove
    //                                        someAssert gridr (fun r -> Assert.AreEqual(8,r.Pieces.Length); piecesHaveOwnSpot r) "could not move piece"

    //                testCase "Show grid" <| fun _ -> showConsoleGrid grid
    //                testCase "All pieces have their own spot" <| fun _ -> piecesHaveOwnSpot grid
    //                testCase "move pawn from 1,1 to 1,2 move up and 8 pieces still exist" <| fun _ -> 
    //                    let action = canMovePiece grid (Pos (1,1)) 1 2
    //                    let ngrid = movePieceOnGrid grid action 
    //                    match ngrid with 
    //                    | Some r -> 
    //                        Assert.AreEqual(8,r.Pieces.Length) 
    //                        let get = getPiece r 1 2
    //                        someAssert get (fun r -> Assert.AreEqual(1,r.X);Assert.AreEqual(2,r.Y)) "there was no piece"
    //                    | None -> Assert.Fail()
    //                    ()
                                        
    //            ]
