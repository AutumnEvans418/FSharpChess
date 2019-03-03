namespace Tests
open Chess
open Pieces
open Chess
open Board
open NUnit.Framework
open System.Linq
open FsUnit
module TestModule =
    open NUnit.Framework
    open NUnit.Framework
    open NUnit.Framework
    
    let getPieces grid =
                    grid.Squares  |> List.map(fun r -> r.Piece) |> List.choose id
    let onepGrid = updateGrid create4By4Grid [(1,1,Some (pawnStart White))]
    
    [<TestFixture>]
    type test() = class
         
            [<Test>]
            member self.``moving has the same amount``() =
                create4By4Grid.Squares  |> should be unique
            
           
                getPieces create4By4Grid |> should haveLength 0

                getPieces setup4By4PawnGame |> should haveLength 8

                let move = movePiece setup4By4PawnGame {From=(1,1);To=(1,2)}
                let m = handleFailure move setup4By4PawnGame (fun r-> ())
                getPieces m |> should haveLength 8
            

                ()
            [<Test>]
            member self.``Grid update the grid``() =
                updateGrid setup4By4PawnGame [(1,1,Some (pawnStart White))] |> getPieces |> should haveLength 8
                ()
            [<Test>]
            member self.``Move Pawn from 1,1 to two 1,2``()=

                getPieces onepGrid |> should haveLength 1

                getPiece onepGrid 1 1 |> Option.isSome |> should be True
                let t = movePiece onepGrid {From=(1,1);To=(1,2)}
                let m = handleFailure t onepGrid (fun r -> ())
                
                (getPiece m 1 1).IsSome |> should be False

                getPiece m 1 2 |> Option.isSome |> should be True
            [<Test>]
            member self.``moving pawn 3 spaces is invalid``() =
                let t = movePiece onepGrid {From=(1,1);To=(1,4)}

                let result = handleFailure t onepGrid (fun r -> Assert.Pass(r)) 

                getPiece result 1 1 |> Option.isSome |> should be True

                Assert.Fail("it was valid when it shouldn't be")
                
        end

    