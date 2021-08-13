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
    
    let parseMove (str:string) =
        let from = str.Split("-").[0]
        let tTo = str.Split("-").[1]
        (from, tTo)

    let parseMoves (str:string) = 
        let moves = [for r in str.Split(",") -> r] 
        moves |> List.map parseMove

    

    let initialGame = 
        [
            [Some Rook; Some Knight; Some Bishop; Some Queen; Some King; Some Bishop; Some Knight; Some Rook ]
            [for r in 0..7 -> Some Pawn]
            [for r in 0..7 -> None]
            [for r in 0..7 -> None]
            [for r in 0..7 -> None]
            [for r in 0..7 -> None]
            [for r in 0..7 -> Some Pawn]
            [Some Rook; Some Knight; Some Bishop; Some King; Some Queen; Some Bishop; Some Knight; Some Rook ]
        ]
    let getXY (position: string) =
        let mapper = dict['a', 0;'b', 1;'c', 2;'d', 3;'e', 4;'f', 5;'g', 6;'h', 7]
        let xPos = mapper.[position.[0]]
        let yPos = 8 - System.Int32.Parse(position.[1].ToString())
        (xPos, yPos)

    let lookup (game:'a option list list) (position:string) =
        let xPos, yPos = getXY position
        game.[yPos].[xPos]

    let move game fromPos toPos = 
        let fromX, fromY = getXY fromPos
        let toX, toY = getXY toPos

        let piece = lookup game fromPos

        [
            for y in 0..7 -> [for x in 0..7 -> if fromX = x && fromY = y then None else if toX = x && toY = y then piece else game.[y].[x] ]
        ]


    [<TestFixture>]
    type chessParserTests() = class
        [<TestCase("a1-a2,b1-b3", 2)>]
        [<TestCase("a3-a4", 1)>]
        member _.validparse str moveCount =
            parseMoves str |> should haveLength moveCount

        [<TestCase("a1", "Rook")>]
        [<TestCase("a8", "Rook")>]
        [<TestCase("h1", "Rook")>]
        [<TestCase("h8", "Rook")>]
        [<TestCase("d8", "Queen")>]
        member _.GetInitialPiece str result =
            lookup initialGame str |> Option.get |> string |> should FsUnit.TopLevelOperators.equal result
    end


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

    