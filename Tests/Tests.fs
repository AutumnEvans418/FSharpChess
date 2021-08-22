namespace Tests
open Chess
open Pieces
open Chess
open NUnit.Framework
open System.Linq
open FsUnit
open System
open ChessParser
open Chess
open Chess
open ChessGrid
open ChessActions

module TestModule =
    


    let validateMoves game move isValid action =
        let moves = parseMoves move
        
        let valid, game = moves |> List.fold (fun (valid, game) (fromPos, toPos) -> 
                                    let fromId = getXY fromPos |> xYToId
                                    let toId = getXY toPos |> xYToId
                                    let result = isValidMoveById (game |> Option.get) fromId toId
                                    let gameMove = moveById (game |> Option.get) fromId toId 
                                    (valid && result, gameMove)
                                    ) (true, Some game) 

        Assert.AreEqual(isValid, valid, action) 

    [<TestFixture>]
    type chessParserTests() = class
        [<TestCase("a1-a2,b1-b3", 2)>]
        [<TestCase("a3-a4", 1)>]
        member _.validparse str moveCount =
            parseMoves str |> should haveLength moveCount

        [<TestCase("a2", "White", "Pawn")>]
        [<TestCase("a7", "Black", "Pawn")>]
        member _.ColorTest position color typeName =
            let piece = lookup initialGame position |> Option.get
            piece.Name |> should equal typeName
            piece.Color |> string |> should equal color            


        


        [<TestCase("a2-a3", true, "White Pawn Forward 1 space")>]
        [<TestCase("a2-a4", true, "White Pawn forward 2")>]
        [<TestCase("a2-a4,a4-a5", true, "white pawn forward 2, then foward 1")>]
        [<TestCase("a7-a5,a5-a6", false, "black pawn forward 2, then back 1")>]
        [<TestCase("a7-a5", true, "black pawn forward 2")>]
        [<TestCase("a7-a6", true, "black pawn forward 1")>]
        [<TestCase("a2-a4,a4-a3", false, "white pawn forward 2, then back 1")>]
        [<TestCase("a2-a3,a3-a5", false, "white pawn forward 1, then foward 2")>]
        [<TestCase("a2-a5", false, "white pawn fwd 2")>]
        [<TestCase("a2-a2", false, "white pawn no move")>]
        [<TestCase("a2-a1", false, "white pawn back 1")>]
        [<TestCase("a2-a4,b7-b5", true, "white pawn move 2. black pawn move 2")>]
        [<TestCase("a2-a4,b7-b5,a4-b5", true, "white pawn move 2. black pawn move 2. white takes black")>]
        [<TestCase("a2-a4,a7-a5,a4-a5", false, "white pawn move 2. black pawn move 2. white takes black invalid")>]
        [<TestCase("b1-a3,a2-a4", false, "knight move. white pawn move 2, but can't jump knight")>]
        [<TestCase("b8-c6,c6-b4,b2-b4", false, "black knight move. white pawn move 2, but can't land on knight")>]
        [<TestCase("b1-b3", false, "white knight move wrong")>]
        [<TestCase("b1-b5", false, "white knight move wrong")>]
        [<TestCase("b1-a3", true, "white knight move left")>]
        [<TestCase("b1-c3", true, "white knight move right")>]
        member _.isValidMoves move isValid action =
            validateMoves initialGame move isValid action   
            
        [<TestCase("a1-a3", true, "move rook forward")>]
        [<TestCase("a1-a3,a3-b3", true, "move rook forward and then right")>]
        [<TestCase("a1-b2", false, "move rook diagnal fail")>]
        [<TestCase("b1-a3,a1-a4", false, "move knight in front of rook and try to jump fail")>]
        [<TestCase("a1-b1", false, "move rook ontop of knight should fail")>]
        member _.``rook isvalidmoves`` move isValid action =
            validateMoves noPawnGame move isValid action  

        [<TestCase("c1-e3,e3-c5,c5-a3,a3-c1", true, "move bishop in diamond")>]
        [<TestCase("b1-d2,c1-e3", false, "move knight in way of bishop should fail")>]
        [<TestCase("c1-h6,h6-c1", true, "move bishop diagnal and back")>]
        [<TestCase("c1-g5,g5-d8", true, "move bishop diagnal and take the queen")>]
        [<TestCase("c1-c4", false, "move bishop forward should fail")>]
        [<TestCase("c1-d2,d2-e1", false, "move bishop on Ally should fail")>]
        member _.``bishop isvalidmoves`` move isValid action =
            validateMoves noPawnGame move isValid action  

        [<TestCase("d2-d3,d1-d2", true, "move pawn and then king 1")>]
        [<TestCase("d2-d4,d1-d3", false, "move pawn and then king 2 should fail")>]
        [<TestCase("b8-c6,c6-b4,d2-d4,d1-d2,d2-d3", false, "move enemy kight and king can't move in danger")>]
        [<TestCase("d2-d4,d1-d2,b7-b6,c8-a6,d2-d3", false, "move enemy bishop and king can't move in danger")>]
        [<TestCase("d2-d4,d1-d2,a7-a5,a8-a6,a6-b6,b6-b3,d2-d3", false, "move enemy rook and king can't move in danger")>]
        [<TestCase("d2-d4,d1-d2,d2-d3,d3-c3,d7-d5,d8-d6,c3-b4", false, "move enemy queen. king can't move in danger")>]
        [<TestCase("d2-d4,d1-d2,c7-c5,d8-b6,d2-c3,c3-c4", true, "king can move in front of enemy pawn")>]
        [<TestCase("d2-d4,c7-c5,d1-d2,d2-d3,d3-c3,c3-c4,c4-b4", false, "king can't move diag of enemy pawn")>]
        member _.``king isvalidmoves`` move isValid action =
            validateMoves initialGame move isValid action  

        [<TestCase("e8-d7", false, "move king in front of queen fail")>]
        [<TestCase("e1-d2", false, "move king in front of queen fail")>]
        [<TestCase("e1-d1", false, "move king on ally queen fail")>]
        [<TestCase("e8-d8", false, "move king on ally queen fail")>]
        [<TestCase("d8-d4,e1-f2", false, "move queen and king in danger should fail")>]
        member _.``king endgame isvalidmoves`` move isValid action =
            validateMoves endGame move isValid action

        [<TestCase("a1", "Rook")>]
        [<TestCase("a8", "Rook")>]
        [<TestCase("h1", "Rook")>]
        [<TestCase("h8", "Rook")>]
        [<TestCase("d8", "Queen")>]
        [<TestCase("e1", "Queen")>]
        [<TestCase("a2", "Pawn")>]
        [<TestCase("a3", "None")>]
        member _.``Lookup an initial game piece`` str result =
            lookup initialGame str |> Option.fold (fun _ v -> v.Name) "None" |> should equal result

        [<TestCase("a2-a3", "a3", "Pawn")>]
        [<TestCase("a2-a3", "a2", "None")>]
        [<TestCase("a2-a3", "d8", "Queen")>]
        [<TestCase("a2-a3", "h8", "Rook")>]
        member _.MakeAMove str result piece =
            let frm,tto = parseMove str
            let newBoard = moveByXY initialGame frm tto |> Option.get
               
            lookup newBoard result |> Option.fold (fun _ v -> v.Name) "None" |> should equal piece

        [<Test>]
        member _.``game board is 64 squares``() =
            initialGame |> should haveLength 64
    end

    