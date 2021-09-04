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
open Chess.ChessAi
module TestModule =
    


    let validateMoves game move isValid action =
        let moves = parseMoves move
        
        let valid, game = moves |> List.fold (fun (valid, game) (fromPos, toPos) -> 
                                    let fromId = getXY fromPos |> xYToId
                                    let toId = getXY toPos |> xYToId
                                    let result = isValidMoveById (game) fromId toId
                                    let gameMove = moveById (game) fromId toId 
                                    (valid && result, gameMove)
                                    ) (true, game) 

        Assert.AreEqual(isValid, valid, action)
        game

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
            validateMoves initialGame move isValid action |> ignore 
            
        [<TestCase("a1-a3", true, "move rook forward")>]
        [<TestCase("a1-a3,a3-b3", true, "move rook forward and then right")>]
        [<TestCase("a1-b2", false, "move rook diagnal fail")>]
        [<TestCase("b1-a3,a1-a4", false, "move knight in front of rook and try to jump fail")>]
        [<TestCase("a1-b1", false, "move rook ontop of knight should fail")>]
        member _.``rook isvalidmoves`` move isValid action =
            validateMoves noPawnGame move isValid action |> ignore

        [<TestCase("d2-d3,c1-e3,e3-c5,c5-a3,a3-c1", true, "move bishop in diamond")>]
        [<TestCase("b1-d2,c1-e3", false, "move knight in way of bishop should fail")>]
        [<TestCase("d2-d3,c1-h6,h6-c1", true, "move bishop diagnal and back")>]
        [<TestCase("d2-d3,e7-e6,c1-g5,g5-d8", true, "move bishop diagnal and take the queen")>]
        [<TestCase("c1-c4", false, "move bishop forward should fail")>]
        [<TestCase("c1-d2,d2-e1", false, "move bishop on Ally should fail")>]
        member _.``bishop isvalidmoves`` move isValid action =
            validateMoves noPawnGame move isValid action |> ignore
            

        [<TestCase("e2-e3,e1-e2", true, "move pawn and then king 1")>]
        [<TestCase("e2-e4,e1-e3", false, "move pawn and then king 2 should fail")>]
        [<TestCase("g8-f6,f6-g4,e2-e4,e1-e2,e2-e3", false, "move enemy kight and king can't move in danger")>]
        [<TestCase("e2-e3,b7-b6,c8-a6,e1-e2", false, "move enemy bishop and king can't move in danger")>]
        [<TestCase("e2-e4,e1-e2,a7-a5,a8-a6,a6-b6,b6-b3,e2-e3", false, "move enemy rook and king can't move in danger")>]
        [<TestCase("e2-e4,e1-e2,e2-e3,e7-e6,d8-f6,e3-f3", false, "move enemy queen. king can't move in danger")>]
        [<TestCase("e2-e4,e1-e2,d7-d5,e2-e3,e3-d4", true, "king can move in front of enemy pawn")>]
        [<TestCase("e2-e4,e1-e2,d7-d5,e2-e3,e3-d4,d4-c4", false, "king can't move diag of enemy pawn")>]
        [<TestCase("e1-e2", false, "king can't move on ally pawn")>]
        member _.``king isvalidmoves`` move isValid action =
            validateMoves initialGame move isValid action |> ignore 

        [<TestCase("e8-d7", false, "move king in front of queen fail")>]
        [<TestCase("e1-d2", false, "move king in front of queen fail")>]
        [<TestCase("e1-d1", false, "move king on ally queen fail")>]
        [<TestCase("e8-d8", false, "move king on ally queen fail")>]
        [<TestCase("d8-d4,e1-f2", false, "move queen and king in danger should fail")>]
        member _.``king endgame isvalidmoves`` move isValid action =
            validateMoves endGame move isValid action |> ignore

        [<TestCase("d2-d4,d1-d3,f7-f6,d3-g6,e7-e6", false, "Black queen checks white king. pawn doesn't block should be false")>]
        [<TestCase("g8-f6,d2-d3,f6-g4,d3-d4,g4-e3,d4-d5,e3-c2,d5-d6", false, "Black knight checks white king. pawn doesn't block should be false")>]
        [<TestCase("d2-d4,c7-c6,d1-d2,d8-a5,c2-c3", true, "Black queen checks white king. pawn blocks should be valid")>]
        [<TestCase("d2-d4,c7-c6,d1-d2,d8-a5,d2-d3", true, "Black queen checks white king. white king moves should be valid")>]
        member _.``king in check should limit valid moves`` move isValid action =
            validateMoves initialGame move isValid action |> ignore

        [<TestCase("h8-a8", false, "black king should not be able to move")>]
        member _.``endGame king should have no moves`` move isValid action =
            validateMoves endGame2 move isValid action |> ignore

        [<TestCase("a1", "Rook")>]
        [<TestCase("a8", "Rook")>]
        [<TestCase("h1", "Rook")>]
        [<TestCase("h8", "Rook")>]
        [<TestCase("d8", "Queen")>]
        [<TestCase("d1", "Queen")>]
        [<TestCase("a2", "Pawn")>]
        [<TestCase("a3", "None")>]
        member _.``Lookup an initial game piece`` str result =
            lookup initialGame str |> Option.fold (fun _ v -> v.Name) "None" |> should equal result

        [<TestCase("f2-f3,e7-e5,g2-g4,d8-h4","Winner Black")>]
        [<TestCase("f2-f3,e7-e5,g2-g4","Na")>]
        member _.``EndGame result`` move result =
            let g = validateMoves initialGame move true "checked mate the black king"
            gameOver g |> string |> should equal result 

        [<Test>]
        member _.``endgame should be a draw``() =
            gameOver endGame2 |> should equal Tie

        [<Test>]
        member _.``endgame king should have no moves``() =
            getMoves2 endGame2 63 |> Seq.length |> should equal 0

        [<Test>]
        member _.``endgame black should have no moves``() =
            let list = getMovesByColor endGame2 Black 
            list |> List.length |> should equal 1
            let id, moves = list |> List.item 0 
            id |> should equal 63
            moves |> Seq.length |> should equal 0

       
        
        [<Test>]
        member _.``endGame should be 64 grid``() =
            endGame2 |> List.length |> should equal 64

        [<TestCase("a2-a3", "a3", "Pawn")>]
        [<TestCase("a2-a3", "a2", "None")>]
        [<TestCase("a2-a3", "d8", "Queen")>]
        [<TestCase("a2-a3", "h8", "Rook")>]
        member _.MakeAMove str result piece =
            let frm,tto = parseMove str
            let newBoard = moveByXY initialGame frm tto
               
            lookup newBoard result |> Option.fold (fun _ v -> v.Name) "None" |> should equal piece

        [<Test>]
        member _.``game board is 64 squares``() =
            initialGame |> should haveLength 64
        
        [<Test>]
        [<Repeat(5)>]
        member _.``Ai should move``() =
            let move, _ = minimax initialGame 0 10 3 true White
            move |> should equal (12,20)

        [<Test>]
        member _.``All moves should be in range``() =
            let moves = getMovesByColor initialGame White
            for (piece,pMoves) in moves do
                for move in pMoves do
                    let p = initialGame.[piece]
                    printfn "%O-%i" p move
                    inRange move |> should equal true
    end

    