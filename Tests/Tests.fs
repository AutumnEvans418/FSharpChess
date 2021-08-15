namespace Tests
open Chess
open Pieces
open Chess
open Board
open NUnit.Framework
open System.Linq
open FsUnit
open System
module TestModule =
    
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

    

    let initialGame2 =
        let pawns = [for r in 0..7 -> Some Pawn]
        [Some Rook; Some Knight; Some Bishop; Some Queen; Some King; Some Bishop; Some Knight; Some Rook ] 
        |> List.append pawns
        |> List.append [for r in 0..31 -> None]
        |> List.append pawns
        |> List.append [Some Rook; Some Knight; Some Bishop; Some King; Some Queen; Some Bishop; Some Knight; Some Rook ]
    

    
    let initialGame =
        [
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
        let yPos = System.Int32.Parse(position.[1].ToString()) - 1
        (xPos, yPos)

    let xYToId (x,y) =
        y * 8 + x

    let lookupXY game x y =
        game |> List.item (xYToId (x, y))

    let lookup (game:'a option list) (position:string) =
        let xPos, yPos = getXY position
        lookupXY game xPos yPos

    let idToXY id = 
        let y = id / 8
        let x = id - y * 8
        (x,y)
    
    let between x a b =
        x >= a && x <= b

    let isValidMoveById game fromId toId =
        let piece = game |> List.item fromId
        let toCell = game |> List.item toId
        match piece, toCell with
        | None, _ -> false
        | Some p, None -> 
            match p with
            | Pawn -> 
                if Math.Abs(fromId - toId) = 8 then true
                else if Math.Abs(fromId - toId) = 16 then true
                else false
            | _ -> true
        | Some p, Some c -> false

    let moveById game fromId toId =
        if isValidMoveById game fromId toId |> not then game
        else
        let piece = game |> List.item fromId
        
        [for id in 0..63 -> 
            if fromId = id then None
            else if toId = id then piece
            else game.[id]]

    let moveByXY game fromPos toPos = 
        let fromId = getXY fromPos |> xYToId
        let toId = getXY toPos |> xYToId

        moveById game fromId toId



    let convertToGrid list = 
        [for y in 0..7 -> [for x in 0..7 -> lookupXY list x y]]

    [<TestFixture>]
    type chessParserTests() = class
        [<TestCase("a1-a2,b1-b3", 2)>]
        [<TestCase("a3-a4", 1)>]
        member _.validparse str moveCount =
            parseMoves str |> should haveLength moveCount

        [<TestCase("a2-a3", true)>]
        [<TestCase("a2-a4", true)>]
        [<TestCase("a2-a4,a4-a5", true)>]
        [<TestCase("a2-a3,a3-a5", false)>]
        [<TestCase("a2-a5", false)>]
        [<TestCase("a2-a2", false)>]
        [<TestCase("a2-a1", false)>]
        member _.isValidMoves move isValid =
            let moves = parseMoves move
            
            let valid, game = moves |> List.fold (fun (valid, game) (fromPos, toPos) -> 
                                        let fromId = getXY fromPos |> xYToId
                                        let toId = getXY toPos |> xYToId
                                        let result = isValidMoveById game fromId toId
                                        let gameMove = moveById game fromId toId 
                                        (valid && result, gameMove)
                                        ) (true, initialGame2) 

            Assert.AreEqual(valid, isValid, game |> string)      
            
            


        [<TestCase("a1", "Rook")>]
        [<TestCase("a8", "Rook")>]
        [<TestCase("h1", "Rook")>]
        [<TestCase("h8", "Rook")>]
        [<TestCase("d8", "Queen")>]
        [<TestCase("e1", "Queen")>]
        [<TestCase("a2", "Pawn")>]
        [<TestCase("a3", "None")>]
        member _.``Lookup an initial game piece`` str result =
            lookup initialGame2 str |> Option.fold (fun _ v -> v |> string) "None" |> should equal result

        [<TestCase("a2-a3", "a3", "Pawn")>]
        [<TestCase("a2-a3", "a2", "None")>]
        [<TestCase("a2-a3", "d8", "Queen")>]
        [<TestCase("a2-a3", "h8", "Rook")>]
        member _.MakeAMove str result piece =
            let frm,tto = parseMove str
            let newBoard = moveByXY initialGame2 frm tto
               
            lookup newBoard result |> Option.fold (fun _ v -> v |> string) "None" |> should equal piece

        [<Test>]
        member _.``game board is 64 squares``() =
            initialGame2 |> should haveLength 64
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

    