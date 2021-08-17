namespace Chess
open FSharp.Core
open System.Linq
open System

module Pieces =
    type Color = Black | White
    type PieceType = Pawn  | King   | Queen   | Knight   | Bishop   | Rook  

    type Piece = {Color:Color;HasMoved: bool;Name: string; Type:PieceType}
    
    let pawn color = {Color=color;HasMoved=false; Name="Pawn";Type=Pawn}
    let king color = {Color=color;HasMoved=false; Name="King";Type=King}
    let queen color ={Color=color;HasMoved=false; Name="Queen";Type=Queen}
    let knight color={Color=color;HasMoved=false; Name="Knight";Type=Knight}
    let bishop color = {Color=color;HasMoved=false; Name="Bishop"; Type=Bishop}
    let rook color = {Color=color;HasMoved=false; Name="Rook"; Type=Rook}

        
module ChessParser =
    let mapper = dict['a', 0;'b', 1;'c', 2;'d', 3;'e', 4;'f', 5;'g', 6;'h', 7]

    let parseMove (str:string) =
        let from = str.Split('-').[0]
        let tTo = str.Split('-').[1]
        (from, tTo)

    let parseMoves (str:string) = 
        let moves = [for r in str.Split(',') -> r] 
        moves |> List.map parseMove

    let getXY (position: string) =
        let xPos = mapper.[position.[0]]
        let yPos = System.Int32.Parse(position.[1].ToString()) - 1
        (xPos, yPos)

    let xYToId (x,y) =
        y * 8 + x

    let getRow id =
        id / 8

    let idToXY id = 
        let y = getRow id
        let x = id - y * 8
        (x,y)
        
    let between x a b =
        x >= a && x <= b

    let idToPos id =
        let x,y = idToXY id
        let xPos = mapper.First(fun r -> r.Value = x).Key
        sprintf "%c%i" xPos (y+1) 

module ChessGrid =
    open Pieces
    open ChessParser

    let initialGame2 =
        let pawns color = [for r in 0..7 -> Some (pawn color)]
        [Some (rook Black); Some (knight Black); Some (bishop Black); Some (queen Black); Some (king Black); Some (bishop Black); Some (knight Black); Some (rook Black) ] 
        |> List.append (pawns Black)
        |> List.append [for r in 0..31 -> None]
        |> List.append (pawns White)
        |> List.append [Some (rook White); Some (knight White); Some (bishop White); Some (king White); Some (queen White); Some (bishop White); Some (knight White); Some (rook White) ]

    let lookupXY game x y =
        game |> List.item (xYToId (x, y))

    let lookup (game:'a option list) (position:string) =
        let xPos, yPos = getXY position
        lookupXY game xPos yPos
    
    let convertToGrid list = 
        [for y in 0..7 -> [for x in 0..7 -> lookupXY list x y]]

module ChessActions =
    open Pieces
    open ChessParser

    let index color = 
        match color with
        | White -> 1
        | Black -> -1

    let getPawnMoves game piece toPiece fromId toId =
        let i = index piece.Color
        seq {
            let id = fromId + 8 * i
            let id16 = fromId + 16*i
            if Option.isNone toPiece then yield id;
            if piece.HasMoved |> not && game |> List.item id |> Option.isNone && game |> List.item id16 |> Option.isNone then yield id16;
            if Option.isSome toPiece && (fromId + 9*i = toId || fromId + 7*i=toId) then yield toId }
            

    let validatePawn game piece toPiece (fromId:int) toId =
        let moves = getPawnMoves game piece toPiece fromId toId
        moves |> Seq.contains toId

    let getKnightMoves fromId =
        let row = getRow fromId
        seq {
            if getRow (fromId+15) = row + 2 then yield fromId + 15;
            if getRow (fromId+17) = row + 2 then yield fromId + 17;
            if getRow (fromId-15) = row - 2 then yield fromId - 15;
            if getRow (fromId-17) = row - 2 then yield fromId - 17;
            if getRow (fromId+10) = row + 1 then yield fromId + 10;
            if getRow (fromId-10) = row - 1 then yield fromId - 10;
            if getRow (fromId+6) = row + 1 then yield fromId + 6;
            if getRow (fromId-6) = row - 1 then yield fromId - 6;}

    let validateKnight piece (fromId:int) toId =
        let moves = getKnightMoves fromId
        moves |> Seq.contains toId

    let isValidMoveById game fromId toId =
        let piece = game |> List.item fromId
        let toCell = game |> List.item toId
        match piece, toCell with
        | None, _ -> false
        | Some p, Some c when p.Color = c.Color -> false
        | Some p, c -> 
            match p.Type with
            | Pawn -> validatePawn game p c fromId toId
            | Knight -> validateKnight p fromId toId
            | _ -> true

    let moveById game fromId toId =
        if isValidMoveById game fromId toId |> not then None
        else
        let piece = game |> List.item fromId |> Option.get
        
        Some [for id in 0..63 -> 
                if fromId = id then None
                else if toId = id then Some {piece with HasMoved = true}
                else game.[id]]

    let moveByXY game fromPos toPos = 
        let fromId = getXY fromPos |> xYToId
        let toId = getXY toPos |> xYToId

        moveById game fromId toId
