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

    let idToXY id = 
        let y = id / 8
        let x = id - y * 8
        (x,y)
        
    let between x a b =
        x >= a && x <= b

    let idToPos id =
        let x,y = idToXY id
        let xPos = mapper.First(fun r -> r.Value = x).Key
        sprintf "%c%i" xPos y 

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
    let validatePawn piece (fromId:int) toId =
        
        if Math.Abs(fromId - toId) = 8 then 
            match piece.Color with 
            | White -> fromId < toId
            | Black -> fromId > toId
        else if Math.Abs(fromId - toId) = 16 && piece.HasMoved |> not then true
        else false

    let validateKnight piece (fromId:int) toId =
        true

    let isValidMoveById game fromId toId =
        let piece = game |> List.item fromId
        let toCell = game |> List.item toId
        match piece, toCell with
        | None, _ -> false
        | Some p, None -> 
            match p.Type with
            | Pawn -> validatePawn p fromId toId
            | Knight -> validateKnight p fromId toId
            | _ -> true
        | Some p, Some c -> false

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
