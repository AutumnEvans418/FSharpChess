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

    let initialGame =
        let pawns color = [for r in 0..7 -> Some (pawn color)]
        [Some (rook Black); Some (knight Black); Some (bishop Black); Some (queen Black); Some (king Black); Some (bishop Black); Some (knight Black); Some (rook Black) ] 
        |> List.append (pawns Black)
        |> List.append [for r in 0..31 -> None]
        |> List.append (pawns White)
        |> List.append [Some (rook White); Some (knight White); Some (bishop White); Some (king White); Some (queen White); Some (bishop White); Some (knight White); Some (rook White) ]

    let noPawnGame =
        [Some (rook Black); Some (knight Black); Some (bishop Black); Some (queen Black); Some (king Black); Some (bishop Black); Some (knight Black); Some (rook Black) ] 
        |> List.append [for r in 0..47 -> None]
        |> List.append [Some (rook White); Some (knight White); Some (bishop White); Some (king White); Some (queen White); Some (bishop White); Some (knight White); Some (rook White) ]
    
    let endGame =
        [None; None; None; Some (queen Black); Some (king Black); None; None; None] 
        |> List.append [for r in 0..47 -> None]
        |> List.append [None; None; None; Some (queen White); Some (king White); None; None; None]
        

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
    let inRange id = id < 64 && id >= 0

    let index color = 
        match color with
        | White -> 1
        | Black -> -1
    let emptySpace game id = game |> List.item id |> Option.isNone

    let getPawnMoves game piece fromId kingCheck =
        let i = index piece.Color
        seq {
            let id = fromId + 8 * i
            let id16 = fromId + 16*i

            let attack1 = fromId + 9*i
            let attack2 = fromId + 7*i

            let isFree id = game |> List.item id |> Option.isNone 

            if kingCheck |> not then
                if isFree id then yield id;
                if piece.HasMoved |> not && emptySpace game id && emptySpace game id16 then yield id16;
            
            if isFree attack1 |> not || kingCheck  then yield attack1
            if isFree attack2 |> not || kingCheck then yield attack2
        }
           
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


    let getColor game fromId =
        if inRange fromId then
            game |> List.item fromId |> Option.map (fun r -> r.Color)
        else
            None
    let getEnemy color =
         match color with
         | White -> Black
         | Black -> White

    let isEnemyColor game fromId id =
        getColor game fromId <> getColor game id

    let MoveAndCheckForPieces game isEnemyColor next isValid a  =
        

        match a with
        | Some id when inRange id && isValid id next -> 
            if emptySpace game id then 
                Some(a, Some(id+next)) 
            else if isEnemyColor id then
                Some(a, None)
            else
                None
        | Some _ -> None
        | None -> None

    let getRookMoves game fromId =
        let inColumn = fun toId _ -> (fromId - toId) % 8 = 0
        let inRow = fun toId _ -> getRow fromId = getRow toId
        

        let getMoves skip validate = Seq.unfold (MoveAndCheckForPieces game (isEnemyColor game fromId) skip validate) (Some(fromId + skip))  |> Seq.choose id
        seq {
            // move forward until you hit something
            yield! getMoves 8 inColumn
            yield! getMoves -8 inColumn

            // move sideways until you hit something
            yield! getMoves 1 inRow
            yield! getMoves -1 inRow
        }

    let getBishopMoves game fromId =
        let isInNextRow id next = Math.Abs(getRow (id) - getRow (id-next)) = 1

        let getMoves skip = Seq.unfold (MoveAndCheckForPieces game (isEnemyColor game fromId) skip isInNextRow) (Some(fromId+skip)) |> Seq.choose id

        seq {
            yield! getMoves 9
            yield! getMoves 7
            yield! getMoves -9
            yield! getMoves -7
        }

    let getQueenMoves game fromId =
        getBishopMoves game fromId |> Seq.append (getRookMoves game fromId)

 

    
    let validateMoves toId moves =
        moves |> Seq.contains toId


    let private getMoves game fromId includeMoves =
        let piece = game |> List.item fromId
        match piece with
        | Some p -> 
            match p.Type with
            | Pawn -> getPawnMoves game p fromId includeMoves
            | Knight -> getKnightMoves fromId 
            | Rook -> getRookMoves game fromId 
            | Bishop -> getBishopMoves game fromId
            | Queen -> getQueenMoves game fromId 
            | _ -> Seq.empty
        | None -> Seq.empty

    let getKingMoves game fromId =
        let adds = [1;-1;8;-8;9;-9;7;-7]
        let enemyClr = getColor game fromId |> Option.get |> getEnemy
        let enemies = game 
                    |> List.mapi (fun i p -> match p with | Some pi -> Some(i,pi) | None -> None) 
                    |> List.choose id
                    |> List.filter (fun (_,p) -> p.Color = enemyClr) 
                    |> List.map (fun (i,_) -> getMoves game i true)
                    |> Seq.concat
                    |> List.ofSeq
        let isDangerous id =
            validateMoves id enemies
            
        seq [for next in adds do 
                let id = fromId + next
                if isDangerous id |> not && isEnemyColor game fromId id then 
                    yield id
            ]


    let getMoves2 game fromId =
        let piece = game |> List.item fromId
        match piece with
        | Some p -> 
            match p.Type with
            | King -> getKingMoves game fromId
            | _ -> getMoves game fromId false
        | None -> Seq.empty

    let isValidMoveById game fromId toId =
        getMoves2 game fromId |> validateMoves toId

    let moveById game fromId toId =
        let piece = game |> List.item fromId |> Option.get
        
        [for id in 0..63 -> 
            if fromId = id then None
            else if toId = id then Some {piece with HasMoved = true}
            else game.[id]]

    let moveByXY game fromPos toPos = 
        let fromId = getXY fromPos |> xYToId
        let toId = getXY toPos |> xYToId

        moveById game fromId toId

module ChessIcons =
    open Pieces
    let getIcon (piece:Piece) =
        piece.Name + piece.Color.ToString() + ".png"