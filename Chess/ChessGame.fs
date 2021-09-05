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
    let swap color = 
        match color with | White -> Black | Black -> White  

        
module ChessParser =
    type ChessBoardId = ChessXY of int * int | ChessAlpha of string | ChessId of int
    
    let private mapper = dict['a', 0;'b', 1;'c', 2;'d', 3;'e', 4;'f', 5;'g', 6;'h', 7]

    let parseMove (str:string) =
        let from = str.Split('-').[0]
        let tTo = str.Split('-').[1]
        ChessAlpha from, ChessAlpha tTo

    let parseMoves (str:string) = 
        let moves = [for r in str.Split(',') do if String.IsNullOrWhiteSpace r |> not then yield r] 
        moves |> List.map parseMove

    let private alphaToXY (position: string) =
        let xPos = mapper.[position.[0]]
        let yPos = System.Int32.Parse(position.[1].ToString()) - 1
        (xPos, yPos)

    let private xYToId (x,y) =
        y * 8 + x

    let getRow id =
        id / 8

    let private idToXY id = 
        let y = getRow id
        let x = id - y * 8
        (x,y)
        
    let between x a b =
        x >= a && x <= b

    let private xYtoPos (x,y) = 
        let xPos = mapper.First(fun r -> r.Value = x).Key
        sprintf "%c%i" xPos (y+1) 
    
    let private idToPos id = idToXY id |> xYtoPos
        

    let getAlpha bId =
        match bId with
        | ChessAlpha a -> a
        | ChessXY (x,y) -> xYtoPos (x,y)
        | ChessId id -> idToPos id
       
    let getXY bId =
        match bId with
        | ChessAlpha a -> alphaToXY a
        | ChessXY (x,y) -> (x,y)
        | ChessId id -> idToXY id

    let getId bId =
        match bId with
        | ChessAlpha a -> alphaToXY a |> xYToId
        | ChessXY (x,y) -> xYToId (x,y)
        | ChessId id -> id

module ChessGrid =
    open Pieces
    open ChessParser

    let initialGame =
        let pawns color = [for r in 0..7 -> Some (pawn color)]

        let backRow c = 
            [rook c;knight c;bishop c;queen c;king c;bishop c;knight c;rook c] 
            |> List.map (fun r -> Some r)
        backRow Black
        |> List.append (pawns Black)
        |> List.append [for r in 0..31 -> None]
        |> List.append (pawns White)
        |> List.append (backRow White)

    let noPawnGame =
        let pawns color = [None;None;None;Some (pawn color);Some (pawn color);None;None;None]

        [Some (rook Black); Some (knight Black); Some (bishop Black); Some (queen Black); Some (king Black); Some (bishop Black); Some (knight Black); Some (rook Black) ] 
        |> List.append (pawns Black)
        |> List.append [for r in 0..31 -> None]
        |> List.append (pawns White)
        |> List.append [Some (rook White); Some (knight White); Some (bishop White); Some (king White); Some (queen White); Some (bishop White); Some (knight White); Some (rook White) ]
    //based on https://www.chess.com/terms/draw-chess
    let endGame2 = 
        [Some(king Black)]
        |> List.append [for r in 0..8 -> None]
        |> List.append [Some(queen White)]
        |> List.append [Some(king White)]
        |> List.append [for r in 0..51 -> None]

    let endGame =
        [None; None; None; Some (queen Black); Some (king Black); None; None; None] 
        |> List.append [for r in 0..47 -> None]
        |> List.append [None; None; None; Some (queen White); Some (king White); None; None; None]
        


    let lookup game bId =
        game |> List.item (getId bId)
    
    let convertToGrid list = 
        [for y in 0..7 -> [for x in 0..7 -> lookup list (ChessXY (x,y))]]

module ChessActions =
    open Pieces
    open ChessParser
    open ChessGrid
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

            let isFree id = inRange id && game |> List.item id |> Option.isNone 

            if kingCheck |> not then
                if isFree id then yield id;
                if inRange id16 && piece.HasMoved |> not && emptySpace game id && emptySpace game id16 then yield id16;
            
            if inRange attack1 && (isFree attack1 |> not || kingCheck) then yield attack1
            if inRange attack2 && (isFree attack2 |> not || kingCheck) then yield attack2
        }
           
    let getKnightMoves fromId =
        let moves = [
            fromId+15,2
            fromId+17,2
            fromId-15,-2
            fromId-17,-2
            fromId+10,1
            fromId-10,-1
            fromId+6,1
            fromId-6,-1]

        let row = getRow fromId

        moves |> List.filter (fun (toId, rowDiff) -> getRow toId = row + rowDiff && inRange toId) |> List.map (fun (toId, _) -> toId) |> List.toSeq


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
        moves |> Seq.contains (getId toId)


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

    let isPieceInCheck game fromId toId =
        let enemyClr = getColor game (getId fromId) |> Option.get |> getEnemy
        let enemies = game 
                    |> List.mapi (fun i p -> match p with | Some pi -> Some(i,pi) | None -> None) 
                    |> List.choose id
                    |> List.filter (fun (_,p) -> p.Color = enemyClr) 
                    |> List.map (fun (i,_) -> getMoves game i true)
                    |> Seq.concat
                    |> List.ofSeq
        validateMoves toId enemies

    let isKingChecked game color: bool =
        [0..63] |> List.exists (fun id -> 
            let piece = game |> List.item id
            match piece with
            | Some p -> p.Type = King && p.Color = color && isPieceInCheck game (ChessId id) (ChessId id)
            | None -> false)

    
          
            

    let private getKingMoves game fromId =
        let adds = [(1,0);(-1,0);(8,1);(-8,1);(9,1);(-9,1);(7,1);(-7,1)]
        let check = isPieceInCheck game (ChessId fromId)
        seq [for (next,rowDif) in adds do 
                let id = fromId + next
                let row = getRow fromId - getRow id |> Math.Abs
                if row = rowDif && inRange id && check (ChessId id) |> not && isEnemyColor game fromId id then 
                    yield id
            ]

    let moveById game fromId toId =
        let piece = lookup game fromId |> Option.get
    
        [for id in 0..63 -> 
            if getId fromId = id then None
            else if getId toId = id then Some {piece with HasMoved = true}
            else game.[id]]

    

    let private pruneMoves game fromId moves =
        let color = getColor game (getId fromId)
        match color with 
        | None -> moves
        | Some clr ->
            if isKingChecked game clr |> not then moves
            else
                seq [for move in moves do 
                        let nGame = moveById game fromId (ChessId move)
                        if isKingChecked nGame clr |> not then yield move
                    ]

    let getMoves2 game fromId =
        let pruner = pruneMoves game fromId

        let piece = game |> List.item (getId fromId)
        match piece with
        | Some p -> 
            match p.Type with
            | King -> getKingMoves game (getId fromId) |> pruner
            | _ -> getMoves game (getId fromId) false |> pruner
        | None -> Seq.empty

    let isValidMoveById game fromId toId =
        getMoves2 game fromId |> validateMoves toId

    type EndGame =
        | Winner of Color
        | Tie
        | Na
    
    let getMovesByColor game color =
        [0..63] 
        |> List.map (fun i -> (i, getColor game i)) 
        |> List.filter (fun (i, clr) -> match clr with | Some c -> c = color | _ -> false)
        |> List.map (fun (i,clr) -> (i, getMoves2 game (ChessId i)))

    let gameOver game = 
        let noBlackMoves = getMovesByColor game Black |> List.exists (fun (i,l) -> l |> Seq.length > 0) |> not
        let noWhiteMoves = getMovesByColor game White |> List.exists (fun (i,l) -> l |> Seq.length > 0) |> not

        if noWhiteMoves && isKingChecked game White then Winner Black
        else if noBlackMoves && isKingChecked game Black then Winner Black
        else if noWhiteMoves || noBlackMoves then Tie
        else Na

module ChessIcons =
    open Pieces
    let getIcon (piece:Piece) =
        piece.Name + piece.Color.ToString() + ".png"

module ChessAi = 
    open ChessActions
    open Pieces
    open ChessParser

    let getNode game color = 
        getMovesByColor game color

    let getValue piece =
        match piece with
        | Some p -> match p.Type with
                    | Queen -> 20
                    | King -> 100
                    | Knight -> 6
                    | Pawn -> 1
                    | Bishop -> 5
                    | Rook -> 5
        | None -> 0

    let getMoveValue game toId =
        getValue (game |> List.item toId)

    let getPlayerBoardValue game player =
        let nodes = getNode game player
        let pieces = nodes |> List.map (fun (p, moves) -> p) |> List.length
        let moves = nodes |> List.map (fun (p, moves) -> moves |> Seq.length) |> List.sum

        let moveCalc id =
            getMoveValue game id

        let moveValues = nodes |> List.map (fun (p, moves) -> moves |> Seq.map moveCalc |> Seq.sum) |> List.sum
        [pieces;moves;moveValues] |> List.sum

    let getBoardValue game player =
        let current = getPlayerBoardValue game player
        let enemy = getPlayerBoardValue game (swap player)
        current - enemy

    // https://www.chessprogramming.org/Simplified_Evaluation_Function
    // https://stackoverflow.com/questions/64417780/increase-the-average-depth-for-the-minimax-algorithm-in-chess
    let rec minimax game alpha beta depth maximizingPlayer player: (int * int) option * int =
        
        //let min list = list |> Seq.append [None,Int32.MaxValue] |> Seq.minBy (fun (_, value) -> value)
        //let max list = list |> Seq.append [None,Int32.MinValue] |> Seq.maxBy (fun (_, value) -> value)
        
        let rec max (bestId,bestValue) list =
            match list |> Seq.tryHead with
            | Some (a,value) -> 
                if value >= beta then a,value
                else if value > alpha then a,value
                else
                    if value > bestValue then
                        max (a, value) (list |> Seq.tail)
                    else
                        max (bestId, bestValue) (list |> Seq.tail)
            | None -> (bestId, bestValue)  

        let rec min (bestId,bestValue) list =
            match list |> Seq.tryHead with
            | Some (a,value) -> 
                if value <= alpha then a,value
                else if value < beta then a,value
                else 
                    if value < bestValue then
                        min (a, value) (list |> Seq.tail)
                    else
                        min (bestId, bestValue) (list |> Seq.tail)
            | None -> None,0


        let moveCalc (fromId, toId) =
            let nGame = moveById game (ChessId fromId) (ChessId toId)
            let _, value = minimax nGame alpha beta (depth-1) (maximizingPlayer |> not) (swap player)
            Some (fromId, toId), value
        
        let nodes = getNode game player

        //let calculate action (i, moves) =
        //    [for move in moves -> moveCalc i move ] |> action

        let getResult action =
            nodes.ToList()
                .SelectMany(fun (fromId, items) -> items.Select(fun toId -> (fromId,toId)))
                .Select(moveCalc)
                |> action

        if depth = 0 || nodes |> List.isEmpty then None, getBoardValue game player
        else if maximizingPlayer then
            getResult (max (None, Int32.MinValue))
        else
            getResult (min (None, Int32.MaxValue))

    let minimax2 game maxPlayer player = 
        minimax game 0 10 2 maxPlayer player
        