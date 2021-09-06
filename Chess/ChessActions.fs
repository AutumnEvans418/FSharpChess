namespace Chess
open System
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

            let isNextRow id row = 
                let ro1 = getRow fromId 
                let ro2 = getRow id
                ro1 = ro2 + row * -i

            let isFree id = game |> List.item id |> Option.isNone 

            if kingCheck |> not then
                if isNextRow id 1 && inRange id && isFree id then yield id;
                if isNextRow id16 2 && inRange id16 && piece.HasMoved |> not && emptySpace game id && emptySpace game id16 then yield id16;
            
            if isNextRow attack1 1 && inRange attack1 && (isFree attack1 |> not || kingCheck) then yield attack1
            if isNextRow attack2 1 && inRange attack2 && (isFree attack2 |> not || kingCheck) then yield attack2
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

    let getBoardMoves game =
        game |> List.mapi (fun i p -> getMoves2 game (ChessId i) |> Seq.map (fun move -> p,i, move)) |> Seq.concat

    let getMovesByColor game color =
        game |> List.mapi (fun i p -> match p with 
                                        | Some piece -> 
                                            if piece.Color = color then 
                                                getMoves2 game (ChessId i) |> Seq.map (fun move -> p,i,move)
                                            else
                                                Seq.empty
                                        | None -> Seq.empty)
        |> Seq.concat
        //[0..63] 
        //|> List.map (fun i -> (i, getColor game i)) 
        //|> List.filter (fun (i, clr) -> match clr with | Some c -> c = color | _ -> false)
        //|> List.map (fun (i,clr) -> (i, getMoves2 game (ChessId i)))


    let gameOver game = 
        let noBlackMoves = getMovesByColor game Black |> Seq.isEmpty
        let noWhiteMoves = getMovesByColor game White |> Seq.isEmpty

        if noWhiteMoves && isKingChecked game White then Winner Black
        else if noBlackMoves && isKingChecked game Black then Winner Black
        else if noWhiteMoves || noBlackMoves then Tie
        else Na
