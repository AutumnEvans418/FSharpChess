namespace Chess

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
