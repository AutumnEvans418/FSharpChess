namespace Chess

module ChessIcons =
    open Pieces
    let getIcon (piece:Piece) =
        match piece.Color with
        | White -> sprintf "Images\%s-w.png" piece.Name
        | Black -> sprintf "Images\%s-b.png" piece.Name
