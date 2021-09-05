namespace Chess

module ChessIcons =
    open Pieces
    let getIcon (piece:Piece) =
        piece.Name + piece.Color.ToString() + ".png"
