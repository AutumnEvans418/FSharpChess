namespace Chess
open FSharp.Core

//module Direction =
//    type Range = int * int
    
//    type Movement = L | Forward of Option<Range> | Back of Option<Range> | Right of Option<Range> | Left of Option<Range> 
//                    | ForwardRightDiag of Option<Range>
//                    | ForwardLeftDiag of Option<Range> | BackRightDiag of Option<Range> | BackLeftDiag of Option<Range>
//    let rangeSingle value =
//        match value with
//        | Some r -> Some (r,r)
//        | None -> None
//    let all rang = 
//        let range = rangeSingle rang
//        [Forward range;ForwardLeftDiag range;ForwardRightDiag range;Back range;BackLeftDiag range;BackRightDiag range;Left range;Right range]
//    let straights rang =
//        let range = rangeSingle rang
//        [Forward range;Right range;Back range;Left range;]
//    let diagnals rang =
//        let range = rangeSingle rang
//        [ForwardLeftDiag range;ForwardRightDiag range;BackLeftDiag range;BackRightDiag range]

module Pieces =
    type Color = Black | White

    type Piece = {Color:Color;HasMoved: bool}
    type PieceType = Pawn of Piece | King of Piece  | Queen of Piece  | Knight of Piece  | Bishop of Piece  | Rook of Piece 
    
    let pawn color = Pawn {Color=color;HasMoved=false}
    let king color = King {Color=color;HasMoved=false}
    let queen color = Queen {Color=color;HasMoved=false}
    let knight color = Knight {Color=color;HasMoved=false}
    let bishop color = Bishop {Color=color;HasMoved=false}
    let rook color = Rook {Color=color;HasMoved=false}

        
