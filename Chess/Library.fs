namespace Chess
open FSharp.Core
module Say =
    let hello name =
        printfn "Hello %s" name

module Pieces =
    type Range = int * int
    type Movement = L | Foward of Option<Range> | Back of Option<Range> | Right of Option<Range> | Left of Option<Range> 
                    | ForwardRightDiag of Option<Range>
                    | ForwardLeftDiag of Option<Range> | BackRightDiag of Option<Range> | BackLeftDiag of Option<Range>
    type Color = Black | White
    type PieceLocation = {X:int;Y:int;Color:Color;Moves:Movement list;Attacks:Movement list}
    let range from tto = Some (Range(from,tto))
    let pawnStart color x y  = {X=x;Y=y;Color=color;Moves=[Foward (range 1 2)];Attacks=[ForwardLeftDiag (range 1 1);ForwardRightDiag (range 1 1)]}
    
module Board =
    0