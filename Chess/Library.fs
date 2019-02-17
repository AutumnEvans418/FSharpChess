namespace Chess
open FSharp.Core


module Pieces =
    type Range = int * int
    type PieceType = Pawn
    type Movement = L | Foward of Option<Range> | Back of Option<Range> | Right of Option<Range> | Left of Option<Range> 
                    | ForwardRightDiag of Option<Range>
                    | ForwardLeftDiag of Option<Range> | BackRightDiag of Option<Range> | BackLeftDiag of Option<Range>
    type Color = Black | White
    type Piece = {X:int;Y:int;Color:Color;Moves:Movement list;Attacks:Movement list;Type:PieceType}
    let range from tto = Some (Range(from,tto))
    let pawnStart color x y  = {X=x;Y=y;Color=color;Moves=[Foward (range 1 2)];Attacks=[ForwardLeftDiag (range 1 1);ForwardRightDiag (range 1 1)];Type=Pawn}
    let pawnRegular pawn = {pawn with Moves= [Foward (range 1 1)]}
    
module Board =
    open Pieces
    open System.Linq
    open System

    type MoveAction = {Piece:Piece;NewX:int;NewY:int}
    type CanMovePiece = bool * MoveAction
    type Grid = {Width:int;Height:int;Pieces:Piece list}
    let private movePiece piece x y = {piece with X=x;Y=y}

    let create4by4PawnGame = {Width=4;Height=4;Pieces=[for x in 1..4 do yield pawnStart White x 1; yield pawnStart Black x 4]}
    let canMovePiece piece x y =
        (true,{Piece=piece;NewX=x;NewY=y})
    let showGrid showCellAction grid =
        for x in 1..grid.Width do 
        for y in 1..grid.Height do
                                let p = grid.Pieces |> Seq.tryFind(fun p -> p.X = x && p.Y = y)
                                showCellAction x y p
    
    let writeToConsole x y piece = 
        match piece with
        | Some r -> Console.SetCursorPosition(x*4,y*4)
                    Console.Write(r.Type)
        | None -> ()
        
        
    let showConsoleGrid grid =
        showGrid writeToConsole grid
    let movePieceOnGrid grid canMove =
        let move,action = canMove
        if move then Some {grid with Pieces=[for p in grid.Pieces do if p.X <> action.Piece.X && p.Y <> action.Piece.Y then yield p else yield movePiece action.Piece action.NewX action.NewY]}
        else None
