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

    type MoveAction = {Piece:Piece option;NewX:int;NewY:int}
    type CanMovePiece = bool * MoveAction
    type Grid = {Width:int;Height:int;Pieces:Piece list}
    let private movePiece piece x y = {piece with X=x;Y=y}
    type CanMove = Pos of int * int | Piece of Piece
    let getPiece grid x y =
        grid.Pieces |> Seq.tryFind(fun r -> r.X = x && r.Y=y)
    let create4by4PawnGame = {Width=4;Height=4;Pieces=[for x in 1..4 do yield pawnStart White x 1; yield pawnStart Black x 4]}
    let canMovePiece grid piece x y =
        match piece with
        | Pos (x,y) -> 
            let p = getPiece grid x y
            (p.IsSome,{Piece=p;NewX=x;NewY=y})
        | Piece p -> (true,{Piece=Some p;NewX=x;NewY=y})
        
    let showGrid showCellAction grid =
        for x in 1..grid.Width do 
        for y in 1..grid.Height do
                                let p = grid.Pieces |> Seq.tryFind(fun p -> p.X = x && p.Y = y)
                                showCellAction x y p
    
    let writeToConsole x y piece = 
        match piece with
        | Some r -> Console.SetCursorPosition(x*4,y*4)
                    Console.Write(r.Color)
                    Console.SetCursorPosition(x*4,y*4+1)
                    Console.Write(r.Type)

        | None -> Console.SetCursorPosition(x*4,y*4)
                  Console.Write(x.ToString() + "," + y.ToString())
        
        
    let showConsoleGrid grid =
        Console.Clear()
        showGrid writeToConsole grid
    
        
    let movePieceOnGrid grid canMove =
        let move,action = canMove
        match action.Piece with
        | Some p ->
            if move then Some {grid with Pieces=[for p in grid.Pieces do if p.X <> p.X && p.Y <> p.Y then yield p else yield movePiece p action.NewX action.NewY]}
            else None
        | None -> None
    
    let takeTurn showGrid requestMove grid =
        showGrid grid
        let move = requestMove grid
        match move with 
        | Some r -> showGrid r 
                    r
        | None _ -> grid
    
    let requestMoveConsole grid =
        Console.WriteLine()
        Console.WriteLine("from x?")
        let x = Console.ReadLine() |> int
        Console.WriteLine("from y?")
        let y = Console.ReadLine() |> int
        Console.WriteLine("to x?")
        let tox = Console.ReadLine() |> int
        Console.WriteLine("to y?")
        let toy = Console.ReadLine() |> int
        let p = getPiece grid x y
        match p with 
        | Some r -> let canMove = canMovePiece grid (Piece r) tox toy
                    movePieceOnGrid grid canMove
        | None _ -> Some grid
    let takeTurnConsole grid =
        takeTurn showConsoleGrid requestMoveConsole grid
        
        
