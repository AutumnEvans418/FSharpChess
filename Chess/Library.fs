namespace Chess
open FSharp.Core


module Pieces =
    type Range = int * int
    type PieceType = Pawn
    type Movement = L | Foward of Option<Range> | Back of Option<Range> | Right of Option<Range> | Left of Option<Range> 
                    | ForwardRightDiag of Option<Range>
                    | ForwardLeftDiag of Option<Range> | BackRightDiag of Option<Range> | BackLeftDiag of Option<Range>
    type Color = Black | White
    type Piece = {Color:Color;Moves:Movement list;Attacks:Movement list;Type:PieceType}
    let range from tto = Some (Range(from,tto))
    let pawnStart color = {Color=color;Moves=[Foward (range 1 2)];Attacks=[ForwardLeftDiag (range 1 1);ForwardRightDiag (range 1 1)];Type=Pawn}
    let pawnRegular pawn = {pawn with Moves= [Foward (range 1 1)]}
    
module Board =
    open Pieces
    open System.Linq
    open System

    type MoveAction = {Piece:Piece option;NewX:int;NewY:int}
    type CanMovePiece = bool * MoveAction

    type Square = {X:int;Y:int;Piece:Piece option}
    type Grid = {Width:int;Height:int;Squares:Square list}
    

    let createSquares width height createPiece =
        [for x in 1..width do for y in 1..height -> createPiece x y]
    
    let createGrid createPiece width height  =
        {Width=width;Height=height;Squares=createSquares width height createPiece}
    
    let createEmptySquare x y =  {X=x;Y=y;Piece=None}
    let createEmptyGrid = createGrid (fun x y -> createEmptySquare x y )
    
    let updateSquare (square:Square) piece =
        {square with Piece=Some piece}
    
    let createSquare matches =
        let infun x y =
            let result = matches |> Seq.tryFind(fun (px, py, _) -> px = x && py = y)
            match result with
            | None -> createEmptySquare x y
            | Some (x,y,p) -> {X=x;Y=y;Piece=Some p}
        infun
    let updateGrid grid updates =
        {grid with Squares=createSquares grid.Width grid.Height (createSquare updates)}
    
    let create4By4Grid = createEmptyGrid 4 4
    
    let addPiece = updateGrid create4By4Grid [(1,1, pawnStart White)]
    //let movePiece piece x y = {piece with X=x;Y=y}

    





    type CanMove = Pos of int * int | Piece of Piece
    let getPiece grid x y =
        grid.Squares |> Seq.tryFind(fun r -> r.X = x && r.Y=y)
    
    
    
    let create4by4PawnGame = 
        {  
            Width=4;
            Height=4;
            Squares=[for x in 1..4 do yield {X=x;Y=1;Piece= Some (pawnStart White)}; yield {X=x;Y=4;Piece= Some (pawnStart Black)}]
        }
    let canMovePiece grid piece x y =
        match piece with
        | Pos (x,y) -> 
            let p = getPiece grid x y
            (p.IsSome,{Piece=Some p;NewX=x;NewY=y})
        | Piece p -> (true,{Piece=Some p;NewX=x;NewY=y})
        
    let showGrid showCellAction grid =
        for x in 1..grid.Width do 
        for y in 1..grid.Height do
                                let p = grid.Squares |> Seq.tryFind(fun p -> p.X = x && p.Y = y)
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
    
    
    let replacePiece grid actionPiece newX newY =
        [for p in grid.Pieces do 
                              if (p.X = actionPiece.X && p.Y = actionPiece.Y) then 
                                      yield movePiece p newX newY 
                              else yield p]
    let movePieceOnGrid grid canMove =
        let move,action = canMove
        
        match action.Piece with
        | Some actionPiece ->
            if move then Some {grid with Pieces=replacePiece grid actionPiece action.NewX action.NewY}
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
        
        
