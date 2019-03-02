namespace Chess
open FSharp.Core


module Pieces =
    type Range = int * int
    type PieceType = Pawn
    type Movement = L | Forward of Option<Range> | Back of Option<Range> | Right of Option<Range> | Left of Option<Range> 
                    | ForwardRightDiag of Option<Range>
                    | ForwardLeftDiag of Option<Range> | BackRightDiag of Option<Range> | BackLeftDiag of Option<Range>
    type Color = Black | White
    type Piece = {Color:Color;Moves:Movement list;Attacks:Movement list;Type:PieceType}
    let range from tto = Some (Range(from,tto))
    let pawnStart color = {Color=color;Moves=[Forward (range 1 2)];Attacks=[ForwardLeftDiag (range 1 1);ForwardRightDiag (range 1 1)];Type=Pawn}
    let pawnRegular pawn = {pawn with Moves= [Forward (range 1 1)]}
    
module Board =
    open Pieces
    open System.Linq
    open System
    open NUnit.Framework
    open NUnit.Framework


    type MoveAction = {Piece:Piece option;NewX:int;NewY:int}
    type CanMovePiece = bool * MoveAction

    type Square = {X:int;Y:int;Piece:Piece option}
    type Grid = {Squares:Square list}
    

    let createSquares width createPiece =
        [for x in 1..width do for y in 1..width -> createPiece x y]
    
    let createGrid createPiece width  =
        {Squares=createSquares width createPiece}
    
    let createEmptySquare x y =  {X=x;Y=y;Piece=None}
    let createEmptyGrid = createGrid (fun x y -> createEmptySquare x y )
    
    let updateSquare (square:Square) piece =
        {square with Piece=Some piece}
    
    let getPiece grid x y =
        grid.Squares 
        |> List.where(fun r -> r.X=x && r.Y=y) 
        |> List.map(fun r-> r.Piece) 
        |> List.exactlyOne

    let createSquare matches =
        let infun x y =
            let result = matches |> Seq.tryFind(fun (px, py, _) -> px = x && py = y)
            match result with
            | None -> createEmptySquare x y
            | Some (x,y,p) -> {X=x;Y=y;Piece=p}
        infun
    let updateSquares grid updates =
        let infun x y =
            let newPiece = updates |> List.where(fun (fx,fy,_)-> fx=x && fy=y) |> List.map(fun (_,_,p)->p) |> List.tryHead
            match newPiece with
            | Some r -> {X=x;Y=y;Piece=r}
            | None -> 
                        let oldPiece = getPiece grid x y
                        {X=x;Y=y;Piece=oldPiece}
            
        infun
    let getGridWidth grid =
        let l =grid.Squares |> List.length 
        l/4
    let updateGrid grid updates =
        {grid with Squares=createSquares (getGridWidth grid) (updateSquares grid updates)}
    
    let create4By4Grid = createEmptyGrid 4
    
    let setup4By4PawnGame = updateGrid create4By4Grid [for r in 1..4 do yield (r,1,Some (pawnStart White)); yield (r,4,Some ( pawnStart Black))]
    
    
    let addPiece = updateGrid create4By4Grid [(1,1,Some (pawnStart White))]
    //let movePiece piece x y = {piece with X=x;Y=y}
    let tests =
        Assert.AreEqual(1, addPiece.Squares.Count(fun r -> r.Piece.IsSome))

        
        ()
   
        



    type Move = {From:int*int;To:int*int}
    type MoveResult = Success of Grid | Failure of string

    let validMove grid move movement =
        match movement with
        | L -> ()
        | Forward r -> ()

    let movePiece grid move =
        let (fx,fy) = move.From
        let (tx,ty) = move.To
        let piece = getPiece grid fx fy
        match piece with
        | Some p -> 
                    if p.Moves |> List.exists (fun r -> r.)
                    Success (updateGrid grid [(fx,fy,None);(tx,ty,Some p)])
        | None -> Failure (sprintf "there is no piece at (%i,%i)" fx fy)
        
    let handleFailure moveResult grid fail =
        match moveResult with
        | Success r -> r
        | Failure s -> fail s
                       grid

        
    let inline showGrid showCellAction grid =
        let width = getGridWidth grid
        for x in 1..width do 
        for y in 1..width do
                                let p = grid.Squares |> Seq.find(fun p -> p.X = x && p.Y = y)
                                showCellAction x y p
    
    let writeToConsole (x:int) (y:int) cell = 
        match cell.Piece with
        | Some r -> Console.SetCursorPosition(x*4,y*4)
                    Console.Write(r.Color)
                    Console.SetCursorPosition(x*4,y*4+1)
                    Console.Write(r.Type)

        | None -> Console.SetCursorPosition(x*4,y*4)
                  Console.Write(x.ToString() + "," + y.ToString())
        
        
    let showConsoleGrid grid =
        Console.Clear()
        showGrid writeToConsole grid
    
    
    
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
        let m = movePiece grid {From=(x,y);To=(tox,toy)}  
        handleFailure m grid (fun r -> Console.WriteLine(r))
        

    
    let rec playGame grid showGrid requestMove gameOver =
        showGrid grid
        let newGrid = requestMove grid
        if gameOver newGrid then newGrid
        else playGame newGrid showGrid requestMove gameOver

    let playConsoleGame grid =
        playGame grid showConsoleGrid requestMoveConsole (fun r -> false)
        
        
