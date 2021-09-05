namespace Chess
open System
open System.Linq

module ChessParser =
    type ChessBoardId = ChessXY of int * int | ChessAlpha of string | ChessId of int
    
    let private mapper = dict['a', 0;'b', 1;'c', 2;'d', 3;'e', 4;'f', 5;'g', 6;'h', 7]

    let parseMove (str:string) =
        let from = str.Split('-').[0]
        let tTo = str.Split('-').[1]
        ChessAlpha from, ChessAlpha tTo

    let parseMoves (str:string) = 
        let moves = [for r in str.Split(',') do if String.IsNullOrWhiteSpace r |> not then yield r] 
        moves |> List.map parseMove

    let private alphaToXY (position: string) =
        let xPos = mapper.[position.[0]]
        let yPos = System.Int32.Parse(position.[1].ToString()) - 1
        (xPos, yPos)

    let private xYToId (x,y) =
        y * 8 + x

    let getRow id =
        id / 8

    let private idToXY id = 
        let y = getRow id
        let x = id - y * 8
        (x,y)
        
    let between x a b =
        x >= a && x <= b

    let private xYtoPos (x,y) = 
        let xPos = mapper.First(fun r -> r.Value = x).Key
        sprintf "%c%i" xPos (y+1) 
    
    let private idToPos id = idToXY id |> xYtoPos
        

    let getAlpha bId =
        match bId with
        | ChessAlpha a -> a
        | ChessXY (x,y) -> xYtoPos (x,y)
        | ChessId id -> idToPos id
       
    let getXY bId =
        match bId with
        | ChessAlpha a -> alphaToXY a
        | ChessXY (x,y) -> (x,y)
        | ChessId id -> idToXY id

    let getId bId =
        match bId with
        | ChessAlpha a -> alphaToXY a |> xYToId
        | ChessXY (x,y) -> xYToId (x,y)
        | ChessId id -> id