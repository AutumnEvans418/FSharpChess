namespace Chess
open FSharp.Core
open System.Linq
open System

module ChessAi = 
    open ChessActions
    open Pieces
    open ChessParser

    let getNode game color = 
        getMovesByColor game color

    let getValue piece =
        match piece with
        | Some p -> match p.Type with
                    | Queen -> 20
                    | King -> 100
                    | Knight -> 6
                    | Pawn -> 1
                    | Bishop -> 5
                    | Rook -> 5
        | None -> 0

    let getMoveValue game toId =
        getValue (game |> List.item toId)

    let getPlayerBoardValue game player =
        let nodes = getNode game player
        let pieces = nodes |> List.map (fun (p, moves) -> p) |> List.length
        let moves = nodes |> List.map (fun (p, moves) -> moves |> Seq.length) |> List.sum

        let moveCalc id =
            getMoveValue game id

        let moveValues = nodes |> List.map (fun (p, moves) -> moves |> Seq.map moveCalc |> Seq.sum) |> List.sum
        [pieces;moves;moveValues] |> List.sum

    let getBoardValue game player =
        let current = getPlayerBoardValue game player
        let enemy = getPlayerBoardValue game (swap player)
        current - enemy

    // https://www.chessprogramming.org/Simplified_Evaluation_Function
    // https://stackoverflow.com/questions/64417780/increase-the-average-depth-for-the-minimax-algorithm-in-chess
    let rec minimax game alpha beta depth maximizingPlayer player: (int * int) option * int =
        
        //let min list = list |> Seq.append [None,Int32.MaxValue] |> Seq.minBy (fun (_, value) -> value)
        //let max list = list |> Seq.append [None,Int32.MinValue] |> Seq.maxBy (fun (_, value) -> value)
        
        let rec max (bestId,bestValue) list =
            match list |> Seq.tryHead with
            | Some (a,value) -> 
                if value >= beta then a,value
                else if value > alpha then a,value
                else
                    if value > bestValue then
                        max (a, value) (list |> Seq.tail)
                    else
                        max (bestId, bestValue) (list |> Seq.tail)
            | None -> (bestId, bestValue)  

        let rec min (bestId,bestValue) list =
            match list |> Seq.tryHead with
            | Some (a,value) -> 
                if value <= alpha then a,value
                else if value < beta then a,value
                else 
                    if value < bestValue then
                        min (a, value) (list |> Seq.tail)
                    else
                        min (bestId, bestValue) (list |> Seq.tail)
            | None -> None,0


        let moveCalc (fromId, toId) =
            let nGame = moveById game (ChessId fromId) (ChessId toId)
            let _, value = minimax nGame alpha beta (depth-1) (maximizingPlayer |> not) (swap player)
            Some (fromId, toId), value
        
        let nodes = getNode game player

        //let calculate action (i, moves) =
        //    [for move in moves -> moveCalc i move ] |> action

        let getResult action =
            nodes.ToList()
                .SelectMany(fun (fromId, items) -> items.Select(fun toId -> (fromId,toId)))
                .Select(moveCalc)
                |> action

        if depth = 0 || nodes |> List.isEmpty then None, getBoardValue game player
        else if maximizingPlayer then
            getResult (max (None, Int32.MinValue))
        else
            getResult (min (None, Int32.MaxValue))

    let minimax2 game maxPlayer player = 
        minimax game 0 10 2 maxPlayer player
        