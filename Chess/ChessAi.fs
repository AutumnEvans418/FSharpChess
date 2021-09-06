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


    let private PlayerValues = dict [
        White,1.
        Black,-1.
    ]

    let private PieceValues = dict [
        King,200.
        Queen,9.
        Rook,5.
        Knight,3.
        Bishop,3.
        Pawn,1.
    ]

    //let getMoveValue game toId =
    //    getValue (game |> List.item toId)

    let getPlayerBoardValue game =
        let pieces = game 
                    |> List.choose id 
                    |> List.map (fun p -> PlayerValues.[p.Color] * PieceValues.[p.Type]) 
                    |> List.sum
        let mobility = game 
                    |> getBoardMoves 
                    |> Seq.map (fun (fr,_,_) -> PlayerValues.[fr.Value.Color])
                    |> Seq.sum
        pieces + mobility

        
        //let nodes = getNode game player
        //let pieces = nodes |> List.map (fun (p, moves) -> p) |> List.length
        //let moves = nodes |> List.map (fun (p, moves) -> moves |> Seq.length) |> List.sum

        //let moveCalc id =
        //    getMoveValue game id

        //let moveValues = nodes |> List.map (fun (p, moves) -> moves |> Seq.map moveCalc |> Seq.sum) |> List.sum
        //[pieces;moves;moveValues] |> List.sum

    let getBoardValue game =
        getPlayerBoardValue game

    // https://www.chessprogramming.org/Simplified_Evaluation_Function
    // https://stackoverflow.com/questions/64417780/increase-the-average-depth-for-the-minimax-algorithm-in-chess
    let rec minimax game alpha beta depth maximizingPlayer player: (int * int) option * float =
        
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
            | None -> (bestId, bestValue)


        let moveCalc (_, fromId, toId) =
            let nGame = moveById game (ChessId fromId) (ChessId toId)
            let _, value = minimax nGame alpha beta (depth-1) (maximizingPlayer |> not) (swap player)
            Some (fromId, toId), value
        
        let nodes = getNode game player

        //let calculate action (i, moves) =
        //    [for move in moves -> moveCalc i move ] |> action

        let getResult action =
            nodes
                .Select(moveCalc)
                |> action

        if depth = 0 || nodes |> Seq.isEmpty then None, getBoardValue game
        else if maximizingPlayer then
            getResult (max (None, -infinity))
        else
            getResult (min (None, infinity))

    let minimax2 game maxPlayer player = 
        minimax game 1. 10. 2 maxPlayer player
        