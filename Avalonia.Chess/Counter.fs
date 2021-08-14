namespace Avalonia.Chess

open Chess.Pieces

module Counter =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Tests.TestModule
    
    type State = { 
        game : PieceType option list
        fromPos : int option
        }
    let init = { game = initialGame2; fromPos = None; }

    type Msg = From of int | To of int | Reset

    let update (msg: Msg) (state: State) : State =
        match msg with
        | From v -> { state with fromPos = Some v }
        | To v -> { state with fromPos = None; game = moveById state.game (state.fromPos |> Option.get) v }
        | Reset -> init
    
    let view (state: State) (dispatch) =
        let pos = state.fromPos
        Grid.create [
            Grid.rowDefinitions "Auto,*"
            Grid.children [
                StackPanel.create [
                    Grid.row 0
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children [
                        Button.create [
                            Button.onClick (fun _ -> dispatch Reset)
                            Button.content "Reset"
                        ]
                    ]
                ]
                Grid.create [
                    Grid.row 1
                    Grid.rowDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.columnDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.children [
                        for id in 0..63 -> 
                            Button.create [
                                Button.background (match pos with | Some pos when pos = id -> "lightblue" | _ -> "gray")
                                Button.content (state.game.[id] |> Option.fold (fun _ a -> a |> string) "")
                                Button.row (
                                    let x,y = idToXY id
                                    x)
                                Button.column (
                                    let x,y = idToXY id
                                    y)
                                match pos, state.game.[id] with 
                                | Some r, _ -> Button.onClick (fun _ -> dispatch (To id)) 
                                | None, Some a -> Button.onClick (fun _ -> dispatch (From id))
                                | _,_ -> Button.onClick (fun _ -> ())
                            ]
                    ]
                ]
            ]
        ]      