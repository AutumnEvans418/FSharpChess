namespace Avalonia.Chess

open Chess.Pieces
open Avalonia.FuncUI.Components

module ChessPage =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Chess.ChessParser
    open Chess.ChessActions
    open Chess.ChessGrid
    
    type State = { 
        game : Piece option list
        fromPos : int option
        Moves: string list
        }
    let init = { game = initialGame2; fromPos = None; Moves=[] }

    type Msg = From of int | To of int | Reset

    let update (msg: Msg) (state: State) : State =
        match msg with
        | From v -> { state with fromPos = Some v }
        | To v -> 
            let fromId = state.fromPos |> Option.get
            let moveAction = moveById state.game fromId v
            match moveAction with
            | Some game ->
                let fromStr = idToPos fromId
                let toStr = idToPos v
                { state with fromPos = None; game = game; Moves = state.Moves |> List.append [sprintf "%s-%s" fromStr toStr] }
            | None -> { state with fromPos = None;}
        | Reset -> init
    
    let view (state: State) (dispatch) =
        let pos = state.fromPos
        Grid.create [
            Grid.rowDefinitions "Auto,*"
            Grid.columnDefinitions "*, 100"
            Grid.children [
                StackPanel.create [
                    Grid.row 0
                    Grid.column 0
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
                    Grid.column 0
                    Grid.rowDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.columnDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.children [
                        for id in 0..63 -> 
                            Button.create [
                                Button.background (match pos with | Some pos when pos = id -> "lightblue" | _ -> "gray")
                                Button.content (state.game.[id] |> Option.fold (fun _ a -> a.Name) "")
                                Button.row (
                                    let x,y = idToXY id
                                    y)
                                Button.column (
                                    let x,y = idToXY id
                                    x)
                                match pos, state.game.[id] with 
                                | Some r, _ -> Button.onClick (fun _ -> dispatch (To id)) 
                                | None, Some a -> Button.onClick (fun _ -> dispatch (From id))
                                | _,_ -> Button.onClick (fun _ -> ())
                            ]
                    ]
                ]
                ListBox.create [
                    Grid.row 1
                    Grid.column 1
                    ListBox.dataItems state.Moves
                    ListBox.itemTemplate (DataTemplateView<string>.create(fun item -> TextBlock.create[TextBlock.text item]))
                ]
            ]
        ]      