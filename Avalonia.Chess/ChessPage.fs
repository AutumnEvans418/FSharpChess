namespace Avalonia.Chess

open Chess.Pieces
open Avalonia.FuncUI.Components
open Avalonia

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
        Player: Color
        }
    let init = { game = initialGame; fromPos = None; Moves=[]; Player = White }

    type Msg = 
        | From of int 
        | To of int 
        | Reset 
        | RemovePawns 
        | Flip 
        | CopyMoves
        | EndGame

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
                { state with fromPos = None; game = game; Moves = [sprintf "%s-%s" fromStr toStr] |> List.append state.Moves }
            | None -> { state with fromPos = None;}
        | Reset -> init
        | RemovePawns -> { init with game = noPawnGame }
        | Flip -> { state with Player = match state.Player with | White -> Black | Black -> White }
        | CopyMoves -> 
            Application.Current.Clipboard.SetTextAsync (state.Moves |> String.concat ",") |> ignore
            state
        | EndGame -> {init with game = endGame }
    
    let view (state: State) (dispatch) =
        let fromPos = state.fromPos
        //let fromPiece = fromPos |> Option.map (fun r -> state.game.[r]) |> Option.flatten |> Option.map (fun r -> r.Color)
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
                        Button.create [
                            Button.onClick (fun _ -> dispatch RemovePawns)
                            Button.content "Remove Pawns"
                        ]
                        Button.create [
                            Button.onClick (fun _ -> dispatch Flip)
                            Button.content "Flip"]
                        Button.create [
                            Button.onClick (fun _ -> dispatch CopyMoves)
                            Button.content "Copy Moves"]
                        Button.create [
                            Button.onClick (fun _ -> dispatch EndGame)
                            Button.content "End Game"]
                    ]
                ]
                Grid.create [
                    Grid.row 1
                    Grid.column 0
                    Grid.rowDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.columnDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
                    Grid.children [
                        for id in 0..63 -> 
                            let idStr = sprintf "(%s)" (idToPos id)
                            let x,y = idToXY id

                            let background, forground = match fromPos, state.game.[id] |> Option.map (fun r -> r.Color) with 
                                                        | Some pos, _ when pos = id -> "lightblue","black"
                                                        | Some pos, _ when isValidMoveById state.game pos id -> "yellow","black"
                                                        | _, Some White -> "lightgray","black"
                                                        | _, Some Black -> "black","white"
                                                        | _, None -> "gray","white"
                            
                            let row = match state.Player with
                                        | White -> 7-y
                                        | Black -> y

                            Button.create [
                                Button.background background
                                Button.foreground forground
                                Button.content (state.game.[id] |> Option.fold (fun _ a -> a.Name + " " + idStr) idStr)
                                Button.row row
                                Button.column x
                                match fromPos, state.game.[id] with 
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