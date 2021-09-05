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
    open Chess.ChessAi
    

    type State = { 
        game : Piece option list
        fromPos : int option
        Moves: string list
        Player: Color
        GameOver: EndGame
        Turn: Color
        Ai: Color
        }
    let init = { game = initialGame; fromPos = None; Moves=[]; Player = White; GameOver = Na; Turn=White; Ai=Black }

    type Msg = 
        | From of int 
        | To of int 
        | Reset 
        | RemovePawns 
        | Flip 
        | CopyMoves
        | EndGame
        | Undo

    let addMoves moves fromId toId =
        let fromStr = idToPos fromId
        let toStr = idToPos toId
        [sprintf "%s-%s" fromStr toStr] |> List.append moves

    let update (msg: Msg) (state: State) : State =
        match msg with
        | From v -> { state with fromPos = Some v }
        | To v -> 
            let fromId = state.fromPos |> Option.get
            if isValidMoveById state.game fromId v then
                let moveAction = moveById state.game fromId v

                let enemy = swap state.Turn
                let nState = { state with fromPos = None; game = moveAction; Moves = addMoves state.Moves fromId v; GameOver = gameOver moveAction; Turn = enemy }

                let eMove, _ = minimax2 nState.game true enemy
                match eMove with
                | Some (eFromId, eToId) -> 
                    let eMoveAction = moveById nState.game eFromId eToId
                    { nState with game = eMoveAction; GameOver = gameOver eMoveAction; Turn = state.Turn; Moves = addMoves nState.Moves eFromId eToId }
                | None -> nState
            else 
                { state with fromPos = None }
        | Reset -> init
        | RemovePawns -> { init with game = noPawnGame }
        | Flip -> { state with Player = swap state.Player  }
        | CopyMoves -> 
            Application.Current.Clipboard.SetTextAsync (state.Moves |> String.concat ",") |> ignore
            state
        | EndGame -> {init with game = endGame }
        | Undo -> 
            if state.Moves.IsEmpty then state
            else
                let moveStr = state.Moves.[state.Moves.Length-1]
                let tos,froms = parseMove moveStr
                let toId = getXY tos |> xYToId
                let fromId = getXY froms |> xYToId
                let moves = state.Moves |> List.filter (fun r -> r <> moveStr)
                let action = moveById state.game fromId toId 
                {state with Moves = moves; game = action}
    
    let button dispatch name action =
        Button.create [
            Button.onClick (fun _ -> dispatch action)
            Button.content (name |> string)
        ]

    let gameOverView (state: State) =
        StackPanel.create [
            Grid.row 1
            Grid.column 0
            StackPanel.verticalAlignment VerticalAlignment.Center
            StackPanel.horizontalAlignment HorizontalAlignment.Center
            StackPanel.children [
                TextBlock.create [
                    let txt = match state.GameOver with
                                | Winner clr -> sprintf "Player %O Won!" clr
                                | Tie -> "Tie!"
                                | _ -> ""
                    TextBlock.text txt
                ]
            ]
        ]

    let chessBoard (state: State) (dispatch) =
        let fromPos = state.fromPos
        let color id = state.game.[id] |> Option.map (fun r -> r.Color)
        Grid.create [
            Grid.row 1
            Grid.column 0
            Grid.rowDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
            Grid.columnDefinitions (System.String.Join(",",[for r in 0..7 -> "*"]))
            Grid.children [
                for id in 0..63 -> 
                    let idStr = sprintf "(%s %i)" (idToPos id) id
                    let x,y = idToXY id

                    let background, forground = match fromPos, color id with 
                                                | Some pos, _ when pos = id -> "lightblue","black"
                                                | Some pos, _ when isValidMoveById state.game pos id -> "yellow","black"
                                                | _, Some White -> "lightgray","black"
                                                | _, Some Black -> "black","white"
                                                | _, None -> 
                                                    if (id + (y % 2)) % 2 = 0 then
                                                        "gray","white"
                                                    else
                                                        "lightyellow","black"
                    
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
                        | Some _, _ -> Button.onClick (fun _ -> dispatch (To id)) 
                        | None, Some item when item.Color = state.Turn -> Button.onClick (fun _ -> dispatch (From id))
                        | _,_ -> Button.onClick (fun _ -> ())
                    ]
            ]
        ]

    let toolPanel (state: State) (dispatch) =
        StackPanel.create [
            Grid.row 0
            Grid.column 0
            StackPanel.margin 10.
            StackPanel.spacing 10.
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                button dispatch "Reset" Reset
                button dispatch "No Pawn" RemovePawns
                button dispatch "Flip" Flip
                button dispatch "Copy Moves" CopyMoves
                button dispatch "End Game" EndGame
                button dispatch "Undo" Undo
                TextBlock.create [
                    TextBlock.text (sprintf "Player's %O Turn" state.Turn)
                ]
            ]
        ]

    let movesPanel (state: State) = 
        ListBox.create [
            Grid.row 1
            Grid.column 1
            ListBox.dataItems state.Moves
            ListBox.itemTemplate (DataTemplateView<string>.create(fun item -> TextBlock.create[TextBlock.text item]))
        ]

    let view (state: State) (dispatch) =
        //let fromPiece = fromPos |> Option.map (fun r -> state.game.[r]) |> Option.flatten |> Option.map (fun r -> r.Color)
        Grid.create [
            Grid.rowDefinitions "Auto,*"
            Grid.columnDefinitions "*, 100"
            Grid.children [
                toolPanel state dispatch
                movesPanel state
                match state.GameOver with
                | Na -> chessBoard state dispatch
                | _ -> gameOverView state
            ]
        ]      