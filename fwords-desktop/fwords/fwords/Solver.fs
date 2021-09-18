namespace fwords

/// This is a view that lets you solve an existing Puzzle
module Solver = 
    open Elmish
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Input
    open Avalonia.Interactivity
    open Avalonia.FuncUI.DSL

    open fwords.Core

    type State = {
        puzzle: CluedPuzzle
        solution: Solution
        selected: int*int
        orientation: ClueOrientation
    }

    let init (cp:CluedPuzzle option) (s:Solution option) : (State * Cmd<_>) = 
        let myPuzzle = defaultArg cp (CluedPuzzle.generateBlank 1 1)
        let mySolution = defaultArg s (Solution.generateBlank myPuzzle.puzzle)
        Puzzle.checkSame myPuzzle.puzzle mySolution.puzzle // This may throw invalidArg
        {puzzle=myPuzzle; solution=mySolution; selected=(-1,-1); orientation=Across}, Cmd.none


    let update (msg: SolverMsg) (state: State) =
        match msg with
        | SolverMsg.ToLobby -> {state with selected=(-1,-1)}, Cmd.ofMsg (ShellMsg.SetView LobbyView)
        | SolverMsg.ToLibrary -> {state with selected=(-1,-1)}, Cmd.ofMsg (ShellMsg.SetView LibraryView)
        | SolverMsg.SelectCell (row, col) -> 
            {state with selected=row,col}, Cmd.none
        | SolverMsg.MoveSelection direction ->
            let nextCell = 
                match state.selected with 
                | (row,col) -> 
                    match Puzzle.getNextCell state.solution.puzzle direction row col with
                    | Some (nrow,ncol) -> nrow,ncol
                    | None -> row,col
            {state with selected=nextCell}, Cmd.none
        | SolverMsg.SetCell letter -> 
            match state.selected with
            | (row,col) -> 
                let newsol = Solution.setCell state.puzzle.puzzle state.solution row col letter
                let moveCursorMsg = 
                    match state.orientation with
                    | ClueOrientation.Across -> SolverMsg.MoveSelection Rightwards
                    | ClueOrientation.Down -> SolverMsg.MoveSelection Downwards
                    |> ShellMsg.SolverMsg
                { state with solution=newsol}, Cmd.ofMsg moveCursorMsg
        | ToggleOrientation -> 
            let nexto = if state.orientation=Across then Down else Across
            { state with orientation=nexto }, Cmd.none

    let cellKeyEventHandler (dispatch: SolverMsg -> unit) (keyEvt:KeyEventArgs) (state:State) row col = 
        if keyEvt.Route = RoutingStrategies.Tunnel then // For some reason we were getting both tunnel and bubble events for alpha keys
            match keyEvt.Key with
            | Key.Space -> dispatch SolverMsg.ToggleOrientation
            | Key.Up -> Direction.Upwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Down -> Direction.Downwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Left -> Direction.Leftwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Right -> Direction.Rightwards |> SolverMsg.MoveSelection |> dispatch
            | c when c >= Key.A && Key.Z >= c ->
                let letter = char ((int c) + (int 'A') - (int Key.A)) // Convert Key code to character
                letter |> SolverMsg.SetCell |> dispatch
            | _ -> () // Do nothing for any other key code


    //let viewCell (state: State) (dispatch: SolverMsg -> unit) row col letter = 
    let viewCell (state:State) (dispatch: SolverMsg -> unit) row col letter = 
        Button.create [
            Grid.row row
            Grid.column col
            if letter <> Puzzle.FILL_CHAR then
                Button.classes ["cell"]
                Button.content(string letter)
                Button.onClick (fun _ -> (row,col) |> SolverMsg.SelectCell |> dispatch)
                Button.onGotFocus (fun _ -> (row,col) |> SolverMsg.SelectCell |> dispatch)
                match state.selected with
                | (r, c) ->
                    if r=row && c=col then 
                        Button.background "Orange"
            else
                Button.isEnabled false
                Button.background "White"
        ]

    let view (state: State) (dispatch: SolverMsg -> unit) =
        Grid.create [ // Separate navigation buttons from everything else
            Grid.columnDefinitions "*"
            Grid.rowDefinitions "*, Auto"
            Grid.children [
                Grid.create [ // Separate Clues area from Puzzle area
                    Grid.row 0
                    Grid.columnDefinitions "*, Auto"
                    Grid.rowDefinitions "*"
                    Grid.children [
                        ScrollViewer.create [
                            Grid.column 0
                            ScrollViewer.dock Dock.Left
                            ScrollViewer.content (
                                Grid.create [
                                    Grid.columnDefinitions "*,*"
                                    Grid.rowDefinitions "auto"
                                    Grid.children [
                                        StackPanel.create [
                                            Grid.column 0
                                            StackPanel.children [
                                                for clue in state.puzzle.across do
                                                    yield TextBlock.create [ TextBlock.text clue ]
                                            ]
                                        ]
                                        StackPanel.create [
                                            Grid.column 1
                                            StackPanel.children [
                                                for clue in state.puzzle.down do
                                                    yield TextBlock.create [ TextBlock.text clue ]
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                        // Puzzle area
                        Grid.create [
                            let rows = Puzzle.getRows state.puzzle.puzzle
                            let cols = Puzzle.getCols state.puzzle.puzzle
                            let rowdefs = [for i in 0..(rows-1) -> "30"] |> String.concat ","
                            let coldefs = [for i in 0..(cols-1) -> "30"] |> String.concat ","
                            Grid.column 1
                            Grid.dock Dock.Right
                            Grid.horizontalAlignment HorizontalAlignment.Right 
                            Grid.columnDefinitions coldefs
                            Grid.rowDefinitions rowdefs
                            Grid.children [
                                for i in 0..(rows-1) do
                                    for j in 0..(cols-1) do
                                        yield viewCell state dispatch i j (Puzzle.getCell state.solution.puzzle i j)
                            ]
                            match state.selected with
                            | (row, col) -> Grid.onKeyDown (fun keyEvt -> cellKeyEventHandler dispatch keyEvt state row col)
                        ]
                    ]
                ]
                // Navigation buttons
                Grid.create [
                    Grid.row 1
                    Grid.verticalAlignment VerticalAlignment.Bottom
                    Grid.columnDefinitions "Auto, *, Auto"
                    Grid.rowDefinitions "Auto"
                    Grid.children [
                        Button.create [
                            Grid.column 0
                            Button.content "Library"
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLibrary)
                            Button.classes ["pretty"]
                        ]
                        Button.create [
                            Grid.column 2
                            Button.content "Exit to Lobby"
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLobby)
                            Button.classes ["pretty"]
                        ]
                    ]
                ]
            ]
        ]