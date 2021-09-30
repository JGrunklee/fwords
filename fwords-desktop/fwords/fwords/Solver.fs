namespace fwords

/// This is a view that lets you solve an existing Puzzle
module Solver = 
    open Elmish
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.Layout
    open Avalonia.Input
    open Avalonia.Media
    open Avalonia.Interactivity
    open Avalonia.FuncUI.DSL

    open fwords.Core

    type State = {
        puzzle: CluedPuzzle
        solution: Solution
        selected: Cell
        orientation: ClueOrientation
    }

    let init (cp:CluedPuzzle option) (s:Solution option) : (State * Cmd<_>) = 
        let myPuzzle = defaultArg cp (CluedPuzzle.generateBlank 1 1)
        let mySolution = defaultArg s (Solution.generateBlank myPuzzle.puzzle)
        Puzzle.checkSame myPuzzle.puzzle mySolution.puzzle // This may throw invalidArg
        {puzzle=myPuzzle; solution=mySolution; selected=(-1,-1); orientation=Across}, Cmd.none


    let update (msg: SolverMsg) (state: State) =
        match msg with
        | SolverMsg.SetPuzzle (cp, s) -> init cp s
        | SolverMsg.ToLobby -> {state with selected=(-1,-1)}, Cmd.ofMsg (ShellMsg.SetView LobbyView)
        | SolverMsg.ToLibrary -> {state with selected=(-1,-1)}, Cmd.ofMsg (ShellMsg.SetView LibraryView)
        | SolverMsg.SelectCell (row, col) -> 
            {state with selected=row,col}, Cmd.none
        | SolverMsg.MoveSelection direction ->
            let nextCell = 
                match state.selected with 
                | (row,col) -> 
                    match Puzzle.getNextCell state.solution.puzzle direction (row,col) with
                    | Some (nrow,ncol) -> nrow,ncol
                    | None -> row,col
            {state with selected=nextCell}, Cmd.none
        | SolverMsg.SetCell letter -> 
            match state.selected with
            | (row,col) -> 
                let newsol = Solution.setCell state.puzzle.puzzle state.solution (row,col) letter
                let moveCursorMsg = 
                    match state.orientation with
                    | ClueOrientation.Across -> SolverMsg.MoveSelection Rightwards
                    | ClueOrientation.Down -> SolverMsg.MoveSelection Downwards
                    |> ShellMsg.SolverMsg
                { state with solution=newsol}, Cmd.ofMsg moveCursorMsg
        | SolverMsg.ClearCell ->
            let row,col = state.selected
            let newsol = Solution.setCell state.puzzle.puzzle state.solution (row,col) Puzzle.EMPTY_CHAR
            { state with solution = newsol }, Cmd.none
        | ToggleOrientation -> 
            let nexto = if state.orientation=Across then Down else Across
            { state with orientation=nexto }, Cmd.none

    let cellKeyEventHandler (dispatch: SolverMsg -> unit) (keyEvt:KeyEventArgs) = 
        if keyEvt.Route = RoutingStrategies.Tunnel then // For some reason we were getting both tunnel and bubble events for alpha keys
            match keyEvt.Key with
            | Key.Space -> dispatch SolverMsg.ToggleOrientation
            | Key.Up -> Direction.Upwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Down -> Direction.Downwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Left -> Direction.Leftwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Right -> Direction.Rightwards |> SolverMsg.MoveSelection |> dispatch
            | Key.Back -> SolverMsg.ClearCell |> dispatch
            | Key.Delete -> SolverMsg.ClearCell |> dispatch 
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
                Button.onTapped (fun _ -> (row,col) |> SolverMsg.SelectCell |> dispatch) // Apparently SPACE key generates a click event but not a tap event
                Button.onGotFocus (fun _ -> (row,col) |> SolverMsg.SelectCell |> dispatch)
                Button.onKeyDown (fun keyEvt -> cellKeyEventHandler dispatch keyEvt)

                let r,c = state.selected
                if r=row && c=col then Button.background "Orange"
                Button.content(
                    DockPanel.create [
                        DockPanel.children [
                            match Puzzle.getCellNumber state.puzzle.puzzle (row,col) with
                            | Some num -> 
                                TextBlock.create [
                                    TextBlock.dock Dock.Top
                                    TextBlock.classes ["CellNumber"]
                                    TextBlock.text (string num)
                                ]
                            | None -> ()
                            TextBlock.create [
                                TextBlock.text (string letter)
                            ]
                        ]
                    ]
                )
            else
                Button.isEnabled false
                Button.background "White"
        ]

    let viewClue (state:State) (dispatch: SolverMsg -> unit) (o:ClueOrientation) index = 
        Grid.create [
            Grid.columnDefinitions "Auto,*"
            Grid.rowDefinitions "Auto"
            //Grid.onTapped (fun _ -> dispatch ) // Select clue
            Grid.children [
                TextBlock.create [ // Clue number
                    Grid.column 0
                    TextBlock.classes ["ClueNumber"]
                    TextBlock.text (
                        (CluedPuzzle.getClueNumber state.puzzle o index |> string) + ". "
                    )
                ]
                TextBlock.create [ // Clue text
                    Grid.column 1
                    TextBlock.textWrapping TextWrapping.Wrap
                    TextBlock.text (
                        match o with
                        | Across -> state.puzzle.across.[index]
                        | Down -> state.puzzle.down.[index]
                    )
                ]
            ]
        ]

    let view (state: State) (dispatch: SolverMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                // Header bar
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Right
                            Button.classes ["pretty"]
                            Button.content "Check Cell"
                        ]
                        TextBlock.create [
                            TextBlock.classes ["title"]
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.text "Title goes here!"
                        ]
                    ]
                ]

                // Navigation buttons
                DockPanel.create [
                    DockPanel.dock Dock.Bottom
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Left
                            Button.content "Library"
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLibrary)
                            Button.classes ["pretty"]
                        ]
                        Button.create [
                            Button.dock Dock.Right
                            Button.content "Exit to Lobby"
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLobby)
                            Button.classes ["pretty"]
                        ]
                        TextBlock.create [
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.classes ["subtitle"]
                            TextBlock.text (CluedPuzzle.getFormalClue state.puzzle state.selected state.orientation)
                        ]
                    ]
                ]
                Grid.create [
                    Grid.rowDefinitions "*"
                    Grid.columnDefinitions "*,Auto"
                    Grid.children [
                        // Puzzle Area
                        Grid.create [
                            Grid.column 0
                            Grid.columnDefinitions "*,Auto,*"
                            Grid.rowDefinitions "*,Auto,*"
                            Grid.children [
                                StackPanel.create [
                                    Grid.row 1
                                    Grid.column 1
                                    StackPanel.children [
                                        Grid.create [
                                            Grid.dock Dock.Right
                                            let rows = Puzzle.getRows state.puzzle.puzzle
                                            let cols = Puzzle.getCols state.puzzle.puzzle
                                            Grid.columnDefinitions ([for i in 0..(cols-1) -> "35"] |> String.concat ",")
                                            Grid.rowDefinitions ([for i in 0..(rows-1) -> "35"] |> String.concat ",")
                                            Grid.children [
                                                for i in 0..(rows-1) do
                                                    for j in 0..(cols-1) do
                                                        yield viewCell state dispatch i j (Puzzle.getCell state.solution.puzzle (i,j))
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]  
                        // Clue Area
                        ScrollViewer.create [ // TODO: Need a way to compress this horizontally - right now it is screwing up the resize behavior
                            Grid.column 1
                            ScrollViewer.horizontalAlignment HorizontalAlignment.Left
                            ScrollViewer.content (
                                Grid.create [
                                    Grid.columnDefinitions "*,*"
                                    Grid.rowDefinitions "Auto"
                                    Grid.children [
                                        StackPanel.create [
                                            Grid.column 0
                                            StackPanel.children [
                                                yield TextBlock.create [
                                                    TextBlock.text "Across"
                                                ]
                                                for index in [0..(state.puzzle.across.Length-1)] do
                                                    yield viewClue state dispatch Across index
                                            ]
                                        ]
                                        StackPanel.create [
                                            Grid.column 1
                                            StackPanel.children [
                                                yield TextBlock.create [
                                                    TextBlock.text "Down"
                                                ]
                                                for index in [0..(state.puzzle.down.Length-1)] do
                                                    yield viewClue state dispatch Down index
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]      
                    ]
                ]
            ]
        ]