namespace fwords

/// This is a view that lets you solve an existing Puzzle
module Solver = 
    open Elmish
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Input
    open Avalonia.FuncUI.DSL

    open fwords.Core

    type State = {
        puzzle: CluedPuzzle
        solution: Solution
        selected: int*int
    }

    let init (cp:CluedPuzzle option) (s:Solution option) : (State * Cmd<_>) = 
        let myPuzzle = defaultArg cp (CluedPuzzle.generateBlank 1 1)
        let mySolution = defaultArg s (Solution.generateBlank myPuzzle.puzzle)
        Puzzle.checkSame myPuzzle.puzzle mySolution.puzzle // This may throw invalidArg
        {puzzle=myPuzzle; solution=mySolution; selected=(-1,-1)}, Cmd.none


    let update (msg: SolverMsg) (state: State) =
        match msg with
        | SolverMsg.ToLobby -> state, Cmd.ofMsg (ShellMsg.SetView LobbyView)
        | SolverMsg.ToLibrary -> state, Cmd.ofMsg (ShellMsg.SetView LibraryView)
        | SolverMsg.SelectCell (row, col) -> 
            {state with selected=row,col}, Cmd.none
        | SolverMsg.SetCell (row,col,letter) -> { state with puzzle=CluedPuzzle.setCell state.puzzle row col letter}, Cmd.none

    let cellKeyEventHandler (dispatch: SolverMsg -> unit) (keyEvt:KeyEventArgs) row col  = 
        let letter = char keyEvt.Key
        if System.Char.IsLetter(letter) then (row,col,letter) |> SolverMsg.SetCell |> dispatch

    //let viewCell (state: State) (dispatch: SolverMsg -> unit) row col letter = 
    let viewCell (state:State) (dispatch: SolverMsg -> unit) row col letter = 
        TextBlock.create [
            Grid.row row
            Grid.column col
            if letter <> Puzzle.FILL_CHAR then
                TextBlock.text (string letter)
                TextBlock.onPointerPressed (fun _ -> (row,col) |> SolverMsg.SelectCell |> dispatch)
                TextBlock.onKeyDown (fun keyEvt -> cellKeyEventHandler dispatch keyEvt row col)
                match state.selected with
                | (r, c) -> if r=row && c=col then TextBlock.background "Orange"
            else
                TextBlock.background "White"
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
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLibrary)
                            Button.content "Library"
                        ]
                        Button.create [
                            Grid.column 2
                            Button.onClick (fun _ -> dispatch SolverMsg.ToLobby)
                            Button.content "Exit to Lobby"
                        ]
                    ]
                ]
            ]
        ]