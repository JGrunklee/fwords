namespace fwords

/// This is a view that lets you solve an existing Puzzle
module Solver = 
    open Elmish
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Media
    open Avalonia.FuncUI.DSL

    open fwords.Core

    type State = {
        puzzle: CluedPuzzle
        solution: Solution
    }

    let init (cp:CluedPuzzle option) (s:Solution option) : (State * Cmd<_>) = 
        let myPuzzle = defaultArg cp (CluedPuzzle.generateBlank 1 1)
        let mySolution = defaultArg s (Solution.generateBlank myPuzzle.puzzle)
        Puzzle.checkSame myPuzzle.puzzle mySolution.puzzle // This may throw invalidArg
        {puzzle=myPuzzle; solution=mySolution}, Cmd.none


    let update (msg: SolverMsg) (state: State) =
        match msg with
        | SolverMsg.ToLobby -> state, Cmd.ofMsg (ShellMsg.SetView LobbyView)
        | SolverMsg.ToLibrary -> state, Cmd.ofMsg (ShellMsg.SetView LibraryView)

    let view (state: State) (dispatch: SolverMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                Button.create [
                    Button.dock Dock.Left
                    Button.onClick (fun _ -> dispatch SolverMsg.ToLibrary)
                    Button.content "Library"
                ]
                Button.create [
                    Button.dock Dock.Right
                    Button.onClick (fun _ -> dispatch SolverMsg.ToLobby)
                    Button.content "Lobby"
                ]
            ]
        ]