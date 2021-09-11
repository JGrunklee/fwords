namespace fwords

/// This is the place where you can view all of the available Puzzles
/// and select one to work on
module Library =
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Components
    open Avalonia.FuncUI.Elmish
    open Elmish

    open fwords.Core

    type State = {
        puzzles: Puzzle list // TODO: probably actually make this a list of info about the Puzzles, not the Puzzles themselves
    }

    let init = { puzzles = [] }, Cmd.none

    let update (msg: LibraryMsg) (state: State) =
        match msg with
        | LoadPuzzles -> state, Cmd.none // TODO
        | LibraryMsg.ToLobby -> state, Cmd.ofMsg (SetView LobbyView)
        | ToPuzzle index -> state, Cmd.ofMsg (SetView SolverView)

    let view (state: State) (dispatch: LibraryMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                // TODO: scrollable list of puzzles
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch LibraryMsg.ToLobby)
                    Button.content "back"
                ]
                Button.create [
                    Button.dock Dock.Top
                    Button.onClick (fun _ -> dispatch (ToPuzzle 0))
                    Button.content "Go To Puzzle"
                ]
            ]
        ]
