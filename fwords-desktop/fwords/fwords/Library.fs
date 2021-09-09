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

    type Msg =
        | LoadPuzzles
        | ToLobby
        | ToPuzzle of int // Index into the puzzzle list

    let update (msg: Msg) (state: State) =
        match msg with
        | LoadPuzzles -> () // TODO
        | ToLobby -> ()
        | ToPuzzle index -> () // TODO
        state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        DockPanel.create [
            DockPanel.children [
                // TODO: scrollable list of puzzles
                Button.create [
                    Button.dock Dock.Bottom
                    Button.onClick (fun _ -> dispatch ToLobby)
                    Button.content "back"
                ]
            ]
        ]
