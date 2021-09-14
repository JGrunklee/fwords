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
        selected: string
        puzzles: PuzzleInfo list // TODO: probably actually make this a list of info about the Puzzles, not the Puzzles themselves
    }

    //let init = { puzzles = [] }, Cmd.none

    let init = { // Some dummy data for visualization
        selected = ""
        puzzles = [
            {name="jeff"; id="123"; level=0; progress=0.0}
            {name="NYT"; id="456"; level=1; progress=0.75}
            {name="Star Tribune"; id="17"; level=2; progress=0.43}
            {name="Jackson's book"; id="a string"; level=4; progress=0.12}
                ]}, Cmd.none

    let update (msg: LibraryMsg) (state: State) =
        match msg with
        | LoadPuzzles -> state, Cmd.none // TODO
        | LibraryMsg.ToLobby -> state, Cmd.ofMsg (SetView LobbyView)
        | ToPuzzle index -> state, Cmd.ofMsg (SetView SolverView)
        | SelectPuzzle id -> {state with selected = id}, Cmd.none

    let libraryEntry (state: State) (dispatch: LibraryMsg -> unit) (pi:PuzzleInfo) = 
        Grid.create [
            Grid.columnDefinitions "*, *, *" // Three equally-sized columns
            Grid.rowDefinitions "Auto" // One automatically-sized row
            Grid.onTapped (fun _ -> dispatch (LibraryMsg.SelectPuzzle pi.id))
            Grid.children [
                TextBlock.create [
                    Grid.column 0
                    TextBlock.text (
                        if pi.id = state.selected then  pi.name + " - selected" 
                        else pi.name )
                ]
                TextBlock.create [
                    Grid.column 1
                    TextBlock.text (string pi.level)
                ]
                TextBlock.create [
                    Grid.column 2
                    TextBlock.text ( pi.progress
                        |> (*) 100.0
                        |> string
                        |> (+) "%"
                    )
                ]
            ]
        ]

    let view (state: State) (dispatch: LibraryMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                // List of available puzzles
                ScrollViewer.create [
                    ScrollViewer.dock Dock.Top
                    ScrollViewer.content ( 
                        StackPanel.create [
                            StackPanel.horizontalAlignment HorizontalAlignment.Stretch
                            StackPanel.verticalAlignment VerticalAlignment.Stretch
                            StackPanel.children [ 
                                for item in state.puzzles do
                                    yield libraryEntry state dispatch item
                            ]
                        ]
                    ) // yes, this is the only one that needs to be ')' instead of ']'
                ]

                // Navigation buttons
                Grid.create [
                    Grid.dock Dock.Bottom
                    Grid.verticalAlignment VerticalAlignment.Bottom
                    Grid.columnDefinitions "Auto, *, Auto"
                    Grid.rowDefinitions "Auto"
                    Grid.children [
                        Button.create [
                            Grid.column 0
                            Button.content "back"
                            Button.onClick (fun _ -> dispatch LibraryMsg.ToLobby)
                            Button.classes ["pretty"]
                        ]
                        Button.create [
                            Grid.column 2
                            Button.content "Solve!"
                            Button.onClick (fun _ -> dispatch (ToPuzzle state.selected))
                            Button.classes ["pretty"]
                        ]
                    ]
                ]
            ]
        ]
