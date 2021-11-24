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
    open Avalonia.Media

    open fwords.Core

    type State = {
        selected: string
        puzzles: PuzzleInfo list // TODO: probably actually make this a list of info about the Puzzles, not the Puzzles themselves
    }

    //let init = { puzzles = [] }, Cmd.none

    let init = { // Some dummy data for visualization
        selected = ""
        puzzles = [
            {name="jeff"; id="123"; level=Difficulty.Easy; progress=0.0}
            {name="NYT"; id="456"; level=Difficulty.Easy; progress=0.75}
            {name="Star Tribune"; id="start_tribune.pzl"; level=Difficulty.Hard; progress=0.43}
            {name="Jackson's book"; id="jacksons_book.pzl"; level=Difficulty.Medium; progress=0.12}
                ]}, Cmd.none

    let update (msg: LibraryMsg) (state: State) =
        match msg with
        | LoadPuzzles -> state, Cmd.none // TODO
        | LibraryMsg.ToLobby -> state, Cmd.ofMsg (SetView LobbyView)
        | ToPuzzle -> 
            try 
                let selectedInfo =
                    state.puzzles
                    |> List.find (fun pi -> pi.id = state.selected) // May throw KeyNotFound if no entry selected
                let myPuzzle = 
                    // Actual version will do: PuzzleInfo.loadFromId selectedInfo.id
                    if selectedInfo.id = "123" then
                        SampleData.myCluedPuzzle
                    else SampleDatatwo.myCluedPuzzle
                state, Cmd.batch [
                (Some(myPuzzle),None) |> SolverMsg.SetPuzzle |> ShellMsg.SolverMsg |> Cmd.ofMsg
                Cmd.ofMsg (SetView SolverView)
                ]
            with | _ -> state, Cmd.none
        | SelectPuzzle id -> 
            {state with selected = id}, Cmd.none


    //let puzzleSelectorHandler (dispatch: SolverMsg -> unit) TODO

    let libraryEntry (state: State) (dispatch: LibraryMsg -> unit) (pi:PuzzleInfo) = 
        Button.create [
            Button.onTapped (fun _ -> dispatch (LibraryMsg.SelectPuzzle pi.id))
            ["wide" ; "list"] @ if pi.id = state.selected then ["selected"] else []
            |> Button.classes
            
            Button.content (
                Grid.create [
                    Grid.columnDefinitions "*, *, *" // Three equally-sized columns
                    Grid.rowDefinitions "Auto" // One automatically-sized row
                    Grid.children [
                        TextBlock.create [
                            Grid.column 0
                            TextBlock.text pi.name
                            TextBlock.classes["center" ; "list"]
                        ]
                        TextBlock.create [ 
                            Grid.column 1
                            TextBlock.text (string pi.level)
                            TextBlock.classes["center" ; "list"]
                        ]
                        TextBlock.create [
                            Grid.column 2
                            TextBlock.text (
                                (pi.progress
                                    |> (*) 100.0
                                    |> string)
                                + " %"
                            )
                            TextBlock.classes["center" ; "list"]
                        ]
                    ]
                ]
            )
        ]
        

    let view (state: State) (dispatch: LibraryMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                // Header bar
                DockPanel.create [
                    DockPanel.dock Dock.Top
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Right
                            Button.classes ["pretty"]
                            Button.content "Apply Filter"
                        ]
                        Button.create [
                            Button.dock Dock.Left
                            Button.classes ["pretty"]
                            Button.content "Refresh List"
                        ]
                        TextBox.create [
                            TextBox.text "search..."
                        ]
                    ]
                ]

                // Navigation buttons
                DockPanel.create [
                    DockPanel.dock Dock.Bottom
                    DockPanel.children [
                        Button.create [
                            DockPanel.dock Dock.Left
                            Button.content "back"
                            Button.onClick (fun _ -> dispatch LibraryMsg.ToLobby)
                            Button.classes ["pretty"]
                        ]
                        Button.create [
                            DockPanel.dock Dock.Right
                            Button.content "Solve!"
                            Button.onClick (fun _ -> dispatch ToPuzzle)
                            Button.isEnabled (state.selected <> "")
                            Button.classes ["list"; "selected"]
                        ]
                        // Placeholder to keep the buttons small
                        TextBlock.create [
                            TextBlock.text ""
                        ]
                    ]
                ]

                // List of available puzzles
                Grid.create [
                    Grid.columnDefinitions "*, *, *, *, *"
                    Grid.rowDefinitions "Auto, *"
                    Grid.children [
                        // Header row
                        TextBlock.create [
                            Grid.column 1
                            Grid.row 0
                            TextBlock.text ("Title")
                            TextBlock.classes["title"; "center"]
                        ]
                        TextBlock.create [
                            Grid.column 2
                            Grid.row 0
                            TextBlock.text ("Difficulty")
                            TextBlock.classes["title"; "center"]
                        ]
                        TextBlock.create [
                            Grid.column 3
                            Grid.row 0
                            TextBlock.text ("Progress")
                            TextBlock.classes["title"; "center"]
                        ]
                        
                        // Table
                        ScrollViewer.create [
                            Grid.column 1
                            Grid.columnSpan 3
                            Grid.row 1
                            ScrollViewer.content ( 
                                StackPanel.create [
                                    StackPanel.horizontalAlignment HorizontalAlignment.Stretch
                                    StackPanel.children [
                                        for item in state.puzzles do
                                            yield libraryEntry state dispatch item
                                    ]
                                ]
                            ) // yes, this is the only one that needs to be ')' instead of ']'
                        ]
                    ]
                ]
            ]
        ]

