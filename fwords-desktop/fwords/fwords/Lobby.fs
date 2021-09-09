namespace fwords

/// The lobby is the menu/welcome screen
module Lobby =
    open Elmish
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Media
    open Avalonia.FuncUI.DSL

    type State =
        { noop: bool } // There isn't really any state needed here

    type Difficulty = 
        | Easy
        | Medium
        | Hard

    type Msg = 
        | NewRandPuzzle of Difficulty
        | ResumePuzzle
        | ToLibrary
        | JoinOnline

    let init = { noop = false }, Cmd.none

    let update (msg: Msg) (state: State) =
        match msg with
        | NewRandPuzzle level -> () // Placeholder
        | ResumePuzzle -> () // Placeholder
        | ToLibrary -> () // Placeholder
        | JoinOnline -> () // Placeholder
        state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        DockPanel.create [ 
            DockPanel.horizontalAlignment HorizontalAlignment.Center
            DockPanel.verticalAlignment VerticalAlignment.Center
            DockPanel.children [ 
                StackPanel.create [ 
                    StackPanel.dock Dock.Top
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.classes [ "title" ]
                            TextBlock.textAlignment TextAlignment.Center
                            TextBlock.text ("fwords" )
                        ]
                        TextBlock.create [
                            TextBlock.classes [ "subtitle" ]
                            TextBlock.textAlignment TextAlignment.Center
                            TextBlock.text ("fun with puzzles and F#")
                        ]
                        StackPanel.create [
                            StackPanel.horizontalAlignment HorizontalAlignment.Center
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "new random puzzle: "
                                ]
                                Button.create [
                                    Button.content "Easy"
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Easy))
                                ]
                                Button.create [
                                    Button.content "Medium"
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Medium))
                                ]
                                Button.create [
                                    Button.content "Hard"
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Hard))
                                ]
                            ]
                        ]
                        Button.create [
                            Button.content "Library"
                            Button.onClick (fun _ -> dispatch ToLibrary)
                        ]
                        Button.create [
                            Button.content "Resume"
                            Button.onClick (fun _ -> dispatch ResumePuzzle)
                        ]
                        Button.create [
                            Button.content "Join Online"
                            Button.onClick (fun _ -> dispatch JoinOnline)
                        ]
                    ]
                ]
            ]
        ]
