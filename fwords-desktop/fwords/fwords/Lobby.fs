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

    open fwords.Core

    type State = { 
        noop: bool
    }

    let init = { noop = false }, Cmd.none

    let update (msg: LobbyMsg) (state: State) =
        match msg with
        | LobbyMsg.ToLibrary -> 
            state, Cmd.ofMsg (ShellMsg.SetView LibraryView)
        | NewRandPuzzle level -> state, Cmd.ofMsg (ShellMsg.LobbyMsg LobbyMsg.ToLibrary) // Placeholder
        | ResumePuzzle -> state, Cmd.none // Placeholder
        | JoinOnline -> state, Cmd.none // Placeholder
        | LobbyMsg.SetTheme theme -> state, theme |> ShellMsg.SetTheme |> Cmd.ofMsg

    let view (state: State) (dispatch: LobbyMsg -> unit) =
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
                            TextBlock.classes [ "subtitle"; "flashy"]
                            TextBlock.textAlignment TextAlignment.Center
                            TextBlock.text ("fun with puzzles and F#")
                        ]
                        StackPanel.create [
                            StackPanel.horizontalAlignment HorizontalAlignment.Center
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.text "new random puzzle: "
                                ]
                                Button.create [
                                    Button.content "Easy"
                                    Button.classes ["pretty"]
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Easy))
                                ]
                                Button.create [
                                    Button.content "Medium"
                                    Button.classes ["pretty"]
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Medium))
                                ]
                                Button.create [
                                    Button.content "Hard"
                                    Button.classes ["pretty"]
                                    Button.onClick (fun _ -> dispatch (NewRandPuzzle Hard))
                                ]
                            ]
                        ]
                        Button.create [
                            Button.content "Library"
                            Button.classes ["pretty"]
                            Button.onClick (fun _ -> dispatch LobbyMsg.ToLibrary)
                        ]
                        Button.create [
                            Button.content "Resume"
                            Button.classes ["pretty"]
                            Button.onClick (fun _ -> dispatch ResumePuzzle)
                        ]
                        Button.create [
                            Button.content "Join Online"
                            Button.classes ["pretty"]
                            Button.onClick (fun _ -> dispatch JoinOnline)
                        ]
                        Button.create [
                            Button.content "Light"
                            Button.classes["pretty"]
                            Button.onClick (fun _ -> FwordsTheme.FwordsLight |> LobbyMsg.SetTheme |> dispatch)
                        ]
                        Button.create [
                            Button.content "Dark"
                            Button.classes["pretty"]
                            Button.onClick (fun _ -> FwordsTheme.FwordsDark |> LobbyMsg.SetTheme |> dispatch)
                        ]
                    ]
                ]
            ]
        ]
