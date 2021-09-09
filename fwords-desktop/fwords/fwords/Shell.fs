namespace fwords

/// This is the main module of your application
/// here you handle all of your child pages as well as their
/// messages and their updates, useful to update multiple parts
/// of your application, Please refer to the `view` function
/// to see how to handle different kinds of "*child*" controls
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish
    open Avalonia.Layout

    /// union of available views
    type views = 
        | LobbyView
        | LibraryView

    type State = {
        currentView: views
        lobbyState: Lobby.State
        libraryState: Library.State
    }

    type Msg =
        | LobbyMsg of Lobby.Msg
        | LibraryMsg of Library.Msg

    let init =
        let lobbyState, lbyCmd = Lobby.init
        let libraryState, libCmd = Library.init
        {
            currentView = LobbyView
            lobbyState = lobbyState
            libraryState = libraryState
        },
        /// If your children controls don't emit any commands
        /// in the init function, you can just return Cmd.none
        /// otherwise, you can use a batch operation on all of them
        /// you can add more init commands as you need
        Cmd.batch [ lbyCmd; libCmd ]

    let update (msg: Msg) (state: State): State * Cmd<_> =
        // Call child update(s)
        match msg with
        | LobbyMsg lbyMsg ->
            let newLbyState, cmd =
                Lobby.update lbyMsg state.lobbyState
            let nextView = 
                if lbyMsg = Lobby.ToLibrary then LibraryView
                else state.currentView
            { state with 
                currentView = nextView
                lobbyState = newLbyState
            }, Cmd.map LobbyMsg cmd // map the message to LobbyMsg
            
        | LibraryMsg libMsg -> 
            let newLibState, cmd = 
                Library.update libMsg state.libraryState
            let nextView = 
                if libMsg = Library.ToLobby then LobbyView
                else state.currentView
            { state with 
                currentView = nextView
                libraryState = newLibState
            }, Cmd.map LibraryMsg cmd // map the message to LibraryMsg

    let view (state: State) (dispatch) =
        DockPanel.create [
            DockPanel.verticalAlignment VerticalAlignment.Center
            DockPanel.children [
                StackPanel.create [
                    StackPanel.children [
                        match state.currentView with
                        | LobbyView -> (Lobby.view state.lobbyState (LobbyMsg >> dispatch))
                        | LibraryView -> (Library.view state.libraryState (LibraryMsg >> dispatch))
                    ]
                        
                ]
            ]
        ]
                

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "Quickstart"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
