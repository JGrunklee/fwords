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

    type State = {
        currentView: View
        lobbyState: Lobby.State
        libraryState: Library.State
        solverState: Solver.State
    }

    let init =
        let lobbyState, lbyCmd = Lobby.init
        let libraryState, libCmd = Library.init
        let solverState, slvCmd = Solver.init None None
        {
            currentView = LobbyView
            lobbyState = lobbyState
            libraryState = libraryState
            solverState = solverState
        },
        /// If your children controls don't emit any commands
        /// in the init function, you can just return Cmd.none
        /// otherwise, you can use a batch operation on all of them
        /// you can add more init commands as you need
        Cmd.batch [ lbyCmd; libCmd; slvCmd ]

    let update (msg: ShellMsg) (state: State): State * Cmd<_> =
        // Call child update(s)
        match msg with
        | SetView view ->
            { state with currentView = view }, Cmd.none
        | LobbyMsg lbyMsg ->
            let newLbyState, cmd =
                Lobby.update lbyMsg state.lobbyState
            { state with lobbyState = newLbyState }, cmd // map the message to LobbyMsg
        | LibraryMsg libMsg -> 
            let newLibState, cmd = 
                Library.update libMsg state.libraryState
            { state with libraryState = newLibState }, cmd // map the message to LibraryMsg
        | SolverMsg slvMsg -> 
            let newSlvState, cmd = 
                Solver.update slvMsg state.solverState
            { state with solverState = newSlvState}, cmd

    let view (state: State) (dispatch: ShellMsg -> unit) =
        DockPanel.create [
            DockPanel.verticalAlignment VerticalAlignment.Center
            DockPanel.children [
                StackPanel.create [
                    StackPanel.children [
                        match state.currentView with
                        | LobbyView -> (Lobby.view state.lobbyState (LobbyMsg >> dispatch))
                        | LibraryView -> (Library.view state.libraryState (LibraryMsg >> dispatch))
                        | SolverView -> (Solver.view state.solverState (SolverMsg >> dispatch))
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
