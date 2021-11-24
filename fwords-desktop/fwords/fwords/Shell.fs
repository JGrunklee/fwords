namespace fwords

/// This is the main module of the application. 
/// Here we handle all of the child pages as well as their messages and updates.
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Elmish

    open fwords.Core

    type State = {
        host: Styling.Styles
        currentView: View
        lobbyState: Lobby.State
        libraryState: Library.State
        solverState: Solver.State
    }

    /// Handle Shell events or call child update functions
    let update (msg: ShellMsg) (state: State): State * Cmd<_> =
        match msg with
        | SetTheme theme ->
            match theme with
            | FwordsTheme.FwordsDark -> state.host.Load "avares://fwords/StylesDark.xaml"
            | FwordsTheme.FwordsLight -> state.host.Load "avares://fwords/StylesLight.xaml"
            state, Cmd.none
        | SetView view -> { state with currentView = view }, Cmd.none
        | LobbyMsg lbyMsg ->
            let newLbyState, cmd = Lobby.update lbyMsg state.lobbyState
            { state with lobbyState = newLbyState }, cmd
        | LibraryMsg libMsg -> 
            let newLibState, cmd = Library.update libMsg state.libraryState
            { state with libraryState = newLibState }, cmd
        | SolverMsg slvMsg -> 
            let newSlvState, cmd = Solver.update slvMsg state.solverState
            { state with solverState = newSlvState}, cmd

    /// Call view function for current child
    let view (state: State) (dispatch: ShellMsg -> unit) =
        DockPanel.create [
            DockPanel.children [
                match state.currentView with
                | LobbyView -> (Lobby.view state.lobbyState (LobbyMsg >> dispatch))
                | LibraryView -> (Library.view state.libraryState (LibraryMsg >> dispatch))
                | SolverView -> (Solver.view state.solverState (SolverMsg >> dispatch))
            ]
        ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()

        /// Shell init is declared here for access to MainWindow styles
        /// for dynamic theme loading
        let init =
            // initialize all child views
            let lobbyState, lbyCmd = Lobby.init
            let libraryState, libCmd = Library.init
            let solverState, slvCmd = Solver.init (Some SampleData.myCluedPuzzle) (Some SampleData.myEmptySolution)
            {
                host = base.Styles
                currentView = View.LobbyView
                lobbyState = lobbyState
                libraryState = libraryState
                solverState = solverState
            },
            // The initial commands are any child commands,
            // plus a command to set the default theme
            Cmd.batch [ lbyCmd; libCmd; slvCmd; 
                FwordsTheme.FwordsDark |> ShellMsg.SetTheme |> Cmd.ofMsg]

        do // Run the program
            base.Title <- "fwords 0.0"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 400.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram (fun () -> init) update view
            |> Program.withHost this
            |> Program.run
