namespace fwords

/// Global types used by fwords
[<AutoOpen>]
module Types = 

    /// Define difficulty
    type Difficulty = 
        | Easy
        | Medium
        | Hard

    /// Messages understood by Lobby module
    type LobbyMsg = 
        | NewRandPuzzle of Difficulty
        | ResumePuzzle
        | ToLibrary
        | JoinOnline

    /// Messages understood by the Library module
    type LibraryMsg =
        | LoadPuzzles
        | ToLobby
        | ToPuzzle of int // Index into the puzzzle list

    // Messages understood by the Solver module
    type SolverMsg = 
        | ToLobby
        | ToLibrary

    /// Available views
    type View = 
        | LobbyView
        | LibraryView
        | SolverView

    /// Messages understood by the Shell module
    type ShellMsg =
        | SetView of View
        | LobbyMsg of LobbyMsg
        | LibraryMsg of LibraryMsg
        | SolverMsg of SolverMsg