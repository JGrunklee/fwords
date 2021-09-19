namespace fwords

/// Global types used by fwords
[<AutoOpen>]
module Types = 
    open fwords.Core

    /// Define difficulty
    type Difficulty = 
        | Easy
        | Medium
        | Hard

    type ClueOrientation = 
        | Down
        | Across

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
        | ToPuzzle
        | SelectPuzzle of string 

    /// Messages understood by the Solver module
    type SolverMsg = 
        | ToLobby
        | ToLibrary
        | SelectCell of Cell
        | MoveSelection of Direction
        | SetCell of char
        | ToggleOrientation
        | SetPuzzle of CluedPuzzle option * Solution option

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


