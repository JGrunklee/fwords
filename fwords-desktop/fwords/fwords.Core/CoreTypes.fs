namespace fwords.Core

[<AutoOpen>]
module FwordsCoreTypes = 

    /// Enumerate the directions that we might want to move through Puzzles
    type Direction = | Upwards | Downwards | Leftwards | Rightwards

    /// Enumerate the orientations that clues can have
    type ClueOrientation = | Down | Across

    /// Enumerate some levels of difficulty
    type Difficulty = | Easy | Medium | Hard

    /// An fwords Puzzle is a 2D array of characters.
    /// It may represent a partial or complete solution to a crossword puzzle.
    type Puzzle = char[,]
        
    /// A Cell holds the coordinates/indices of a square in a Puzzle
    type Cell = int * int

    /// A CellChecker is a function that takes a puzzle and a cell and returns true or false
    type CellChecker = Puzzle -> Cell -> bool

    /// A CluedPuzzle is a Puzzle plus a pair of clue lists
    type CluedPuzzle = {
        puzzle: Puzzle
        across: string list     // The across clues
        down: string list       // The down clues
    }

    /// A Solution represents a partially-solved crossword puzzle
    type Solution = {
        puzzle: Puzzle        // The partial response
        progress: float         // Percentage of the puzzle completed correctly
    }

    /// Puzzle Information
    type PuzzleInfo = {
        name: string
        id: string
        level: Difficulty
        progress: float
    }

