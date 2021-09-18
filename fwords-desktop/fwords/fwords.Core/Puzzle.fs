namespace fwords.Core

type Direction =
    | Upwards
    | Downwards
    | Leftwards
    | Rightwards

// Add helpful functions to F# standard Array2D module
module Array2D = 
    // Split a 2D array into a list of its columns
    let columwise myarray = 
        let numCols = Array2D.length2 myarray
        [for col in 0..numCols-1 -> myarray.[*,col]]

    let sumBy (projection:'T -> 'U) myarray = 
        myarray
        |> Seq.cast<'T>
        |> Seq.sumBy projection

    let reduce (reduction: 'T->'T->'T) myarray =
        myarray
        |> Seq.cast<'T>
        |> Seq.reduce reduction
        

/// An fwords Puzzle is a 2D array of characters.
/// It may represent a partial or complete solution to a crossword puzzle.
type Puzzle = { cells: char[,] }
module Puzzle = 

    /// A character that represents an empty square
    [<Literal>]
    let EMPTY_CHAR = ' '

    /// A character that represents a filled square
    [<Literal>]
    let FILL_CHAR = '#'

    /// Create a puzzle from a 2D char array
    let fromArray (myArray: char[,]) : Puzzle =  {cells = myArray}

    /// Generate an empty puzzle
    let generateBlank rows cols : Puzzle = 
        { cells = Array2D.init rows cols (fun _ _ -> EMPTY_CHAR) }

    /// Get the value of a cell
    let getCell (p:Puzzle) row col : char = 
        p.cells.[row,col]

    /// Return the underlying array
    let getCells (p:Puzzle) = p.cells

    /// Set a cell in a Puzzle by creating a new Puzzle with the cell value changed.
    /// Trying to treat arrays as if they are immutable
    let setCell (p:Puzzle) row col value : Puzzle = 
        let q = p.cells |> Array2D.copy
        Array2D.set q row col value
        {cells = q }

    /// Get the number of rows
    let getRows (p:Puzzle) = Array2D.length1 p.cells

    /// Get the number of columns
    let getCols (p:Puzzle) = Array2D.length2 p.cells

    /// Check a guess against the given cell
    /// Returns true for a correct guess
    let checkCell (p:Puzzle) row col guess : bool = 
        p.cells.[row,col] = guess

    /// Compute which across clue applies to the given cell
    let rec getAcrossClueIndex (p:Puzzle) row col = 
        // Calculate nth across clue in current row
        let acc =
            p.cells.[row,0..col]
            |> Array.pairwise
            |> Array.filter (fun pair -> 
                match pair with
                | (previous, current) -> (not (current = FILL_CHAR)) && (previous = FILL_CHAR))
            |> Array.length
            |> (+) (if p.cells.[row,0] = FILL_CHAR then 0 else 1)
        if row=0 then
            // No more rows left. Return result and zero-index
            acc - 1
        else
            // Compute number of accross clues in entire next row above.
            acc + (getAcrossClueIndex p (row-1) <| getCols p)

    // Count down clues 'anchored' before (left to right, top to bottom) a cell 
    let rec countDownCluesToCell (p:Puzzle) row col = 
        // Count cells in row 0 that 'anchor' a down clue
        let countDownCluesToColRow0 (p:Puzzle) col = 
            p.cells.[0,0..col]
            |> Array.filter (fun c -> not (c = FILL_CHAR))
            |> Array.length
        // Count cells in row n!=0 that 'anchor' a down clue
        let countDownCluesToColRowN (p:Puzzle) row col = 
            p.cells.[(row-1)..row,0..col]
            |> Array2D.columwise
            |> List.filter ( fun pair -> (pair.[0] = FILL_CHAR) && (not (pair.[1] = FILL_CHAR)))
            |> List.length

        // Count the number of down clues in this row
        let acc = 
            if row = 0 then countDownCluesToColRow0 p col
            else countDownCluesToColRowN p row col

        if row=0 then 
            // No more rows left. Return result and zero-index
            acc - 1
        else
            // Compute number of down clues anchored in the row above
            let lastCol = (p |> getCols) - 1
            acc + (countDownCluesToCell p (row-1) lastCol)

    // Get the cell that 'anchors' the down clue for that cell
    let rec getDownClueAnchor (p:Puzzle) row col = 
        if row = 0 then (row, col)
        elif (not (p.cells.[row, col] = FILL_CHAR)) && (p.cells.[row-1, col] = FILL_CHAR) then (row, col)
        else getDownClueAnchor p (row-1) col

    /// Get the index of the down clue for a given cell
    let getDownClueIndex (p:Puzzle) row col = 
        let (anchorRow, anchorCol) = getDownClueAnchor p row col
        countDownCluesToCell p anchorRow anchorCol

    /// Compare one Puzzle against another, replacing cells that differ with the empty character
    let deleteMistakes (answer:Puzzle) (guess:Puzzle) : Puzzle = 
        guess.cells
        |> Array2D.mapi (fun row col letter ->
            if checkCell answer row col letter then letter
            else EMPTY_CHAR)
        |> fromArray

    let computeProgress (answer:Puzzle) (guess:Puzzle) : float =
        let correctCells = 
            guess
            |> deleteMistakes answer
            |> getCells
        let correctCount = Array2D.sumBy (fun letter -> if (letter=FILL_CHAR || letter=EMPTY_CHAR) then 0 else 1) correctCells
        let totalCount = Array2D.sumBy (fun letter -> if not (letter=FILL_CHAR) then 1 else 0) answer.cells
        float(correctCount) / float(totalCount)

    /// Return true if the puzzles have the same number of rows and columns
    /// and the same pattern of filled spaces
    let checkSame (p:Puzzle) (q:Puzzle) : unit = 
        if not(((getRows p)=(getRows q)) && (getCols p)=(getCols q)) then 
            invalidArg "q" "Puzzle.checkSame: Puzzles have different dimensions."
        else
            let same = 
                p.cells
                |> Array2D.mapi (fun row col letter ->
                    let pFilled = (letter=FILL_CHAR)
                    let qFilled = checkCell q row col FILL_CHAR
                    (pFilled=qFilled)) // false if there is a filled cell in one but not the other
                |> Array2D.reduce (&&) // logical AND - we want to know if any value is false
            if not same then invalidArg "q" "Puzzles.checkSame: Puzzles are different."

    let rec walkTo (p:Puzzle) predicate (d:Direction) (row:int) (col:int) : (int*int) option =
        if p.cells.[row,col] |> predicate then Some (row,col)
        else 
            match d with
            | Upwards -> if row>0 then walkTo p predicate d (row-1) col else None
            | Leftwards -> if col>0 then walkTo p predicate d row (col-1) else None
            | Downwards ->
                let lastRow = (getRows p) - 1
                if row<lastRow then walkTo p predicate d (row+1) col else None
            | Rightwards ->
                let lastCol = (getCols p) - 1
                if col<lastCol then walkTo p predicate d row (col+1) else None
        
    let getNextCell (p:Puzzle) (d:Direction) row col : (int*int) option=
        try
            let predicate = fun c -> c <> FILL_CHAR
            match d with
            | Upwards -> walkTo p predicate d (row-1) col
            | Leftwards -> walkTo p predicate d row (col-1)
            | Downwards -> walkTo p predicate d (row+1) col
            | Rightwards -> walkTo p predicate d row (col+1)
        with
        | _ -> None


/// A CluedPuzzle is a Puzzle plus a pair of clue lists
type CluedPuzzle = {
    puzzle: Puzzle
    across: string list     // The across clues
    down: string list       // The down clues
}
module CluedPuzzle = 
    /// Create an empty CluedPuzzle
    let generateBlank rows cols : CluedPuzzle = {
        puzzle = Puzzle.generateBlank rows cols
        across = []
        down = []
    }

    /// Get across clue for a given cell
    let getAcrossClue (cp:CluedPuzzle) row col = 
        if cp.puzzle.cells.[row,col] = Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.across.[Puzzle.getAcrossClueIndex cp.puzzle row col]

    /// Get the down clue for a given cell
    let getDownClue (cp:CluedPuzzle) row col = 
        if cp.puzzle.cells.[row,col] = Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.down.[Puzzle.getDownClueIndex cp.puzzle row col]

    let setCell (p:CluedPuzzle) row col value : CluedPuzzle = { p with puzzle = Puzzle.setCell p.puzzle row col value }        

/// A Solution represents a partially-solved crossword puzzle
type Solution = {
    puzzle: Puzzle        // The partial response
    progress: float         // Percentage of the puzzle completed correctly
}
module Solution = 
    /// Create a brand new (empty) solution
    let generateBlank (p:Puzzle) : Solution = {
        puzzle = 
            p.cells
            |> Array2D.map (fun letter -> 
                if letter=Puzzle.FILL_CHAR then letter
                else Puzzle.EMPTY_CHAR)
            |> Puzzle.fromArray
        progress = 0.0
        }

    /// Set a cell in a Solution
    let setCell (p:Puzzle) (s:Solution) row col letter : Solution =
        let updated = Puzzle.setCell s.puzzle row col letter
        {
            puzzle = updated
            progress = (Puzzle.computeProgress p updated)
        }

type Difficulty = 
    | Easy
    | Medium
    | Hard

type PuzzleInfo = {
    name: string
    id: string
    level: Difficulty
    progress: float
}



    