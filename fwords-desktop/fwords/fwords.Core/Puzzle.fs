namespace fwords.Core

module Util = 
    // Split a 2D array into a list of its columns
    let Array2DColumwise myarray = 
        let numCols = Array2D.length2 myarray
        [for col in 0..numCols-1 -> myarray.[*,col]]

// An fwords Puzzle is a 2D array of characters.
// It may represent a partial or complete solution to a crossword puzzle.
type Puzzle = { answer: char[,] }
module Puzzle = 

    // A character that represents an empty square
    [<Literal>]
    let EMPTY_CHAR = ' '

    // A character that represents a filled square
    [<Literal>]
    let FILL_CHAR = '#'

    // Generate an empty puzzle
    let generateBlank rows cols : Puzzle = 
        { answer = Array2D.init rows cols (fun _ _ -> EMPTY_CHAR) }

    // Get the value of a cell
    let getCell (p:Puzzle) row col : char = 
        p.answer.[row,col]

    // Set a cell in a Puzzle by creating a new Puzzle with the cell value changed.
    // Trying to treat arrays as if they are immutable
    let setCell (p:Puzzle) row col value : Puzzle = 
        let q = p.answer |> Array2D.copy
        Array2D.set q row col value
        {answer = q }

    // Check a guess against the given cell
    let checkCell (p:Puzzle) row col guess : bool = 
        p.answer.[row,col] = guess

    // Compute which across clue applies to the given cell
    let rec getAcrossClueIndex (p:Puzzle) row col = 
        // Calculate nth across clue in current row
        let acc =
            p.answer.[row,0..col]
            |> Array.pairwise
            |> Array.filter (fun pair -> 
                match pair with
                | (previous, current) -> (not (current = FILL_CHAR)) && (previous = FILL_CHAR))
            |> Array.length
            |> (+) (if p.answer.[row,0] = FILL_CHAR then 0 else 1)
        if row=0 then
            // No more rows left. Return result and zero-index
            acc - 1
        else
            // Compute number of accross clues in entire next row above.
            acc + (getAcrossClueIndex p (row-1) <| Array2D.length2 p.answer)

    // Count down clues 'anchored' before (left to right, top to bottom) a cell 
    let rec countDownCluesToCell (p:Puzzle) row col = 
        // Count cells in row 0 that 'anchor' a down clue
        let countDownCluesToColRow0 (p:Puzzle) col = 
            p.answer.[0,0..col]
            |> Array.filter (fun c -> not (c = FILL_CHAR))
            |> Array.length
        // Count cells in row n!=0 that 'anchor' a down clue
        let countDownCluesToColRowN (p:Puzzle) row col = 
            p.answer.[(row-1)..row,0..col]
            |> Util.Array2DColumwise
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
            let lastCol = (Array2D.length2 p.answer) - 1
            acc + (countDownCluesToCell p (row-1) lastCol)

    // Get the cell that 'anchors' the down clue for that cell
    let rec getDownClueAnchor (p:Puzzle) row col = 
        if row = 0 then (row, col)
        elif (not (p.answer.[row, col] = FILL_CHAR)) && (p.answer.[row-1, col] = FILL_CHAR) then (row, col)
        else getDownClueAnchor p (row-1) col

    // Get the index of the down clue for a given cell
    let getDownClueIndex (p:Puzzle) row col = 
        let (anchorRow, anchorCol) = getDownClueAnchor p row col
        countDownCluesToCell p anchorRow anchorCol

// A CluedPuzzle is a Puzzle plus a pair of clue lists
type CluedPuzzle = {
    puzzle: Puzzle
    across: string list     // The across clues
    down: string list       // The down clues
}
module CluedPuzzle = 
    // Get across clue for a given cell
    let getAcrossClue (cp:CluedPuzzle) row col = 
        if cp.puzzle.answer.[row,col] = Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.across.[Puzzle.getAcrossClueIndex cp.puzzle row col]

    // Get the down clue for a given cell
    let getDownClue (cp:CluedPuzzle) row col = 
        if cp.puzzle.answer.[row,col] = Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.down.[Puzzle.getDownClueIndex cp.puzzle row col]

// A Solution represents a partially-solved crossword puzzle
type Solution = {
    solution: Puzzle        // The partial response
    progress: float         // Percentage of the puzzle completed correctly
}
//module Solution = 
    // Function to compute progress given Complete and Partially solved Puzzles
    //let computeProgress (p:Puzzle) (s:Solution) : float = 
        