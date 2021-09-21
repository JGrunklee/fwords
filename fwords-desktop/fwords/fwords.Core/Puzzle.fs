namespace fwords.Core

// Add helpful functions to F# standard Array2D module
module Array2D = 
    /// Split a 2D array into a list of its columns
    let columwise myarray = 
        let numCols = Array2D.length2 myarray
        [for col in 0..numCols-1 -> myarray.[*,col]]

    /// Apply a function to every member of a 2D array and sum the results
    let sumBy (projection:'T -> 'U) myarray = 
        myarray
        |> Seq.cast<'T>
        |> Seq.sumBy projection

    /// Reduce all the members of a 2D array by a function
    let reduce (reduction: 'T->'T->'T) myarray =
        myarray
        |> Seq.cast<'T>
        |> Seq.reduce reduction
        

/// Functions that operate on one or more Puzzle types
module Puzzle = 

    /// A character that represents an empty cell
    [<Literal>]
    let EMPTY_CHAR = ' '

    /// A character that represents a filled cell
    [<Literal>]
    let FILL_CHAR = '#'

    /// Create a puzzle from a 2D char array
    //let fromArray (myArray: char[,]) : Puzzle =  {cells = myArray}

    /// Generate an empty puzzle
    let generateBlank rows cols : Puzzle = Array2D.init rows cols (fun _ _ -> EMPTY_CHAR)

    /// Get the value of a cell
    let getCell (p:Puzzle) (c:Cell) = 
        let row,col = c
        p.[row,col]

    /// Set a cell in a Puzzle by creating a new Puzzle with the cell value changed.
    /// Trying to treat arrays as if they are immutable
    let setCell (p:Puzzle) (c:Cell) value : Puzzle = 
        let row,col = c
        let q = Array2D.copy p
        Array2D.set q row col value
        q

    /// Get the number of rows
    let getRows = Array2D.length1

    /// Get the number of columns
    let getCols = Array2D.length2

    /// Check a guess against the given cell
    /// Returns true for a correct guess
    let checkCell (p:Puzzle) (c:Cell) guess : bool = 
        let row,col = c
        p.[row,col] = guess

    /// Compute which across clue applies to the given cell
    let rec getAcrossClueIndex (p:Puzzle) (c:Cell) = 
        let row,col = c
        // Calculate nth across clue in current row by looking for places 
        // where there is a non-filled cell to the right of a filled cell.
        let acc =
            p.[row,..col]
            |> Array.pairwise
            |> Array.filter ( fun pair -> 
                let previous,current = pair // current cell is to the right of the previous one (moving rightwards)
                (current <> FILL_CHAR) && (previous = FILL_CHAR))
            |> Array.length
            |> (+) (if p.[row,0] = FILL_CHAR then 0 else 1)
        if row=0 then
            acc - 1 // No more rows left. Return result and zero-index
        else
            // Compute number of accross clues in entire next row above.
            ((row-1), getCols p)    // Last cell in the previous row
            |> getAcrossClueIndex p // count clues in that row (and in any additional rows above)
            |> (+) acc              // Add to the accumulator

    /// Count down clues 'anchored' before (left to right, top to bottom) a cell 
    let rec countDownCluesToCell (p:Puzzle) (c:Cell) = 
        // Count cells in row 0 that 'anchor' a down clue
        // Every cell in row 0 that isn't filled achors a down clue
        let countDownCluesToColRow0 (p:Puzzle) col = 
            p.[0,..col]
            |> Array.filter (fun c -> c <> FILL_CHAR)
            |> Array.length
        // Count cells in row n!=0 that 'anchor' a down clue
        // Non-filled cells with a filled cell above anchor down clues
        let countDownCluesToColRowN (p:Puzzle) (c:Cell) =
            let row,col = c
            p.[(row-1)..row,..col] //Slice current row and row immediately above
            |> Array2D.columwise
            |> List.filter ( fun pair -> (pair.[0] = FILL_CHAR) && (pair.[1] <> FILL_CHAR))
            |> List.length

        // Count the number of down clues in this row
        let row,col = c
        let acc = 
            if row = 0 then countDownCluesToColRow0 p col
            else countDownCluesToColRowN p (row,col)

        if row=0 then 
            acc - 1 // No more rows left. Return result and zero-index
        else
            // Compute number of down clues anchored in the row above
            let lastCol = (getCols p) - 1
            ((row-1),lastCol)           // Start at the last cell in the previous row
            |> countDownCluesToCell p   // Count down clues to that cell
            |> (+) acc                  // Add count to the accumulator

    /// Get the cell that 'anchors' the down clue for that cell
    /// Assumes initially provided cell is NOT filled
    let rec getDownClueAnchor (p:Puzzle) (c:Cell) = 
        let row,col = c
        if row = 0 then (row, col)
        elif (p.[row, col] <> FILL_CHAR) && (p.[row-1, col] = FILL_CHAR) then (row, col) // Non-filled cells with a filled cell above anchor down clues
        else getDownClueAnchor p <| ((row-1),col) // Move up one row and try again

    /// Get the index of the down clue for a given cell
    let getDownClueIndex (p:Puzzle) (c:Cell) = getDownClueAnchor p c |> countDownCluesToCell p // heck yeah!

    /// Compare one Puzzle against another, replacing cells that differ with the empty character
    let deleteMistakes (answer:Puzzle) (guess:Puzzle) : Puzzle = 
        guess |> Array2D.mapi (fun row col letter -> 
            if checkCell answer (row,col) letter then letter else EMPTY_CHAR )

    /// Compute percent completion of one puzzle against another
    let computeProgress (answer:Puzzle) (guess:Puzzle) : float =
        let correctCells = deleteMistakes answer guess // Don't count incorrect guesses
        let correctCount = Array2D.sumBy (fun letter -> if (letter=FILL_CHAR || letter=EMPTY_CHAR) then 0 else 1) <| correctCells
        let totalCount = Array2D.sumBy (fun letter -> if not (letter=FILL_CHAR) then 1 else 0) <| answer
        (float correctCount) / (float totalCount)

    /// Return true if the puzzles have the same number of rows and columns
    /// and the same pattern of filled spaces
    let checkSame (p:Puzzle) (q:Puzzle) : unit = 
        if not(((getRows p)=(getRows q)) && (getCols p)=(getCols q)) then 
            invalidArg "q" "Puzzle.checkSame: Puzzles have different dimensions."
        else
            let same = 
                p
                |> Array2D.mapi (fun row col letter ->
                    let pFilled = (letter=FILL_CHAR)
                    let qFilled = checkCell q (row,col) FILL_CHAR
                    (pFilled=qFilled)) // false if there is a filled cell in one but not the other
                |> Array2D.reduce (&&) // logical AND - we want to know if any value is false
            if not same then invalidArg "q" "Puzzles.checkSame: Puzzles are different."

    /// Find the next cell that meets a provided condition given a starting point and a direction to search.
    /// The starting point is included in the search.
    /// The search area stops at the edge of the Puzzle in the given direction.
    /// Returns None if no cells in the search area meet the condition.
    let rec walkTo (p:Puzzle) predicate (d:Direction) (c:Cell) : Cell option =
        let row,col = c
        try
            if p.[row,col] |> predicate then Some (row,col) // Success! We found a cell that satisfied the predicate
            else // try the next cell...
                let nextStep = walkTo p predicate d // We're going to call this a bunch below
                match d with
                | Upwards -> nextStep ((row-1),col)
                | Leftwards -> nextStep (row,(col-1))
                | Downwards -> nextStep ((row+1),col)
                | Rightwards -> nextStep (row,(col+1))
        with
        | _ -> None // Eventually we'll go off the edge of the puzzle and try to access a cell that doesn't exist
        
    /// Find the next cell that isn't filled
    let getNextCell (p:Puzzle) (d:Direction) (c:Cell) : Cell option =
        // This function is walkTo but with a predefined predicate and ignoring the starting cell
        let row,col = c
        let predicate = fun c -> c <> FILL_CHAR
        match d with
        | Upwards -> (row-1),col
        | Leftwards ->  row,(col-1)
        | Downwards -> (row+1),col
        | Rightwards -> row,(col+1)
        |> walkTo p predicate d 

    let rec countNumberedCellsTo (p:Puzzle) (c:Cell) : int =
        let acc = if checkCell p c FILL_CHAR then 0 else 1
        match c with 
        | (0,0) -> acc
        | (0,col) -> (0,col-1) |> countNumberedCellsTo p |> (+) acc
        | (row,0) -> (row-1, (getCols p)-1) |> countNumberedCellsTo p |> (+) acc
        | (row,col) when checkCell p (row-1,col) FILL_CHAR || checkCell p (row,col-1) FILL_CHAR ->
            (row, col-1) |> countNumberedCellsTo p |> (+) acc
        | (row,col) -> (row,col-1) |> countNumberedCellsTo p // DON'T add because this cell has no number

    let getCellNumber (p:Puzzle) (c:Cell) : int option = 
        let row,col = c
        if row<>0 && col<>0 
            && not (checkCell p (row-1,col) FILL_CHAR || checkCell p (row,col-1) FILL_CHAR) then None
        else Some (countNumberedCellsTo p c)
        
/// Functions that operate on CluedPuzzles
module CluedPuzzle = 
    /// Create an empty CluedPuzzle
    let generateBlank rows cols : CluedPuzzle = {
        puzzle = Puzzle.generateBlank rows cols
        across = []
        down = []
    }

    /// Get across clue for a given cell
    let getAcrossClue (cp:CluedPuzzle) (c:Cell) = 
        if Puzzle.checkCell cp.puzzle c Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.across.[Puzzle.getAcrossClueIndex cp.puzzle c]

    /// Get the down clue for a given cell
    let getDownClue (cp:CluedPuzzle) (c:Cell) = 
        if Puzzle.checkCell cp.puzzle c Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.down.[Puzzle.getDownClueIndex cp.puzzle c]

    let setCell (cp:CluedPuzzle) (c:Cell) value : CluedPuzzle = { cp with puzzle = Puzzle.setCell cp.puzzle c value }        

    let getClueNumber (cp:CluedPuzzle) (orientation:ClueOrientation) (index:int) : int =
        let rec loop (c:Cell) = 
            let row,col = c
            if row >= (Puzzle.getCols cp.puzzle) then -1
            elif col >= (Puzzle.getCols cp.puzzle) then loop (row+1,0)
            else
                match Puzzle.getCellNumber cp.puzzle c with
                | None -> loop (row,col+1)
                | Some number -> 
                    match orientation with
                    | Across -> 
                        if Puzzle.getAcrossClueIndex cp.puzzle (row,col) = index then number
                        else loop (row,col+1)
                    | Down ->
                        if Puzzle.getDownClueIndex cp.puzzle (row,col) = index then number
                        else loop (row,col+1)
        loop (0,0)

/// Functions that operate on Solutions
module Solution = 
    /// Create a brand new (empty) solution
    let generateBlank (p:Puzzle) : Solution = {
        puzzle = p |> Array2D.map (fun letter -> if letter=Puzzle.FILL_CHAR then letter else Puzzle.EMPTY_CHAR)
        progress = 0.0
        }

    /// Set a cell in a Solution
    let setCell (p:Puzzle) (s:Solution) (c:Cell) letter : Solution =
        let updated = Puzzle.setCell s.puzzle c letter
        {
            puzzle = updated
            progress = (Puzzle.computeProgress p updated)
        }




    