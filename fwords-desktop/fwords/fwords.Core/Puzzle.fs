namespace fwords.Core

/// Add helpful extensions to F# standard Array2D module.
module Array2D = 
    /// Split a 2D array into a list of its columns.
    let columwise myarray = 
        let numCols = Array2D.length2 myarray
        [for col in 0..numCols-1 -> myarray.[*,col]]

    /// Apply a function to every member of a 2D array and sum the results.
    let sumBy (projection:'T -> 'U) myarray = 
        myarray
        |> Seq.cast<'T>
        |> Seq.sumBy projection

    /// Reduce all the members of a 2D array by a function.
    let reduce (reduction: 'T->'T->'T) myarray =
        myarray
        |> Seq.cast<'T>
        |> Seq.reduce reduction
        

/// Functions that operate on one or more Puzzle types.
module Puzzle = 

    /// A character that represents an empty cell (a cell which may contain letters, but doesn't at this time).
    [<Literal>]
    let EMPTY_CHAR = ' '

    /// A character that represents a filled cell (a cell which may never contain a letter).
    [<Literal>]
    let FILL_CHAR = '#'

    /// Generate an empty puzzle.
    let generateBlank rows cols : Puzzle = Array2D.init rows cols (fun _ _ -> EMPTY_CHAR)

    /// Get the value of a cell.
    let getCell (p:Puzzle) (c:Cell) = 
        let row,col = c
        p.[row,col]

    /// Set a cell in a Puzzle by creating a new Puzzle with the cell value changed.
    /// (Trying to treat arrays as if they are immutable.)
    let setCell (p:Puzzle) (c:Cell) value : Puzzle = 
        let row,col = c
        let q = Array2D.copy p
        Array2D.set q row col value
        q

    /// Get the number of rows.
    let getRows = Array2D.length1 // The first dimension of the 2D array represents rows

    /// Get the number of columns.
    let getCols = Array2D.length2 // The second dimension of the 2D array represents columns

    /// Check a guess against the given cell.
    let checkCell (p:Puzzle) (c:Cell) guess : bool = (guess = getCell p c)

    /// CellChecker to determine if the specified cell is filled.
    let checkCellFilled p c = (FILL_CHAR = getCell p c)

    /// CellChecker to determine if the specified cell is empty.
    let checkCellEmpty p c = (EMPTY_CHAR = getCell p c)

    /// CellChecker to determine if the specified cell "anchors" (holds the first letter for) an across clue.
    let checkAcrossClueAnchor (p:Puzzle) (c:Cell) : bool = 
        let row,col = c
        // A cell anchors an across clue if it is not filled 
        // and is either along the left edge of the puzzle or immeditately to right of a filled cell.
        //FILL_CHAR <> getCell p c && (col=0 || FILL_CHAR = getCell p (row,col-1))
        (not <| checkCellFilled p c) && (col=0 || checkCellFilled p (row,col-1))

    /// CellChecker to determine if the specified cell "anchors" (holds the first letter for) a down clue.
    let checkDownClueAnchor (p:Puzzle) (c:Cell) : bool = 
        let row,col = c
        // A cell anchors a down clue if it is not filled 
        // and is either along the top edge of the puzzle or immeditately below a filled cell.
        (not <| checkCellFilled p c) && (row=0 || checkCellFilled p (row-1,col))

    /// CellChecker to determine if the specified cell has a number in it.
    let checkHasNumber (p:Puzzle) (c:Cell) : bool = checkDownClueAnchor p c || checkAcrossClueAnchor p c

    /// Count the cells that meet the provided condition in reading order from 
    /// the top left corner until (and including) the specified cell.
    let rec countOccurrancesUntil (check:CellChecker) (p:Puzzle) (c:Cell) : int =
        let row,col = c
        let acc = 
            [0..col] // List all the columns from 0 to the provided one
            |> List.filter (fun tempCol -> check p (row,tempCol)) 
            |> List.length
        if row=0 then acc
        else acc + countOccurrancesUntil check p (row-1, -1 + getCols p)

    /// Find the next cell that meets a provided condition given a starting point and a direction to search.
    /// The starting point is included in the search.
    /// The search area stops at the edge of the Puzzle in the given direction.
    /// Returns None if no cells in the search area meet the condition.
    let rec walkUntil (check:CellChecker) (p:Puzzle) (d:Direction) (c:Cell) : Cell option =
        try 
            if check p c then Some c
            else 
                let row,col = c
                let nextStep = walkUntil check p d
                match d with
                | Upwards -> nextStep ((row-1),col)
                | Leftwards -> nextStep (row,(col-1))
                | Downwards -> nextStep ((row+1),col)
                | Rightwards -> nextStep (row,(col+1))
        with _ -> None // Eventually we'll go off the edge of the puzzle and try to access a cell that doesn't exist

    /// Count the number of across clues that occur in reading order from the top left until the specified cell.
    let countAcrossCluesUntil = countOccurrancesUntil checkAcrossClueAnchor

    /// Count the number of down clue "anchors" that occur in reading order from the top left until the specified cell.
    let countDownCluesUntil = countOccurrancesUntil checkDownClueAnchor

    /// Get the cell that "anchors" (holds the first letter for) the down clue for a specified cell.
    let getDownClueAnchor (p:Puzzle) (c:Cell) = 
        match walkUntil checkDownClueAnchor p Upwards c with // This evaluates to a cell option
        | Some c -> c
        | None -> 
            let row,col = c
            (0,col)

    /// Get the cell that "anchors" the across clue for a specified cell.
    let getAcrossClueAnchor (p:Puzzle) (c:Cell) = 
        match walkUntil checkAcrossClueAnchor p Leftwards c with 
        | Some c -> c
        | None -> 
            let row,col = c
            (row,0)

    /// Compare one Puzzle against another, replacing cells that differ with the empty character.
    let deleteMistakes (answer:Puzzle) (guess:Puzzle) : Puzzle = 
        guess |> Array2D.mapi (fun row col letter -> 
            if checkCell answer (row,col) letter then letter else EMPTY_CHAR )

    /// Compute percent completion of one puzzle against another.
    let computeProgress (answer:Puzzle) (guess:Puzzle) : float =
        let correctCells = deleteMistakes answer guess // Don't count incorrect guesses
        let correctCount = Array2D.sumBy (fun letter -> if (letter=FILL_CHAR || letter=EMPTY_CHAR) then 0 else 1) <| correctCells
        let totalCount = Array2D.sumBy (fun letter -> if not (letter=FILL_CHAR) then 1 else 0) <| answer
        (float correctCount) / (float totalCount)

    /// Return true if the puzzles have the same number of rows and columns
    /// and the same pattern of filled spaces.
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

    /// Find the next cell in a specified direction that isn't filled.
    let getNextCell (p:Puzzle) (d:Direction) (c:Cell) : Cell option =
        // Define special checker to skip the starting cell
        let myChecker (tempP:Puzzle) (tempC:Cell) =
            tempC <> c && not <| checkCellFilled tempP tempC 
        walkUntil myChecker p d c
    
    /// Count the cells in reading order that have a number in them.
    let countNumberedCellsTo (p:Puzzle) (c:Cell) : int = countOccurrancesUntil checkHasNumber p c 

    /// Get a cell's number. None if the cell has no number.
    let getCellNumber (p:Puzzle) (c:Cell) : int option = 
        if checkHasNumber p c then Some (countNumberedCellsTo p c)
        else None 

    let calcHighlights (p:Puzzle) (c:Cell) (o:ClueOrientation) : Cell list =
        // Start with state.selected
        // Walk left/up to filled cell and add 1
        // Walk right/down to filled cell and subtract 1
        // Build list of all the cells between the endpoints (inclusive)
        let row,col = c
        match o with
        | Across ->
            let startCol = 
                match walkUntil checkCellFilled p Leftwards c with
                | None -> 0
                | Some (_,x) -> x+1
            let endCol = 
                match walkUntil checkCellFilled p Rightwards c with
                | None -> (getRows p)-1
                | Some (_,x) -> x-1
            [for i in [startCol..endCol] -> row,i]
        | Down ->
            let startRow = 
                match walkUntil checkCellFilled p Upwards c with
                | None -> 0
                | Some (y,_) -> y+1
            let endRow = 
                match walkUntil checkCellFilled p Downwards c with
                | None -> (getCols p)-1
                | Some (y,_) -> y-1
            [for i in [startRow..endRow] -> i,col]

        
/// Functions that operate on CluedPuzzles
module CluedPuzzle = 
    /// Create an empty CluedPuzzle
    let generateBlank rows cols : CluedPuzzle = {
        puzzle = Puzzle.generateBlank rows cols
        across = []
        down = []
    }

    /// Get the index of the down clue for a given cell.
    let getDownClueIndex (cp:CluedPuzzle) (c:Cell) = Puzzle.getDownClueAnchor cp.puzzle c |> Puzzle.countDownCluesUntil cp.puzzle |> (+) -1

    /// Get the index of the across clue for a given cell.
    let getAcrossClueIndex (cp:CluedPuzzle) (c:Cell) = Puzzle.countAcrossCluesUntil cp.puzzle c |> (+) -1

    /// Get across clue for a given cell
    let getAcrossClue (cp:CluedPuzzle) (c:Cell) = 
        if Puzzle.checkCell cp.puzzle c Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.across.[getAcrossClueIndex cp c]

    /// Get the down clue for a given cell
    let getDownClue (cp:CluedPuzzle) (c:Cell) = 
        if Puzzle.checkCell cp.puzzle c Puzzle.FILL_CHAR then
            invalidArg "row, col" "Cell is filled. Filled cells aren't associated with clues."
        else
            cp.down.[getDownClueIndex cp c]

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
                        if getAcrossClueIndex cp (row,col) = index then number
                        else loop (row,col+1)
                    | Down ->
                        if getDownClueIndex cp (row,col) = index then number
                        else loop (row,col+1)
        loop (0,0)

    let getFormalClue (cp:CluedPuzzle) (c:Cell) (o:ClueOrientation) : string = 
        try
            let clueNumStr = 
                match o with
                | Across -> 
                    getAcrossClueIndex cp c
                    |> getClueNumber cp Across
                    |> string
                | Down -> 
                    getDownClueIndex cp c
                    |> getClueNumber cp Down
                    |> string
            let clueStr = 
                match o with 
                | Across -> getAcrossClue cp c
                | Down -> getDownClue cp c 
            clueNumStr + " " + (string o) + ": " + clueStr 
        with | _ -> "" // Return an empty string. This might happen if an invalid cell is selected

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




    