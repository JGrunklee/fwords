namespace fwords.Core.Tests

/// These are sample Unit tests that show you  briefly how you can use
/// [Expecto](https://github.com/haf/expecto) unit test library
module PuzzleTests =
    open Expecto
    open fwords.Core

    // Puzzle 36 in "Crosswords". Intentionally missing last 3 rows
    let myPuzzle = array2D [
        ['S'; 'E'; 'D'; 'G'; 'E'; '#'; 'A'; 'F'; 'O'; 'R'; '#'; 'H'; 'O'; 'L'; 'Y']
        ['O'; 'N'; 'E'; 'O'; 'R'; '#'; 'N'; 'O'; 'R'; 'A'; '#'; 'A'; 'L'; 'E'; 'E']
        ['A'; 'I'; 'R'; 'C'; 'O'; 'N'; 'D'; 'I'; 'T'; 'I'; 'O'; 'N'; 'I'; 'N'; 'G']
        ['R'; 'A'; 'M'; 'A'; '#'; 'O'; 'Y'; 'E'; '#'; 'N'; 'A'; 'G'; 'N'; 'A'; 'G']
        ['#'; 'C'; 'O'; 'R'; 'G'; 'I'; '#'; '#'; '#'; 'I'; 'T'; 'N'; '#'; '#'; '#']
        ['#'; '#'; '#'; 'T'; 'A'; 'R'; 'T'; 'A'; 'R'; 'E'; 'S'; 'A'; 'U'; 'C'; 'E']
        ['U'; 'B'; 'I'; '#'; 'I'; 'S'; 'O'; 'M'; 'E'; 'R'; '#'; 'I'; 'P'; 'O'; 'S']
        ['Z'; 'A'; 'P'; 'P'; 'A'; '#'; 'S'; 'E'; 'W'; '#'; 'C'; 'L'; 'A'; 'M'; 'P']
        ['I'; 'L'; 'S'; 'A'; '#'; 'C'; 'I'; 'M'; 'E'; 'M'; 'A'; '#'; 'T'; 'A'; 'N']
        ['S'; 'T'; 'A'; 'N'; 'D'; 'A'; 'R'; 'D'; 'T'; 'I'; 'M'; 'E'; '#'; '#'; '#']
        ['#'; '#'; '#'; 'P'; 'I'; 'R'; '#'; '#'; '#'; 'X'; 'A'; 'X'; 'I'; 'S'; '#']
        ['S'; 'E'; 'R'; 'I'; 'E'; 'S'; '#'; 'G'; 'E'; 'E'; '#'; 'A'; 'G'; 'A'; 'R']
    ]

    // An empty CluedPuzzle
    let myCluelessPuzzle = {puzzle=myPuzzle; down=[]; across=[]}

    // This blank puzzle has 6 rows and 5 columns
    let blankPuzzle = array2D [
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
    ]

    [<Tests>]
    let puzzleTests =
        testList "Puzzle Tests" [
            testCase "generateBlank - order of parameters" <| fun _ -> 
                let blanky = Puzzle.generateBlank 6 5
                Expect.equal (Puzzle.getRows blanky) (Puzzle.getRows blankPuzzle) "There should be 6 rows."
                Expect.equal (Puzzle.getCols blanky) (Puzzle.getCols blankPuzzle) "There should be 5 columns."

            // Get/set 
            testCase "getCell - order of parameters" <| fun _ ->
                Expect.equal (Puzzle.getCell myPuzzle (3,8)) '#' "Cell (3, 8) is '#'."
                Expect.equal (Puzzle.getCell myPuzzle (8,3)) 'A' "Cell (8, 3) is 'A'."
            testCase "getCell - out of bounds" <| fun _ -> 
                let getCellOob = fun () -> 
                    let dontReturnThis = Puzzle.getCell myPuzzle (14,9)
                    ()
                Expect.throws getCellOob "This should throw something."
            testCase "setCell" <| fun _ ->
                let tPuzzle = Puzzle.setCell myPuzzle (10,14) 'Q'
                Expect.equal (Puzzle.getCell tPuzzle (10,14)) 'Q' "Set a cell."
            testCase "setCell - out of bounds" <| fun _ -> 
                let setCellOob = fun () -> 
                    let dontReturnThis = Puzzle.setCell myPuzzle (-1,-1) 'Z'
                    ()
                Expect.throws setCellOob "This cell is out of bounds."
            testCase "checkCell" <| fun _ -> 
                Expect.isTrue (Puzzle.checkCell myPuzzle (5,13) 'C') "A correct guess."
                Expect.isFalse (Puzzle.checkCell myPuzzle (0,0) 'X') "An incorrect guess."

            // Navigation
            testCase "walkTo" <| fun _ ->
                let actual = Puzzle.walkUntil Puzzle.checkCellFilled myPuzzle Direction.Rightwards (0,0)
                Expect.equal actual (Some (0,5)) "Looking for first cell that's filled"
            testCase "getNextCell - no filled cells in the way" <| fun _ -> 
                let nextEmpty = Puzzle.getNextCell myPuzzle Direction.Rightwards (0,0)
                Expect.equal nextEmpty (Some (0,1)) "Looking for first non-filled cell"
            testCase "getNextCell - jumping over a filled cell" <| fun _ -> 
                let nextEmpty = Puzzle.getNextCell myPuzzle Direction.Rightwards (0,4)
                Expect.equal nextEmpty (Some (0,6)) "Looking for first non-filled cell"

            // Retreiving across clues
            testCase "getAcrossClueIndex - 0th row anchor" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (0,11)) 2 "Index should be 2."
            testCase "getAcrossClueIndex - 0th row non-anchor" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (0,3)) 0 "Index should be 0."
            testCase "getAcrossClueIndex - 0th row filled cell" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (0,10)) 1 "Index should be 1."
            testCase "getAcrossClueIndex - nth row anchor" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (6,4)) 14 "Index should be 14."
            testCase "getAcrossClueIndex - nth row non-anchor" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (3,12)) 9 "Index should be 9."
            testCase "getAcrossClueIndex - nth row filled cell" <| fun _ ->
                Expect.equal (CluedPuzzle.getAcrossClueIndex myCluelessPuzzle (4,14)) 11 "Index should be 14."

            // Retreiving down clues
            testCase "getDownClueIndex - 0th row before a filled cell" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (0,1)) 1 "Index should be 1."
            testCase "getDownClueIndex - 0th row after a filled cell" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (0,13)) 11 "Index should be 11."
            testCase "getDownClueIndex - nth row anchor pt 1" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (2,5)) 13 "Index should be 13."
            testCase "getDownClueIndex - nth row anchor pt 2" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (11,14)) 38 "Index should be 38."
            testCase "getDownClueIndex - nth row non-anchor pt 1" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (5,5)) 13 "Index should be 13."
            testCase "getDownClueIndex - nth row non-anchor pt 2" <| fun _ ->
                Expect.equal (CluedPuzzle.getDownClueIndex myCluelessPuzzle (9,0)) 22 "Index should be 22."

            // Operations with 2 puzzles
            testCase "deleteMistakes" <| fun _ -> 
                let myCopy0 = Array2D.copy myPuzzle
                let myCopy1 = Puzzle.setCell myCopy0 (0,0) 'Q'
                let myCopy2 = Puzzle.setCell myCopy1 (11,4) 'Q'
                let myCopy3 = Puzzle.setCell myCopy2 (10,13) 'Q'
                let corrected = Puzzle.deleteMistakes myPuzzle myCopy3
                Expect.isTrue (
                    (Puzzle.checkCell corrected (0,0) Puzzle.EMPTY_CHAR) && 
                    (Puzzle.checkCell corrected (11,4) Puzzle.EMPTY_CHAR) && 
                    (Puzzle.checkCell corrected (10,13) Puzzle.EMPTY_CHAR))
                    "All the cells we set should be empty in the corrected Puzzle."

            testCase "computeProgress (completed Puzzle)" <| fun _ -> 
                Expect.equal (Puzzle.computeProgress myPuzzle myPuzzle) 1.0 "myPuzzle is 100% complete compared to itself."

            testCase "computeProgress 1" <| fun _ -> 
                let myCopy0 = Array2D.copy myPuzzle
                let myCopy1 = Puzzle.setCell myCopy0 (0,0) 'Q'
                let myCopy2 = Puzzle.setCell myCopy1 (11,4) 'Q'
                let myCopy3 = Puzzle.setCell myCopy2 (10,13) 'Q'
                Expect.equal (Puzzle.computeProgress myPuzzle myCopy3) (143.0/146.0) "Progress when 3 cells are erased."

            testCase "computeProgress 2" <| fun _ -> 
                let myCopy0 = Array2D.copy myPuzzle
                let myCopy1 = Puzzle.setCell myCopy0 (0,0) 'Q'
                let myCopy2 = Puzzle.setCell myCopy1 (11,4) 'Q'
                let myCopy3 = Puzzle.setCell myCopy2 (10,13) 'Q'
                let myCopy4 = Puzzle.setCell myCopy3 (2,2) 'Q'
                Expect.equal (Puzzle.computeProgress myPuzzle myCopy4) (142.0/146.0) "Progress when 4 cells are erased."
        ]

    [<Tests>]
    let solutionTests = 
        testList "Solution Tests" [
            testCase "generateBlank" <| fun _ ->
                let empty = Solution.generateBlank myPuzzle
                let blankCount = Array2D.sumBy (fun letter -> if letter=Puzzle.EMPTY_CHAR then 1 else 0) empty.puzzle
                Expect.equal blankCount 146 "Make sure there are the right number of empty cells."

            testCase "Puzzle computeProgress (empty Solution)" <| fun _ -> 
                let empty = Solution.generateBlank myPuzzle
                Expect.equal (Puzzle.computeProgress myPuzzle empty.puzzle) 0.0 "a blank puzzle is 0% complete."
        ]