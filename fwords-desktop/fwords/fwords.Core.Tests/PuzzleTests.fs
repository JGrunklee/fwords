namespace fwords.Core.Tests

/// These are sample Unit tests that show you  briefly how you can use
/// [Expecto](https://github.com/haf/expecto) unit test library
module Sample =
    open Expecto
    open fwords.Core

    // Puzzle 36 in "Crosswords". Intentionally missing last 3 rows
    let myPuzzle = { answer = array2D [
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
    ]}

    // This blank puzzle has 6 rows and 5 columns
    let blankPuzzle = { answer = array2D [
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
        [for i in 0..4 -> Puzzle.EMPTY_CHAR]
    ]}

    [<Tests>]
    let tests =
        testList "Puzzle Tests" [
            testCase "generateBlank - order of parameters" <| fun _ -> 
                let blanky = Puzzle.generateBlank 6 5
                Expect.equal (Array2D.length1 blanky.answer) (Array2D.length1 blankPuzzle.answer) "There should be 6 rows."
                Expect.equal (Array2D.length2 blanky.answer) (Array2D.length2 blankPuzzle.answer) "There should be 5 columns."

            // Get/set 
            testCase "getCell - order of parameters" <| fun _ ->
                Expect.equal (Puzzle.getCell myPuzzle 3 8) '#' "Cell (3, 8) is '#'."
                Expect.equal (Puzzle.getCell myPuzzle 8 3) 'A' "Cell (8, 3) is 'A'."
            testCase "getCell - out of bounds" <| fun _ -> 
                let getCellOob = fun () -> 
                    let dontReturnThis = Puzzle.getCell myPuzzle 14 9
                    ()
                Expect.throws getCellOob "This should throw something."
            testCase "setCell" <| fun _ ->
                let tPuzzle = Puzzle.setCell myPuzzle 10 14 'Q'
                Expect.equal (Puzzle.getCell tPuzzle 10 14) 'Q' "Set a cell."
            testCase "setCell - out of bounds" <| fun _ -> 
                let tPuzzle = Puzzle.setCell myPuzzle -1 -1 'Z'
                Expect.equal (Puzzle.getCell tPuzzle -1 -1) 'Z' "This cell is out of bounds."
            testCase "checkCell" <| fun _ -> 
                Expect.isTrue (Puzzle.checkCell myPuzzle 5 13 'C') "A correct guess."
                Expect.isFalse (Puzzle.checkCell myPuzzle 0 0 'X') "An incorrect guess."

            // Retreiving cross clues
            testCase "getAcrossClueIndex - 0th row anchor" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 0 11) 2 "Index should be 2."
            testCase "getAcrossClueIndex - 0th row non-anchor" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 0 3) 0 "Index should be 0."
            testCase "getAcrossClueIndex - 0th row filled cell" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 0 10) 1 "Index should be 1."
            testCase "getAcrossClueIndex - nth row anchor" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 6 4) 14 "Index should be 14."
            testCase "getAcrossClueIndex - nth row non-anchor" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 3 12) 9 "Index should be 9."
            testCase "getAcrossClueIndex - nth row filled cell" <| fun _ ->
                Expect.equal (Puzzle.getAcrossClueIndex myPuzzle 4 14) 11 "Index should be 14."

            // Retreiving down clues
            testCase "getDownClueIndex - 0th row before a filled cell" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 0 1) 1 "Index should be 1."
            testCase "getDownClueIndex - 0th row after a filled cell" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 0 13) 11 "Index should be 11."
            testCase "getDownClueIndex - nth row anchor pt 1" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 2 5) 13 "Index should be 13."
            testCase "getDownClueIndex - nth row anchor pt 2" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 11 14) 38 "Index should be 38."
            testCase "getDownClueIndex - nth row non-anchor pt 1" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 5 5) 13 "Index should be 13."
            testCase "getDownClueIndex - nth row non-anchor pt 2" <| fun _ ->
                Expect.equal (Puzzle.getDownClueIndex myPuzzle 9 0) 22 "Index should be 22."
        ]
