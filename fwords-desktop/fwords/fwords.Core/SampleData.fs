namespace fwords.Core

module SampleData = 
    // Puzzle 36 in "Crosswords"
    let myPuzzle = { cells = array2D [
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
        ['T'; 'R'; 'I'; 'P'; 'L'; 'E'; 'W'; 'O'; 'R'; 'D'; 'S'; 'C'; 'O'; 'R'; 'E']
        ['R'; 'O'; 'T'; 'E'; '#'; 'A'; 'I'; 'D'; 'A'; '#'; 'A'; 'T'; 'R'; 'E'; 'E']
        ['S'; 'O'; 'A'; 'S'; '#'; 'T'; 'E'; 'S'; 'S'; '#'; 'B'; 'A'; 'S'; 'E'; 'D']
    ]}

    let acrossClues = [
        "Riverbank plant"
        "Get an ___ effort: 2wds"
        "Third word of 'Silent Night'"
        "___ two (a few): 2wds"
        "Ibsen protagonist"
        "Toward Shelter"
        "Cooling system: 2wds"
        "One of the ten Thai kings"
        "Kings of Convenience guitarist Erlend"
        "'Get off my case': 2wds"
        "Short-legged dog"
        "Brit. news network"
        "Condiment eaten with fish: 2wds"
        "Where: Lat."
        "Chemical cousin"
        "NYSE banner events"
        "Frank of the Mothers of Invention"
        "Make a seam"
        "Squeeze"
        "Mrs. Victor Laszlo"
        "Filmmaker's art"
        "Whup"
        "Official hour in a local region: 2wds"
        "Muslim saint or holy man"
        "Horizontal part of a plane: hyph."
        "World ___"
        "'Oh, wow'"
        "Jelly ingredient"
        "Red square in Scrabble: 3wds"
        "Memorization method"
        "slave of opera"
        "Up ___ (cornered): 2wds"
        "In order (to): 2wds"
        "Actress Harper"
        "'___ on a true story'"
    ]

    let downClues = [
        "Go parasailing"
        "Early supercomputer"
        "Skin: prefix"
        "Racer: var., hyph. (gross)"
        "Ending for pistol or haban"
        "Capp of the comics"
        "Pete de ___ gras"
        "Uneaten morsel"
        "Kelly's prince"
        "Piece of torn skin on a finger"
        "Chemicals giant"
        "Singing great ___ Horne"
        "Burglar"
        "Some roulette bets"
        "Food in a horse's feedbag"
        "Mother of Uranus in Greek Mythology"
        "'___, with love' (Sydney Pioter movie): 2wds"
        "Make changes"
        "Moisten again"
        "___ the crack of dawn: 2wds"
        "Deep sleep"
        "Channel for an armchair quarterback: inits."
        "Commando weapons"
        "Lithuanian, e.g."
        "Res ___ loquitur (rule of evidence)"
        "Primitive wind instrument"
        "Bed, in Spanish"
        "Kid carrier: 2 wds"
        "Intermingled"
        "Involving a 24-hour period, in biology"
        "Track wager"
        "Stravinsky and Tamm"
        "Renee's wrap: var."
        "Orch. section"
        "Ending for switch"
        "Hayworth or Marley"
        "Mount Olympus dwellers"
        "Paleozoic and Mesozoic"
        "Kind of instrument"
        "___ geht's? (German 'how are you?')"
        "Day of rest and worship: abbr."
    ]

    let myCluedPuzzle = {
        puzzle = myPuzzle
        across = acrossClues
        down = downClues
    }

    let myEmptySolution = Solution.generateBlank myPuzzle


