namespace fwords.Core

//// TODO: generic loader that can abstract loading from file vs url vs other sources
//// module Loader =
    

module FileLoader = 
    open FSharp.Json
    open System.IO

    /// F#-style call to File.WriteAllText
    let fsFileWriteAllText path contents = File.WriteAllText(path,contents)

    /// F#-style call to File.ReadAllText
    let fsFileReadAllText path = File.ReadAllText(path)

    /// Path to the "puzzles" folder. TODO: update for Release
    let saveDir = Path.Join [|__SOURCE_DIRECTORY__ ; "puzzles"|]

    /// List of available puzzles
    let listSaved = Directory.GetFiles(saveDir) |> Array.map (fun p -> Path.GetFileNameWithoutExtension(p))

    /// Build save path from a puzzle name
    let getSavePath (basename:string) = 
        let filename = 
            if Path.HasExtension(basename) then Path.ChangeExtension(basename, ".json")
            else basename + ".json"
        Path.Join [| saveDir ; filename |]

    /// Deserialize any type from file
    let load filename =
        fsFileReadAllText filename
        |> Json.deserialize 

    /// Serialize and write any type to file
    let store filename data : unit = 
        Json.serialize data
        |> fsFileWriteAllText filename

    /// Load a puzzle from file
    let loadPuzzle basename : PuzzleInfo * CluedPuzzle * Solution option = basename|> getSavePath |> load

    /// Store a puzzle to a file
    let storePuzzle basename (pi:PuzzleInfo) (cp:CluedPuzzle) (s:Solution option) =
        (pi , cp , s) |> store (getSavePath basename)
    
        
/// Load NYT puzzles from github archive maintained by user doshea
module DosheaLoader = 
    open System.Net.Http
    let private baseurl = "https://raw.githubusercontent.com/doshea/nyt_crosswords/master/"
    type PuzzleEndpoint = FSharp.Data.JsonProvider<"https://raw.githubusercontent.com/doshea/nyt_crosswords/master/2018/03/09.json">

    let EMPTY_CHAR = '.'
    type DosheaSize = {
        rows : int
        cols : int
    }
    type DosheaPair = {
        down : string list
        across : string list
    }
    type DosheaPuzzle = {
        downmap : obj option
        hastitle : bool
        code : obj option
        mini : obj option
        autowrap : obj option
        gridnums : int list
        id : obj option
        size : DosheaSize
        copyright : string
        author : string
        bbars : obj option
        id2 : obj option
        jnotes : obj option
        shadecircles : bool option
        editor : string
        type_ : obj option
        circles : int list option
        interpretcolors : obj option
        track : obj option
        notepad : obj option
        answers : DosheaPair
        grid : char list
        key : obj option
        clues : DosheaPair
        date : string
        hold : obj option
        publisher : string
        acrossmap : obj option
        admin : bool
        title : string
        rbars : obj option
        dow : string
    }

    