namespace fwords.Core.Tests

module JsonTests = 
    open Expecto
    open fwords.Core
    open FSharp.Json
    open System.IO

    [<Tests>]
    let loaderTests = 
        testList "Loader module" [
            testCase "saveDir" <| fun _ ->
                let savePath = FileLoader.saveDir
                Expect.equal savePath "..\..\puzzles" "yeah"

            testCase "store sample data" <| fun _ ->
                FileLoader.store "test.json" SampleData.myCluedPuzzle
                Expect.isTrue (File.Exists("test.json")) "file should exist now"

            testCase "load sample data" <| fun _ -> 
                let mydata = FileLoader.load "test.json"
                Expect.equal SampleData.myCluedPuzzle mydata "should be equal"
                File.Delete "test.json"

            testCase "store in special location" <| fun _ ->
                let mypath = FileLoader.getSavePath "test"
                FileLoader.store mypath SampleData.myCluedPuzzle
                Expect.isTrue (File.Exists("test.json")) "file should exist now"

            testCase "load from special location" <| fun _ -> 
                let mypath = FileLoader.getSavePath "test"
                let mydata = FileLoader.load mypath
                Expect.equal SampleData.myCluedPuzzle mydata "should be equal"
                File.Delete mypath

            
        ]

    [<Tests>]
    let jsonTests =
        testList "Json Encoding/Decoding" [
            
            testCase "serialization round trip" <| fun _ -> 
                let ser = Json.serialize SampleData.myCluedPuzzle
                let myCp2 = Json.deserialize ser
                Expect.equal SampleData.myCluedPuzzle myCp2 "They aren't equal"

            testCase "to/from Jagged" <| fun _ ->
                let jag = Array2D.toJagged SampleData.myCluedPuzzle.puzzle
                let square = Array2D.fromJagged jag
                Expect.equal SampleData.myCluedPuzzle.puzzle square "They should be equal"

            testCase "createTransform (mine)" <| fun _ ->
                let my1 = typeof<Array2DJson.Array2DJagged<char>>
                let my2 = my1.GetConstructors()
                let my3 = Array.length my2
                Expect.isGreaterThanOrEqual my3 1 "Should be at least one"

            testCase "createTransform (theirs)" <| fun _ ->
                let my1 = typeof<FSharp.Json.Transforms.DateTimeEpoch>
                let my2 = my1.GetConstructors()
                let my3 = Array.length my2
                Expect.isGreaterThanOrEqual my3 1 "ditto"
        ]

    //[<Tests>]
    //let jsonProviderTests = 
    //    testList "Json Provider" [
    //        testCase "what type is it"
    //        let 
    //    ]


