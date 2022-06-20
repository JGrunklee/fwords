namespace fwords.Core

/// Add helpful extensions to F# standard Array2D module.
module Array2D = 
    /// Split a 2D array into a list of its columns.
    let columwise myarray = 
        let numCols = Array2D.length2 myarray
        [for col in 0..numCols-1 -> myarray.[*,col]]

    /// Apply a function to every member of a 2D array and sum the results.
    // "inline" keyword is needed to resolve the types of 'T and 'U
    let inline sumBy (projection:'T -> ^U) myarray = 
        myarray
        |> Seq.cast<'T>
        |> Seq.sumBy projection

    /// Reduce all the members of a 2D array by a function.
    let reduce (reduction: 'T->'T->'T) myarray =
        myarray
        |> Seq.cast<'T>
        |> Seq.reduce reduction

    /// Convert a 2D array into an array of arrays
    /// https://stackoverflow.com/questions/37842353/f-converting-array2d-to-array-of-arrays
    let toJagged (arr: 'a[,]) : 'a [] [] = 
        [| for x in 0 .. Array2D.length1 arr - 1 do
                yield [| for y in 0 .. Array2D.length2 arr - 1 -> arr.[x, y] |]
            |]

    /// Convert an array of arrays to a 2D array
    let fromJagged (arr: 'a[][]) : 'a[,] = 
        let len1 = Array.length arr
        let len2 = 
            arr 
            |> Array.map Array.length // find length of each sub-array
            |> Array.max // find largest, so all the information will be included
        Array2D.create len1 len2 arr.[0].[0]
        |> Array2D.mapi (fun index1 index2 _ -> arr.[index1].[index2])

module Array2DJson = 
    open FSharp.Json

    /// Convert a 2D array to a 'jagged' array of arrays
    // IMPORTANT: This is a class!
    type Array2DJagged<'a>() =
        interface ITypeTransform with
            member x.targetType () = (fun _ -> typeof<'a[][]>) ()
            member x.toTargetType value = (fun (v:obj) -> (Array2D.toJagged (v :?> 'a[,])) :> obj ) value
            member x.fromTargetType value = (fun (v:obj) -> (Array2D.fromJagged (v :?> 'a[][])) :> obj ) value


