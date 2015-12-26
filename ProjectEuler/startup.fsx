#r @"C:\Users\Stephen\Documents\Visual Studio 2010\Projects\ProjectEuler\FsExt\bin\Debug\FsExt.dll"
#r @"C:\Users\Stephen\Documents\Visual Studio 2010\Projects\ProjectEuler\FsMath\bin\Debug\FsMath.dll"
#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.Parallel.Seq.dll" 
#r @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.dll"

open Swensen
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections
open System.Numerics
open Swensen.Combinatorics
open Microsoft.FSharp.Core.LanguagePrimitives


//crazy results i don't understand, especially since i've upgraded the project to .net 4.0 now!
//see http://stackoverflow.com/questions/2945880/f-performance-question-what-is-the-compiler-doing
//should maybe move to KVB's approach.
//let val1 =
//    let gL = G_of 1L
//    [1L..100000L] |> List.map (fun n -> Generic.factorize_of gL n) |> ignore
//
//let val2 =
//    [1L..100000L] |> List.map (fun n -> Generic.factorize_of (G_of 1L) n) |> ignore
//
//let val3 =
//    [1L..100000L] |> List.map (fun n -> factorizeL n) |> ignore