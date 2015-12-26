module Digits
let digitsG n =
    let nstr = n |> string
    seq { for c in 0..nstr.Length-1 do
              let cint = c |> int
              if cint <= 57 && cint >= 48 then //digit chars are in range 48..57
                yield cint-48 }
    |> Seq.toArray |> System.Array.AsReadOnly //for consistency, though a little slower
              
open Microsoft.FSharp.Core.LanguagePrimitives
open Swensen

let inline digits_of (g:G<'a>) (ten:'a) (n:'a) =
    let mutable n = if n < g.zero then (Generic.abs_of g n) else n
    let mutable powten = 0
    
    while (n >= (pown ten powten)) do
        powten <- powten+1
        
    let darr = Array.create powten 0
    let mutable i = 0
    while (powten > 0) do
        powten <- powten-1
        let d = n/(pown ten powten)
        darr.[i] <- d |> int
        n <- n - d*(pown ten powten)
        i <- i + 1
        
    System.Array.AsReadOnly darr
    
let digitsn = digits_of gn (gn.any 10)
let digitsI = digits_of gI (gI.any 10)

//let digitsn input =
//    let mutable n = input
//    
//    let mutable powten = 0
//    while (n >= pown 10 powten) do
//        powten <- powten+1
//        
//    let darr = Array.create powten 0           
//    let mutable i = 0
//    while (powten > 0) do
//        powten <- powten-1
//        let d = n/(pown 10 powten)
//        darr.[i] <- d
//        n <- n - d*(pown 10 powten)
//        i <- i + 1
//        
//    darr


let sw = System.Diagnostics.Stopwatch()

sw.Reset()
sw.Start()
let test2 = 
    {1I..200000I} |> Seq.map (digitsG>>Seq.length) |> Seq.length
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds    

//sw.Reset()
//sw.Start()
//let test3 = 
//    {1..200000} |> Seq.map (digitsn>>Seq.length) |> Seq.length
//sw.Stop()
//printfn "%A" sw.ElapsedMilliseconds    

sw.Reset()
sw.Start()
let test1 = 
    {1I..200000I} |> Seq.map (digitsI>>Seq.length) |> Seq.length
sw.Stop()
printfn "%A" sw.ElapsedMilliseconds
