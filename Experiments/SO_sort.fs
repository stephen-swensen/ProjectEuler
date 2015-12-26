namespace bla

module Test =
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core.Operators

    let display =
        let a = [| 1; -1; System.Int32.MinValue; 0; System.Int32.MaxValue; 1 |]
        
        [| 1; -1; System.Int32.MinValue; 0; System.Int32.MaxValue; 1 |]
        |> Seq.sortWith (flip compare)
        |> Seq.iter (printfn "%A")

    let a = [| 1; -1; System.Int32.MinValue; 0; System.Int32.MaxValue; 1 |]
    printfn "%A" (a |> Array.sortWith (fun x y -> compare y x))


    module Seq =
        let sortByDesc f s = System.Linq.Enumerable.OrderByDescending(s, System.Func<'a,'b>(f))

    {0..10} |> Seq.sortByDesc id


    let find elements index =
        //a local tail-recursive function hides implementation details
        //(cur::tail) is a pattern match on the list, i is the current index position
        let rec loop (cur::tail) i =
            if i = index then //when the current index matches the search index
                if cur >= tail.Head then i //compare cur to tail.Head (which is next)
                else (i+1)
            else loop tail (i+1) //else continue
        loop elements 0 //the entry point of loop and our return value