module Swensen.Combinatorics

open System
open System.Collections.Generic
open Microsoft.FSharp.Core.Operators

//fssnipt description
//A fast implementation of a k-length lexicographic permutation sequence generator demonstrating how F# library implementers can take advantage of F# imperative features while keeping the library API pure for consumers when imperative algorithms yield superior performance to pure functional algorithms.

//fssnippit tags: 

//permutations, combinatorics, imperative

//still debating whether e ought to be sorted
///Create a sequence which yields the remaining k-length lexicographical permutations of e using the 
///comparison function f; permutations start with e and continue until the last lexicographical permutation of e:
///if you want all permuations for a given set, make sure to order e before callings this function.
let klexPerms f k e =
    if k < 0 || k > (Seq.length e) then
        invalidArg "k" "out of bounds" |> raise

    //only need to compute IComparers used for Array.Sort in-place sub-range overload once
    let fComparer = f |> comparer
    let revfComparer = f |> flip |> comparer

    ///Advances (mutating) perm to the next k-length lexical permutation.
    let klexPermute perm =
        //sort last perm.Length - r elements in decreasing order,
        //thereby avoiding duplicate permutations of the first r elements
        //todo: experiment with eliminate this trick and instead concat all
        //lex perms generated from ordered combinations of length r of e (like cfern)
        Array.Sort(perm, k, Array.length perm - k, revfComparer)

        //Find the index, call it s, just before the longest "tail" that is
        //ordered  in decreasing order ((s+1)..perm.Length-1).
        let rec tryFind i =
            if i = 0 then
                None
            elif (f perm.[i] perm.[i-1]) >= 0 then
                Some(i-1)
            else
                tryFind (i-1)

        match tryFind (perm.Length-1) with
        | Some s ->
            let sValue = perm.[s]
   
            //Change the value just before the tail (sValue) to the
            //smallest number bigger than it in the tail (perm.[t]).
            let rec find i imin =
                if i = perm.Length then
                    imin
                elif (f perm.[i] sValue) > 0 && (f perm.[i] perm.[imin]) < 0 then
                    find (i+1) i
                else
                    find (i+1) imin
                   
            let t = find (s+1) (s+1)
                   
            perm.[s] <- perm.[t]
            perm.[t] <- sValue

            //Sort the tail in increasing order.
            Array.Sort(perm, s+1, perm.Length - s - 1, fComparer)
            true
        | None ->
            false
      
    //yield copies of each perm
    seq {
        let e' = Seq.toArray e
        yield e'.[..k-1]
        while klexPermute e' do
            yield e'.[..k-1]
    }
                 
let klexPermsAsc k e = klexPerms compare k e
let klexPermsDesc k e = klexPerms (flip compare) k e

let lexPerms f e = klexPerms f (Seq.length e) e
let lexPermsAsc e = klexPermsAsc (Seq.length e) e
let lexPermsDesc e = klexPermsDesc (Seq.length e) e

///apply a total ordering of e by index position
let klexPermsAscIndexOrdering k e =
    e
    |> Seq.mapi (fun i item -> (i,item))
    |> klexPerms (fun (i,_) (j,_) -> compare i j) k
    |> Seq.map (fun arr -> arr |> Array.map (fun (_,item) -> item))

let lexPermsAscIndexOrdering e = klexPermsAscIndexOrdering (Seq.length e) e

//open Microsoft.FSharp.Collections

//let combinations e = seq {
//    
//
//}

//let allRotations startIndex (e:_[]) =
//    seq {
//        for i in 1..e.Length-startIndex-1 do
//            let e' = Array.copy e
//            Array.rotateRangeInPlace startIndex (e.Length - startIndex) i e'
//            yield e'
//    }
//
//let perms e = 
//    let rec perms startIndex (e:_[]) =
//        seq {
//            for i in startIndex..e.Length-2 do
//                for next in allRotations i e do
//                    yield next
//                    yield! perms (i+1) next
//        }
//    
//    seq { 
//        let e' = Seq.toArray e    
//        yield e'
//        yield! perms 0 e'
//    }
//


//let combinations e =
//    let combinationsWithDups =
//        e
//        |> Seq.collect (fun item -> Array.create (e |> Seq.length) item)
//        |> klexPermsAscIndexOrdering (e |> Seq.length)
//        |> Seq.pairwise
//        |> Seq.filter (fun (a,b) -> a <> b)
//
//    seq {
//        yield combinationsWithDups |> Seq.head |> fst
//        yield! combinationsWithDups |> Seq.map snd
//    }

//this is pretty good
//let combinations input =
//    let rec loop remaining current = seq {
//        match remaining with 
//        | [] -> ()
//        | hd::tail -> 
//            yield hd::current
//            yield! loop tail (hd::current)
//            yield! loop tail current
//    }
//    loop input []