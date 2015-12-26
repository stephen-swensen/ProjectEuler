module TestBugs
//module Test
//
//let inline zero_of (target:'a) : 'a = LanguagePrimitives.GenericZero<'a>
//let inline one_of (target:'a) : 'a = LanguagePrimitives.GenericOne<'a>
//let inline two_of (target:'a) : 'a = one_of(target) + one_of(target)
//let inline three_of (target:'a) : 'a = two_of(target) + one_of(target)
//let inline negone_of (target:'a) : 'a = zero_of(target) - one_of(target)
//
//let inline any_of (target:'a) (x:int) : 'a = 
//    let one:'a = one_of target
//    let zero:'a = zero_of target
//    let xu = if x > 0 then 1 else -1
//    let gu:'a = if x > 0 then one else zero-one
//
//    let rec get i g = 
//        if i = x then g
//        else get (i+xu) (g+gu)
//    get 0 zero 
//    
//type G<'a> = {
//    negone:'a
//    zero:'a
//    one:'a
//    two:'a
//    three:'a
//    any: int -> 'a
//}    
//
//let inline G_of (target:'a) : (G<'a>) = {
//    zero = zero_of target
//    one = one_of target
//    two = two_of target
//    three = three_of target
//    negone = negone_of target
//    any = any_of target
//}
//
////let inline factorizeG n = 
////    let g = G_of n
////    let rec factorize n j flist =  
////        if n = g.one then flist 
////        elif n % j = g.zero then factorize (n/j) j (j::flist) 
////        else factorize n (j + g.one) (flist) 
////    factorize n g.two []
//
//let inline factorize (g:G<'a>) n = 
//    let rec factorize n j flist =  
//        if n = g.one then flist 
//        elif n % j = g.zero then factorize (n/j) j (j::flist) 
//        else factorize n (j + g.one) (flist) 
//    factorize n g.two []
//    
////identical to our earlier factorizeG
//let inline factorizeG n = factorize (G_of n) n
//
//let gn = G_of 1  //int32
//let gL = G_of 1L //int64
//let gI = G_of 1I //bigint
//
////allow us to limit to only integral numeric types
//let factorizeL = factorize gL
//let factorizeI = factorize gI
//let factorizen = factorize gn
//
//
////---
//
//let time (lz:Lazy<'a>) : unit =
//    System.GC.Collect()
//    System.GC.WaitForPendingFinalizers()
//    let sw = System.Diagnostics.Stopwatch()
//    sw.Start()
//    ignore (lz.Force())
//    sw.Stop()
//    printfn "time: %s ms" (sw.ElapsedMilliseconds.ToString())
//
////time (lazy ([1L..100000L] |> List.map (fun n -> factorize gL n))) //slow
////time (lazy ([1L..100000L] |> List.map (fun n -> factorize (G_of 1L) n))) //fast
//
//let slow = [1L..100000L] |> List.map (fun n -> factorize gL n)
//let fast = [1L..100000L] |> List.map (fun n -> factorize (G_of 1L) n)

open System
open System.Diagnostics

let inline zero_of (target:'a) : 'a = LanguagePrimitives.GenericZero<'a>
let inline one_of (target:'a) : 'a = LanguagePrimitives.GenericOne<'a>
let inline two_of (target:'a) : 'a = one_of(target) + one_of(target)
let inline three_of (target:'a) : 'a = two_of(target) + one_of(target)
let inline negone_of (target:'a) : 'a = zero_of(target) - one_of(target)

let inline any_of (target:'a) (x:int) : 'a = 
    let one:'a = one_of target
    let zero:'a = zero_of target
    let xu = if x > 0 then 1 else -1
    let gu:'a = if x > 0 then one else zero-one

    let rec get i g = 
        if i = x then g
        else get (i+xu) (g+gu)
    get 0 zero 
    
type G<'a> = {
    negone:'a
    zero:'a
    one:'a
    two:'a
    three:'a
    any: int -> 'a
}    

let inline G_of (target:'a) : (G<'a>) = {
    zero = zero_of target
    one = one_of target
    two = two_of target
    three = three_of target
    negone = negone_of target
    any = any_of target
}

//let inline factorizeG n = 
//    let g = G_of n
//    let rec factorize n j flist =  
//        if n = g.one then flist 
//        elif n % j = g.zero then factorize (n/j) j (j::flist) 
//        else factorize n (j + g.one) (flist) 
//    factorize n g.two []

let inline factorize1 (g:G<'a>) n = 
    let rec factorize n j flist =  
        if n = g.one then flist 
        elif n % j = g.zero then factorize (n/j) j (j::flist) 
        else factorize n (j + g.one) (flist) 
    factorize n g.two []

let (|Integral|_|) (n:obj) = 
    match n with
    | :? int32 | :? int64 | :? bigint -> Some()
    | _ -> None
    
let inline factorize2 n = 
    let g = G_of n
    match n with
    | _ when n < g.one -> raise (ArgumentOutOfRangeException("n"))
    | Integral ->
        let rec factorize n j flist =  
            if n = g.one then flist 
            elif n % j = g.zero then factorize (n/j) j (j::flist) 
            else factorize n (j + g.one) (flist) 
        factorize n g.two []
    | _ -> raise (ArgumentException("n is non-integral"))

let test1() =
    [1L..10000L] |> List.map (fun n -> factorize1 (G_of 1L) n)

let test2() =
    [1L..10000L] |> List.map factorize2
    
test1() |> ignore //warmup
test2() |> ignore //warmup

Console.WriteLine(factorize1 (G_of 1.) 22234.)





//System.GC.Collect()
//System.GC.WaitForPendingFinalizers()
//let sw1 = Stopwatch.StartNew()
//test1() |> ignore
//sw1.Stop()
//Console.WriteLine("test1 {0}ms", sw1.ElapsedMilliseconds)
//
//System.GC.Collect()
//System.GC.WaitForPendingFinalizers()
//let sw2 = Stopwatch.StartNew()
//test2() |> ignore
//sw2.Stop()
//Console.WriteLine("test2 {0}ms", sw2.ElapsedMilliseconds)
//Console.WriteLine("test1/test2: {0}", (sw1.ElapsedMilliseconds|>float)/(sw2.ElapsedMilliseconds|>float))
//
//Console.ReadLine() |> ignore
//
//let time (lz:Lazy<'a>) : unit =
//    System.GC.Collect()
//    System.GC.WaitForPendingFinalizers()
//    let sw = System.Diagnostics.Stopwatch()
//    sw.Start()
//    ignore (lz.Force())
//    sw.Stop()
//    printfn "time: %s ms" (sw.ElapsedMilliseconds.ToString())
//
//open Microsoft.FSharp.Core.LanguagePrimitives
//let tmp = G_of 1L
//time (lazy ([1L..100000L] |> List.map GMath.factorizeG))
//time (lazy ([1L..100000L] |> List.map (fun n -> GMath.Generic.factorize (tmp) n))) //slow
//time (lazy ([1L..100000L] |> List.map (fun n -> GMath.Generic.factorize (G_of 1L) n))) //fast


let rec permutations2 list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations2 list (Set.add l taken) do
              yield l::perm }
    
let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)
    
          
//time 50 (fun () -> (permutations2 [1;2;3;4;5;6;7] Set.empty) |> Seq.iter (fun _ -> ()));;
//time 50 (fun () -> (permutationsAsc [1;2;3;4;5;6;7]) |> Seq.iter (fun _ -> ()));;
//time 50 (fun () -> (perms [1;2;3;4;5;6;7;9;10]) |> Seq.iter (fun _ -> ()));;

