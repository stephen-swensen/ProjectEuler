module PrimeSieve
open Swensen
open Numerics

//current results
//all the nonSieves are slowest
//sieve4 is fast, but improved by sieve5's use of struct type inside a ref cell over a record type (but not that much better)
//jon harrop's primes is extremely fast and probably unbeatable

//3nd fastest
let nonSieve1 = Seq.initInfinite id |> Seq.filter isPrime
//1st fastest
let nonSieve2 = Seq.initInfinite (fun i -> 2*i + 1) |> Seq.filter isPrime

//2nd fastest
let nonSieve3 = 
    let rec loop n = seq {
        if isPrime n then 
            yield n
        
        yield! loop (n+1)
    }
    seq {yield 2 ; yield 3 ; yield 5 ; yield 7 ; yield 11 ; yield! loop 13}

//very fastests
let nonSieve4 =
    seq {
        yield 2
        let n = ref 3
        while true do
            if n.contents |> isPrime then
                yield n.contents

            n.contents <- n.contents+2
    }


type SieveDiscard = { mutable c : int ; p : int ; mutable product : int } 

//slow, despite best efforts -- euler-like sieve
let sieve1 =
    let rec sieve n (sdList:ResizeArray<_>) = seq {

        let isComposite = 
            let rec loop i = 
                let sd = sdList.[i]
                while sd.product < n do 
                    //printfn "looping (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                    sd.c <- sd.c+1
                    sd.product <- sd.c*sd.p
                //printfn "checking (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                if sd.product = n then true
                elif i = (sdList.Count-1) then false
                else loop (i+1)
            loop 0

        if not isComposite then 
            //printfn "found prime: %i, sdList.Count = %i\n" n sdList.Count
            sdList.Add({c=2 ; p=n ; product=2*n })
            yield n
        //else
            //printfn "found composite: %i, sdList.Count = %i\n" n sdList.Count

        yield! sieve (n+2) sdList
    }
    
    seq { 
        let sdList = ResizeArray<SieveDiscard>()
        //sdList.Add({c=2 ; p=2}) //don't need this since we skip multiples of 2 already
    
        yield 2 
        yield! sieve 3 sdList 
    }


//getting better but still not good enough
let sieve2 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<SieveDiscard>()
        sdList.Add({c=2 ; p=3 ; product=6})

        let isComposite n = 
            let rec loop i = 
                let sd = sdList.[i]
                while sd.product < n do 
                    //printfn "looping (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                    sd.c <- sd.c+1
                    sd.product <- sd.c*sd.p
                //printfn "checking (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                if sd.product = n then true
                elif i = (sdList.Count-1) then false
                else loop (i+1)
            loop 0

        let n = ref 5
        while(true) do 
            if not (isComposite n.contents) then 
                sdList.Add({c=2 ; p=n.contents ; product=2*n.contents})
                yield n.contents

            n.contents <- n.contents + 2
    }

//getting better but still not good enough
//not working right now, attempt at P*P failing
let sieve3 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<SieveDiscard>()
        sdList.Add({c=2 ; p=3 ; product=6})

        let isComposite n = 
            let rec loop i = 
                let sd = sdList.[i]
                while sd.product < n do 
                    //printfn "looping (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                    sd.c <- sd.c+1
                    sd.product <- sd.c*sd.p
                //printfn "checking (n=%i): sd.c = %i, sd.p = %i, (sd.c*sd.p)=%i" n sd.c sd.p (sd.c*sd.p)
                if sd.product = n then true
                elif i = (sdList.Count-1) then false
                elif 2*sd.p > n then false //rhs opt: -> 2*(next p) > n, get better?
                else loop (i+1)
            loop 0

        let n = ref 5
        while(true) do 
            if not (isComposite n.contents) then 
                sdList.Add({c=2 ; p=n.contents ; product=2*n.contents})
                yield n.contents

            n.contents <- n.contents + 2
    }

let sieve4 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<SieveDiscard>()
        sdList.Add({c=3 ; p=3 ; product=9})

        let isComposite n = 
            let rec loop i = 
                let sd = sdList.[i]
                while sd.product < n do 
                    sd.c <- sd.c+1
                    sd.product <- sd.c*sd.p

                if sd.product = n then true
                elif i = (sdList.Count-1) then false
                elif sd.p*sd.p > n then false //since we know that let sd' = sd[i+1] in sd'.product >= sd'.p*sd'.p
                else loop (i+1)
            loop 0

        let n = ref 5
        while(true) do 
            if not (isComposite n.contents) then 
                sdList.Add({c=n.contents ; p=n.contents ; product=n.contents*n.contents})
                yield n.contents

            n.contents <- n.contents + 2
    }

//still good (and more functional), but a bout a second slower than seive4 when calculating the 1 millionth prime 
let sieve4v2 =
    let isComposite n (sdList:ResizeArray<_>) = 
        let rec loop i = 
            let sd = sdList.[i]
            while sd.product < n do 
                sd.c <- sd.c+1
                sd.product <- sd.c*sd.p

            if sd.product = n then true
            elif i = (sdList.Count-1) then false
            elif sd.p*sd.p > n then false //since we know that let sd' = sd[i+1] in sd'.product >= sd'.p*sd'.p
            else loop (i+1)
        loop 0

    let rec primes n sdList = seq {
        if not (isComposite n sdList) then 
            sdList.Add({c=n ; p=n ; product=n*n})
            yield n

        yield! primes (n+2) sdList
    }
    
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<SieveDiscard>()
        sdList.Add({c=3 ; p=3 ; product=9})
        yield! primes 5 sdList
    }

[<Struct>]
type SievePrime =
    val mutable prime : int
    val mutable primeSquared : int
    val mutable product : int
    val mutable multiplier : int
    new(_prime) = {
        prime = _prime
        primeSquared = _prime*_prime
        multiplier = _prime
        product = _prime*_prime
    }
    member x.Multiplier 
        with get() = x.multiplier
        and set(_multiplier) = 
            x.multiplier <- _multiplier 
            x.product <-  x.prime * _multiplier
    member x.IncrementMultiplier() =
        x.Multiplier <- x.multiplier + 1

let sieve5 =
    seq { 
        yield 2 ; yield 3

        let spList = ResizeArray<_>()
        spList.Add(ref (SievePrime(3))) //don't need to add 2 since we always skip multiples of 2

        let isComposite n = 
            let rec loop i = 
                let sp = spList.[i]
                while sp.contents.product < n do 
                    sp.contents.IncrementMultiplier()
                if sp.contents.product = n then true
                elif i = (spList.Count-1) then false
                elif sp.contents.primeSquared > n then false //since we know that let sp' = sp[i+1] in sp'.product >= sp'.p*sp'.p
                else loop (i+1)
            loop 0

        let n = ref 5
        while(true) do 
            if not (isComposite n.contents) then 
                spList.Add(ref (SievePrime(n.contents)))
                yield n.contents

            n.contents <- n.contents + 2
    }

let sieve5Cached =
    let isComposite n (spList:ResizeArray<ref<SievePrime>>) = 
        let rec loop i = 
            let sp = spList.[i]
            while sp.contents.product < n do 
                sp.contents.IncrementMultiplier()
            if sp.contents.product = n then true
            elif i = (spList.Count-1) then false
            elif sp.contents.primeSquared > n then false //since we know that let sp' = sp[i+1] in sp'.product >= sp'.p*sp'.p
            else loop (i+1)
        loop 0

    let spList = ResizeArray<_>()
    spList.Add(ref (SievePrime(3))) //don't need to add 2 since we always skip multiples of 2

    seq { 
        yield 2

        for i in {0..spList.Count-1} do
            yield spList.[i].contents.prime

        let n = ref (spList.[spList.Count-1].contents.prime + 2)
        while(true) do 
            if not (isComposite n.contents spList) then 
                spList.Add(ref (SievePrime(n.contents)))
                yield n.contents

            n.contents <- n.contents + 2
    }

let sieve6 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<_>()
        sdList.Add((3,9))

        let isComposite n = 
            let rec loop i = 
                match sdList.[i] with
                | (a,_) when n % a = 0 -> true
                | _ when i = (sdList.Count-1) -> false
                | (_,b) when b > n -> false
                | _ -> loop (i+1)
            loop 0

        let n = ref 5
        while(true) do 
            if not (isComposite n.contents) then 
                sdList.Add(n.contents, n.contents*n.contents)
                yield n.contents

            n.contents <- n.contents + 2
    }

let inline infiniteRange start skip = 
    seq {
        let n = ref start
        while true do
            yield n.contents
            n.contents <- n.contents + skip
    }

let infiniteRange2 start skip = 
    Seq.initInfinite (fun i -> i*skip + start)

open System.Collections.Generic
open System.Collections

let infiniteRange3 start skip : seq<_> = 
    let cur = ref start 

    let getEnumerator start = 
        { new IEnumerator<'T> with 
              member x.Current = cur.contents
          interface IEnumerator with 
              member x.Current = box cur.contents
              member x.MoveNext() = 
                  cur.contents <- cur.contents + skip
                  true
              member x.Reset() = cur.contents <- start
          interface System.IDisposable with 
              member x.Dispose() = ()  } 

    { new System.Collections.Generic.IEnumerable<'T> with 
          member x.GetEnumerator() = (getEnumerator start)
      interface IEnumerable with 
          member x.GetEnumerator() = ((getEnumerator start) :> IEnumerator) }


let sieve7 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<_>()
        sdList.Add((3,9))

        let isComposite n = 
            let rec loop i = 
                match sdList.[i] with
                | (a,_) when n % a = 0 -> true
                | _ when i = (sdList.Count-1) -> false
                | (_,b) when b > n -> false
                | _ -> loop (i+1)
            loop 0

        yield! infiniteRange 5 2
               |> Seq.choose (fun n ->
                   match isComposite n with
                   | true -> None
                   | false -> sdList.Add(n,n*2) ; Some(n)
               )
    }

type SievePrime7 = {pair:int*int}

let sieve7p2 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<_>()
        sdList.Add({pair=(3,9)})

        let isComposite n = 
            let rec loop i = 
                match sdList.[i].pair with
                | (a,_) when n % a = 0 -> true
                | _ when i = (sdList.Count-1) -> false
                | (_,b) when b > n -> false
                | _ -> loop (i+1)
            loop 0

        yield! infiniteRange 5 2
               |> Seq.choose (fun n ->
                   match isComposite n with
                   | true -> None
                   | false -> sdList.Add({pair=(n,n*2)}) ; Some(n)
               )
    }

let sieve8 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<_>()
        sdList.Add(3,9)

        let isComposite n = 
            let rec loop i = 
                match sdList.[i] with
                | (a,_) when n % a = 0 -> true
                | _ when i = (sdList.Count-1) -> false
                | (_,b) when b > n -> false
                | _ -> loop (i+1)
            loop 0

        for n in infiniteRange 5 2 do
            if isComposite n |> not then 
                sdList.Add(n,n*n)
                yield n
    }

//not bad, and most functional solution
//not sure why sieve8 is so much slower
let sieve9 =
    seq { 
        yield 2 ; yield 3

        let sdList = ResizeArray<_>()
        sdList.Add(3,9)

        let isComposite n = 
            let rec loop i = 
                match sdList.[i] with
                | (a,_) when n % a = 0 -> true
                | _ when i = (sdList.Count-1) -> false
                | (_,b) when b > n -> false
                | _ -> loop (i+1)
            loop 0

        yield! infiniteRange 5 2 |> Seq.filter 
            (fun n ->
                match isComposite n with
                | false -> sdList.Add(n,n*n) ; true
                | true -> false)
    }

let sieve10 =
    seq { 
        yield 2 ; yield 3

        let primeList = ResizeArray<_>()
        primeList.Add(3,9)

        let isComposite n = 
            let rec loop i = 
                match primeList.[i] with
                | (a,_) when n % a = 0 -> true
                | _ when i = (primeList.Count-1) -> primeList.Add(n,n*n) ; false
                | (_,b) when b > n -> primeList.Add(n,n*n) ; false
                | _ -> loop (i+1)
            loop 0

        yield! infiniteRange 5 2 |> Seq.filter (isComposite>>not)
    }

//finalist; favorite (most functional) implementation, not too much slower than sieve4
let sieve10cached =
    let primeList = ResizeArray<_>()
    primeList.Add(3,9)

    let isComposite n = 
        let rec loop i = 
            match primeList.[i] with
            | (a,_) when n % a = 0 -> true
            | _ when i = (primeList.Count-1) -> primeList.Add(n,n*n) ; false
            | (_,b) when b > n -> primeList.Add(n,n*n) ; false
            | _ -> loop (i+1)
        loop 0

    seq { 
        yield 2 ; yield 3

        for i in 1..primeList.Count-1 do
            yield fst primeList.[i]

        yield! infiniteRange ((fst primeList.[primeList.Count-1]) + 2) 2 
               |> Seq.filter (isComposite>>not)
    }

//acutally faster (and simpler) than sieve10cached
let sieve11cached =
    let primeList = ResizeArray<_>()
    primeList.Add(3)

    let isComposite n = 
        let rec loop i = 
            match primeList.[i] with
            | p when n % p = 0 -> true
            | _ when i = (primeList.Count-1) -> primeList.Add(n) ; false
            | p when p*p > n -> primeList.Add(n) ; false
            | _ -> loop (i+1)
        loop 0

    seq { 
        yield 2 ; yield 3

        for i in 1..primeList.Count-1 do
            yield primeList.[i]

        yield! infiniteRange ((primeList.[primeList.Count-1]) + 2) 2 
               |> Seq.filter (isComposite>>not)
    }

//leading candiate
let sieve12cached =
    let primeList = ResizeArray<_>()
    primeList.Add(3)

    let isComposite n = 
        let rec loop i = 
            let p = primeList.[i]
            if n%p=0 then true
            elif i = (primeList.Count-1) || p*p > n then
                 primeList.Add(n)
                 false
            else loop (i+1)
        loop 0

    seq { 
        yield 2 ; yield 3

        for i in 1..primeList.Count-1 do
            yield primeList.[i]

        yield! infiniteRange ((primeList.[primeList.Count-1]) + 2) 2 
               |> Seq.filter (isComposite>>not)
    }


//basically sieve4, but with refactoring and caching of the 6-11 series; pretty fast, and looks good enough

///p is "prime", s=p*p, c is "multiplier", m=c*p
type SP13 = {mutable c:int ; p:int ; mutable m:int ; s:int}

let sieve13cached =
    let primeList = ResizeArray<_>()
    primeList.Add({c=3 ; p=3 ; m=9 ; s=9})

    //test whether n is composite, if not add it to the primeList and return false
    let isComposite n = 
        let rec loop i = 
            let sp = primeList.[i]
            while sp.m < n do
                sp.c <- sp.c+1
                sp.m <- sp.c*sp.p

            if sp.m = n then true
            elif i = (primeList.Count-1) || sp.s > n then
                 primeList.Add({c=n ; p=n ; m=n*n ; s=n*n})
                 false
            else loop (i+1)
        loop 0

    seq { 
        yield 2 ; yield 3

        for i in 1..primeList.Count-1 do
            yield primeList.[i].p

        yield! infiniteRange (primeList.[primeList.Count-1].p + 2) 2 
               |> Seq.filter (isComposite>>not)
    }

///p is "prime", s=p*p, c is "multiplier", m=c*p
type SP14 =
    val mutable c: int
    val mutable p: int
    val mutable m: int
    val mutable s: int
    new(_prime:int, _primeSquared:int) = {c = _prime ; p = _prime ; m = _primeSquared ; s = _primeSquared}

//for some reason slower than sieve13cached
let sieve14cached =
    let primeList = ResizeArray<_>()
    primeList.Add(SP14(3,9))

    //test whether n is composite, if not add it to the primeList and return false
    let isComposite n = 
        let rec loop i = 
            let sp = primeList.[i]
            while sp.m < n do
                sp.c <- sp.c+1
                sp.m <- sp.c*sp.p

            if sp.m = n then true
            elif i = (primeList.Count-1) || sp.s > n then
                 primeList.Add(SP14(n,n*n))
                 false
            else loop (i+1)
        loop 0

    seq { 
        yield 2 ; yield 3

        for i in 1..primeList.Count-1 do
            yield primeList.[i].p

        yield! infiniteRange (primeList.[primeList.Count-1].p + 2) 2 
               |> Seq.filter (isComposite>>not)
    }

//
//let takeToList n s = s |> Seq.take n  |> Seq.toList
//
//let x = Seq.iteri 
//let y = Seq.mapi
//let z = Seq.nth

//not good right now
let finiteSieve n =
    let arr = Array.create (n*n) true

    let mutable i = 2
    while i < arr.Length do
        if arr.[i] = true then
            let mutable j = 2
            while (j*i) < arr.Length do
                arr.[j*i] <- false
                j <- j+1
        i <- i+1
    
    seq {
        let i = ref 2
        let count = ref 0
        while i.contents < arr.Length && count.contents < n do
            if arr.[i.contents] then
                count.contents <- count.contents + 1 
                yield i.contents

            i.contents <- i.contents+1
    }
//extremely fast
//flying frog
let primes =
    let a = ResizeArray[2]

    let grow() =
      let p0 = a.[a.Count-1]+1
      let b = Array.create p0 true

      for di in a do
        let rec loop i =
          if i<b.Length then
            b.[i] <- false
            loop(i+di)

        let i0 = p0/di*di
        loop(if i0<p0 then i0+di-p0 else i0-p0)

      for i=0 to b.Length-1 do
        if b.[i] then a.Add(p0+i)

    Seq.initInfinite 
        (fun n ->
              while n >= a.Count do
                grow()
              a.[n])
    
module FinalSieve =
    let inline infiniteRange start skip = 
        seq {
            let n = ref start
            while true do
                yield n.contents
                n.contents <- n.contents + skip
        }

    ///p is "prime", s=p*p, c is "multiplier", m=c*p
    type SievePrime<'a> = {mutable c:'a ; p:'a ; mutable m:'a ; s:'a}

    ///A cached, infinite sequence of primes
    let primes =
        let primeList = ResizeArray<_>()
        primeList.Add({c=3 ; p=3 ; m=9 ; s=9})

        //test whether n is composite, if not add it to the primeList and return false
        let isComposite n = 
            let rec loop i = 
                let sp = primeList.[i]
                while sp.m < n do
                    sp.c <- sp.c+1
                    sp.m <- sp.c*sp.p

                if sp.m = n then true
                elif i = (primeList.Count-1) || sp.s > n then
                    primeList.Add({c=n ; p=n ; m=n*n ; s=n*n})
                    false
                else loop (i+1)
            loop 0

        seq { 
            yield 2 ; yield 3

            for i in 1..primeList.Count-1 do
                yield primeList.[i].p

            yield! infiniteRange (primeList.[primeList.Count-1].p + 2) 2 
                   |> Seq.filter (isComposite>>not)
        }
        
//a purely function, completely ungodly implementations
module PrimeSieveImmutable =
    ///p is "prime", s=p*p, c is "multiplier", m=c*p
    type SievePrime = {c:int ; p:int ; m:int ; s:int}

    type status =
        | Prime
        | Composite
        | Unknown
        | Start
        | End

    let updateMap (mp:Map<int,SievePrime>) n prevPrime = 
        mp 
        |> Seq.scan
            (fun (prevState, _) kv ->
                let p,sp = kv.Key,kv.Value
                
                match prevState with
                | End | Composite | Prime -> (End, None)
                | Start | Unknown ->
                    let rec loop sp = 
                        if sp.m < n then loop {sp with c=sp.c+1; m=(sp.c+1)*sp.p}
                        else sp
                
                    let sp' = loop sp
                    let spChange = if sp = sp' then None else Some(sp')
                
                    if sp'.m = n then 
                        (Composite, spChange)
                    elif sp'.p = prevPrime || sp'.s > n then 
                        (Prime, spChange)
                    else
                        (Unknown, spChange))
            (Start,None)
        |> Seq.takeWhile (fst>>(function End -> false | _ -> true))
        |> Seq.fold
            (fun (isPrime,mp) (state,spChange) ->                
                let mp =
                    match spChange with
                    | Some(sp) -> mp |> Map.remove sp.p |> Map.add sp.p sp
                    | None -> mp

                match state with
                | Prime -> (true, mp |> Map.add n {c=n ; p=n ; m=n*n ; s=n*n})
                | _ -> (false, mp))
            (false, mp)

    let primes =
        let rec primes mp n prevPrime = seq {
            match updateMap mp n prevPrime with
            | true, mp -> yield n ; yield! primes mp (n+2) n
            | false, mp -> yield! primes mp (n+2) prevPrime
        }
        seq {
            yield 2 ; yield 3
            yield! primes (Map.empty |> Map.add 3 {c=3 ; p=3 ; m=9 ; s=9}) 5 3
        }