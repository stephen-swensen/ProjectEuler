[<AutoOpen>]
module Swensen.Numerics

open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core.Operators //not sure why this needs to be opened, should be autoopened
open Microsoft.FSharp.Core.LanguagePrimitives

module Generic =
    ///Infinite Fibonacci sequence
    let inline fibseq_of (g:G<'a>) =    
        let rec fibseq n1 n2 = 
            seq { 
                let n0 = Checked.(+) n1 n2 
                yield n0
                yield! fibseq n0 n1 
            }
        seq { yield g.one ; yield g.one ; yield! (fibseq g.one g.one) }

    ///Finds the nth term of the Fibonacci sequence
    let inline fib_of (g:G<'a>) n = 
        (fibseq_of g) |> Seq.nth n
            
    ///Finds the square root (integral or floating point) of n
    ///Does not work with BigRational
    let inline sqrt_of (g:G<'a>) n =
        if g.zero = n then g.zero
        else
            let mutable s:'a = (n / g.two) + g.one
            let mutable t:'a = (s + (n / s)) / g.two
            while t < s do
                s <- t
                let step1:'a = n/s
                let step2:'a = s + step1
                t <- step2 / g.two
            s
                
    ///Computes n!
    let inline factorial_of (g:G<'a>) (n:'a) = 
        {g.one..n} |> Seq.fold (Checked.(*)) g.one
            
    ///Checks whether n is prime.
    let inline isPrime_of (g:G<'a>) n = 
        if n < g.two then false
        else
            let nsqrt = sqrt_of g n
            let rec isPrime i =  
                if i > nsqrt then true 
                elif n % i = g.zero then false
                else isPrime (i + g.one)
            isPrime g.two
            
    ///Prime factorization, from greatest to least.  For example, factorize 12 = [3;2;2].
    let inline factorize_of (g:G<'a>) n = 
        let rec factorize n j flist =  
            if n = g.one then flist 
            elif n % j = g.zero then factorize (n/j) j (j::flist) 
            else factorize n (j + g.one) (flist) 
        factorize n g.two []
        
    ///Canonical prime factorization.  For example, cfactorize 12 = [(2,2);(3,1)].
    //performance improvements over Seq.groupBy implementation is 500ms vs 600ms, or about a 20% gain
    let inline cfactorize_of (g:G<'a>) n =
        if n = g.one then []
        else
            let rec build (f::flist) count cflist =
                if flist = [] then
                    (f,count+1)::cflist
                elif f = flist.Head then
                    build flist (count + 1) cflist
                else 
                    build flist 0 ((f,count+1)::cflist)
            build (factorize_of g n) 0 []
            
//    let inline cfactorize n (g:G<'a>) =
//        factorize n g
//        |> Seq.groupBy id
//        |> Seq.map (fun (f,s) -> (f, Seq.length s))
            
    ///sigma divisor function (sum of divisors)
    let inline sigma_of (g:G<'a>) n =
        cfactorize_of g n
        |> Seq.map 
            (fun (p,e) -> 
                let numer:'a = (pown p (e+1) - g.one) //pown is Checked
                let denom:'a = (p-g.one)
                numer / denom)
        |> Seq.fold (*) g.one
        
    ///Sigma proper divisor function (sum of proper divisors)
    let inline psigma_of (g:G<'a>) n = (sigma_of g n) - n
        
    ///Convert n to a sequence of digits
    ///(perhaps an array or even list of digits would be better
    //let inline digits (g:G<'a>) (n:'a) = n |> string |> Seq.map (string >> int)
    //Make this an extension property, number.Digits, of type array, lazily initialized

    ///Absolute Value
    let inline abs_of (g:G<'a>) n = 
        if n < g.zero then n*g.negone else n
            
    ///Greatest Common Divisor
    ///n and m must be non-zero integers
    let inline gcd_of (g:G<'a>) n m =
        if n = g.zero || m = g.zero then 
            raise (System.ArgumentOutOfRangeException("n and m must non-zero"))
        else
            let rec gcd n m = 
                if n = m then n
                else
                    if n > m then gcd (n-m) m
                    else gcd n (m-n)
            gcd (abs_of g n) (abs_of g m)

    ///p is "prime", s=p*p, c is "multiplier", m=c*p
    type SievePrime<'a> = {mutable c:'a ; p:'a ; mutable m:'a ; s:'a}

    ///A cached, infinite sequence of primes
    let inline primes_of (g:G<'a>) =
        let primeList = ResizeArray<_>()
        primeList.Add({c=g.three ; p=g.three ; m=g.three*g.three ; s=g.three*g.three})

        //test whether n is composite, if not add it to the primeList and return false
        let isComposite n = 
            let rec loop i = 
                let sp = primeList.[i]
                while sp.m < n do
                    sp.c <- sp.c+g.one
                    sp.m <- sp.c*sp.p

                if sp.m = n then true
                elif i = (primeList.Count-1) || sp.s > n then
                    primeList.Add({c=n ; p=n ; m=n*n ; s=n*n}) //should use Checked(*)
                    false
                else loop (i+1)
            loop 0

        seq { 
            yield g.two ; yield g.three

            for i in 1..primeList.Count-1 do
                yield primeList.[i].p

            yield! Seq.infiniteRange (primeList.[primeList.Count-1].p + g.two) g.two 
                   |> Seq.filter (isComposite>>not)
        }

open Generic
//no generic versions of fib or fibseq, since only term of int32 provided
let fibseq  = fibseq_of gn
let fibseqL = fibseq_of gL
let fibseqI = fibseq_of gI

let fib  = fib_of gn
let fibL = fib_of gL
let fibI = fib_of gI

let inline factorialG n = factorial_of (G_of n) n
let factorial  = factorial_of gn
let factorialL = factorial_of gL
let factorialI = factorial_of gI

let inline sqrtG n = sqrt_of (G_of n) n
let sqrtn = sqrt_of gn //this has suffix "n" because sqrt is not strictly integral type
let sqrtL = sqrt_of gL
let sqrtI = sqrt_of gI
let sqrtF = sqrt_of gF
let sqrtM = sqrt_of gM
//let sqrtN = sqrt_of gN //never converges

let inline isPrimeG n = isPrime_of (G_of n) n
let isPrime  = isPrime_of gn
let isPrimeL = isPrime_of gL 
let isPrimeI = isPrime_of gI

let inline factorizeG n = factorize_of (G_of n) n
let factorize  = factorize_of gn
let factorizeL = factorize_of gL
let factorizeI = factorize_of gI

let inline cfactorizeG n = cfactorize_of (G_of n) n
let cfactorize  = cfactorize_of gn
let cfactorizeL = cfactorize_of gL
let cfactorizeI = cfactorize_of gI

let inline sigmaG n = sigma_of (G_of n) n
let sigma  = sigma_of gn
let sigmaL = sigma_of gL
let sigmaI = sigma_of gI

let inline psigmaG n = psigma_of (G_of n) n
let psigma  = psigma_of gn
let psigmaL = psigma_of gL
let psigmaI = psigma_of gI

let inline absG n = abs_of (G_of n) n
let absn = abs_of gn
let absL = abs_of gL
let absI = abs_of gI
let absF = abs_of gF
let absM = abs_of gM
let absN = abs_of gN
    
let inline gcdG n = gcd_of (G_of n) n
let gcd  = gcd_of gn
let gcdL = gcd_of gL
let gcdI = gcd_of gI

let primes = primes_of gn
let primesL = primes_of gL
let primesI = primes_of gI
    
//---------Generic operations that do not require numeric constraints
let truncF (n:float) = System.Math.Truncate(n)
let truncM (n:decimal) = System.Math.Truncate(n)
let truncN (n:bignum) = bignum.FromBigInt(bignum.ToBigInt(n))