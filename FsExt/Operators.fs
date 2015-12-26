namespace Microsoft.FSharp.Core
//perhaps should extend ExtraTopLevelOperators instead
module Operators =
    let flip f x y = f y x
                
    ///Convert the given function to an IComparer<'a>
    let comparer f = 
        { new System.Collections.Generic.IComparer<_> with 
            member self.Compare(x,y) = f x y }
    
    //perhaps should be in differnt project, like utils
    ///returns the average number of milleseconds elapse executing f() with the given sampleSize.
    ///typically calls f once (jit call) before executing sample loop, but if sampleSize=0, then 
    ///sampleSize is considered to be 1 with no jit call
    let time sampleSize f = 
        if sampleSize < 0 then failwith "sampleSize must be greater than 0"

        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        
        if sampleSize > 0 then f() //jit
        let sampleSize = if sampleSize = 0 then 1 else sampleSize

        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        for i in 1..sampleSize do f()
        sw.Stop()
        sw.ElapsedMilliseconds / (int64 sampleSize)
        
    ///No-op, i.e. do nothing
    let noop x = ()

    ///i.e. printf "%A" x
    let print x = printf "%A" x

    ///i.e. printfn "%A" x
    let printn x = printfn "%A" x

    //http://stackoverflow.com/questions/833180/handy-f-snippets/851449#851449
    let memoize f = 
        let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        fun x ->
            match cache.TryGetValue(x) with
            | true, res -> res
            | _ -> 
                let res = f x
                cache.[x] <- res
                res

    //http://stackoverflow.com/questions/833180/handy-f-snippets/1477188#1477188
    let (=~) input pattern = System.Text.RegularExpressions.Regex.IsMatch(input, pattern)
    
    //original raise is not inlined in Core.Operators, so (sometimes) shows up in stack traces.  we inline it here
    ///Raises an exception. Inlined so that it doesn't show up in stack traces.
    let inline raise (e: System.Exception) = (# "throw" e : 'U #)