namespace Swensen

///Contains several functions for converting to and from digit sequences represented by seq<int>.
module Digits =
    //at least twice as fast as uncheckedParse
    ///Convert an int64 to a sequence of its digits.
    let fromInt64 = function
        | 0L -> Seq.singleton 0
        | n ->
            let mutable n = if n < 0L then (abs n) else n
            let mutable powten = 0
                
            while (n/(pown 10L powten) >= 10L) do
                powten <- powten+1
                    
            let darr = Array.create (powten+1) 0
            let mutable i = 0
            while (powten >= 0) do
                let d = n/(pown 10L powten)
                darr.[i] <- d |> int
                n <- n - d*(pown 10L powten)
                i <- i + 1
                powten <- powten-1
                    
            Seq.readonly darr
                            
    //Convert an int to a sequence of its digits.
    let fromInt (n:int) = n |> int64 |> fromInt64
        
    ///Parse a positive or negative string of integers. If other characters (besides leading '-', which is skipped,
    ///and integers) are present results will be unpredictable. If null or empty, an empty sequence is returned.
    let uncheckedParse (nstr:string) =
        if System.String.IsNullOrEmpty nstr then Seq.empty
        else
            let charToInt = (fun c -> (int c) - 48) 
            match nstr.[0] with
            | '-' -> nstr |> Seq.skip 1 |> Seq.map charToInt
            | _ -> nstr |> Seq.map charToInt

    let private isDigit n = n >= 0 && n <= 10
        
    ///Parse a positive or negative string of integers, skipping all non-integer characters. If null or empty, an empty sequence is returned.
    let filteredParse nstr = 
        uncheckedParse nstr
        |> Seq.filter isDigit

    ///Try to parse a positive or negative string of integers. If other characters (besides leading '-', which is skipped, and integers) are
    ///present, returns None.  If null or empty also returns None.
    let tryParse nstr =
        let digs = uncheckedParse nstr
        if digs |> Seq.isEmpty |> not && digs |> Seq.forall isDigit then Some(digs)
        else None

    ///Try to parse a positive or negative string of integers. If other characters (besides leading '-', which is skipped, and integers) are
    ///present, raises ArgumentOutOfRangeException.  If null or empty also throws ArgumentOutOfRangeException.
    let parse nstr = 
        match tryParse(nstr) with
        | Some(digs) -> digs
        | None -> raise <| System.ArgumentOutOfRangeException("nstr", nstr, "is null or empty or contains characters other than leading '-' and integers")
        
    let fromBigInt (n:bigint) = n |> string |> uncheckedParse
    
    ///performs a checked conversion to int64            
    let toInt64 digs = 
        digs
        |> Seq.fold 
            (fun (e, sum) d -> let e = e-1 in (e, Checked.(+) sum ((d |> int64)*(pown 10L e))))
            (Seq.length digs, 0L)
        |> snd
                    
    ///note: (System.Int32.MinValue) |> Digits.fromInt |> Digits.toInt results
    ///in overflow exception since abs(int.MinValue) > int.MaxValue
    let toInt digs = 
        digs
        |> Seq.fold 
            (fun (e, sum) d -> let e = e-1 in (e, Checked.(+) sum (d*(pown 10 e))))
            (Seq.length digs, 0)
        |> snd
        
    open System.Text
    let toBigInt (digs:seq<int>) =         
        digs 
        |> Seq.fold (fun (sb:StringBuilder) d -> sb.Append(d)) (StringBuilder())
        |> string
        |> bigint.Parse


//assumming Digits.parse is good, the following is a good test for Seq.fromInt64 (which is a tricky one)
//([-1000L..1000L] |> List.map (Digits.fromInt64>>Seq.toList)) = ([-1000..1000] |> List.map string |> List.map (Digits.parse>>Seq.toList));;