namespace Microsoft.FSharp.Collections
module Seq =
    let sortWith f e = 
        let e' = e |> Seq.toArray
        e' |> Array.sortInPlaceWith f
        e' |> Seq.readonly

    let inline infiniteRange start skip = 
        seq {
            let n = ref start
            while true do
                yield n.contents
                n.contents <- n.contents + skip
        }