let (|ID|) x = x

///compare lhs to rhs and return Some(rhs) if true
let (|CompareID|_|) comparer lhs rhs =
    if comparer lhs rhs then Some(rhs) else None
    
///compare lhs to rhs using the given comperer
let (|Compare|_|) comparer lhs rhs =
    if comparer lhs rhs then Some() else None
    
///compare lhs to rhs
//let (|Equals|NotEquals|LessThan|GreaterThan|LessThanOrEquals|GreaterThanOrEquals|) (lhs,rhs) =
//    match lhs with
//    | Compare (=)  rhs -> Equals
//    | Compare (<>) rhs -> NotEquals
//    | Compare (<)  rhs -> LessThan
//    | Compare (>)  rhs -> GreaterThan
//    | Compare (<=) rhs -> LessThanOrEquals
//    | Compare (>=) rhs -> GreaterThanOrEquals
    
let (|Equals|_|) rhs lhs = if lhs = rhs then Some() else None
let (|NotEquals|_|) rhs lhs = if lhs <> rhs then Some() else None
let (|LessThan|_|) rhs lhs = if lhs < rhs then Some() else None
let (|GreaterThan|_|) rhs lhs = if lhs > rhs then Some() else None
let (|LessThanOrEquals|_|) rhs lhs = if lhs <= rhs then Some() else None
let (|GreaterThanOrEquals|_|) rhs lhs = if lhs >= rhs then Some() else None

let pmatch (nlist:int list) (y:int) =
    let sum = List.sum nlist
    match sum with
    | LessThan y -> "gt"
    | _ -> sprintf "no match, sum=%d, y=%d" sum y


let divisorPairs p =
    let upper = sqrtn p
    let rec loop mn = 
        seq{ if mn <= upper then
                if p % mn = 0 then 
                    yield (mn,p/mn) 
                yield! loop (mn+1) }
    loop 2