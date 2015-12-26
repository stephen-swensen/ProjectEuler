//http://stackoverflow.com/questions/2840714/f-static-member-type-constraints
namespace Microsoft.FSharp.Core
open System.Numerics

module LanguagePrimitives =

    let inline zero_of (target:'a) : 'a = LanguagePrimitives.GenericZero<'a>
    let inline one_of (target:'a) : 'a = LanguagePrimitives.GenericOne<'a>
    let inline two_of (target:'a) : 'a = one_of(target) + one_of(target)
    let inline three_of (target:'a) : 'a = two_of(target) + one_of(target)
    let inline negone_of (target:'a) : 'a = zero_of(target) - one_of(target)
    let inline parse_of< ^a when ^a : (static member Parse: string -> ^a)> (target: ^a) (str: string) = 
        (^a : (static member Parse : string -> ^a) (str))
    
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
        parse: string -> 'a
    }    

    let inline G_of (target:'a) : (G<'a>) = {
        zero = zero_of target
        one = one_of target
        two = two_of target
        three = three_of target
        negone = negone_of target
        any = any_of target
        parse = parse_of target
    }

    let gn = G_of 1   //int32
    let gL = G_of 1L  //int64
    let gI = G_of 1I  //bigint
    let gF = G_of 1.0 //float 
    let gM = G_of 1.0M//decimal
    let gN = G_of 1N//decimal
    //big rational?