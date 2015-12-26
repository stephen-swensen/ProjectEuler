module DigitsTests

open Swensen
open Swensen.Unquote
open Xunit
open Xunit.Extensions

module XunitHelpers =
    ///xUnit.net's Xunit.Extensions.PropertyDataAttribute requires a property of type #seq<obj[]>, therefore this function
    ///converts a value of type #seq<'a> to a value of type #seq<obj[]>.
    let toPropertyData input =
        input |> Seq.map (fun i -> [|box i|])

//Using nested module allows us to gives us error messages like "DigitsTests+UncheckedParse.allows empty string"
module UncheckedParse =
    [<Fact>] //start with the easy cases, vanilla single assertion test
    let ``allows empty string`` () =
        //we leverage structural equality of lists
        test <@ Digits.uncheckedParse "" |> Seq.toList = [] @>

    [<Fact>]
    let ``allows null string`` () =
        test <@ Digits.uncheckedParse null |> Seq.toList = [] @>

    //these are used for PropertyData tests
    let positiveSingleDigitStrings = [0..9] |> List.map string |> XunitHelpers.toPropertyData
    let negativeSingleDigitStrings = [-9..-1] |> List.map string |> XunitHelpers.toPropertyData

    [<Theory>] //needed for PropetyData tests
    [<PropertyData("positiveSingleDigitStrings")>] //generates independent test case for each element in given property sequence
    let ``handles positive single digit`` positiveSingleDigitString =
        test <@ positiveSingleDigitString |> Digits.uncheckedParse |> Seq.toList = [System.Int32.Parse(positiveSingleDigitString)] @>

    [<Theory>]
    [<PropertyData("negativeSingleDigitStrings")>]
    //the biggest advantage of ``test description`` style test function names is not use of whitespace in my opinion, I've never had a problem with
    //camleCase, rather the advantage is being able to use other symbols and punctions like follows.
    let ``skips leading '-' in negative single digit`` negativeSingleDigitString =
        test <@ negativeSingleDigitString |> Digits.uncheckedParse |> Seq.toList = [System.Int32.Parse(negativeSingleDigitString) |> abs] @>

    [<Fact>]
    let ``handles big positive number`` () =
        test <@ Digits.uncheckedParse "7340982374098132740987243908" |> Seq.toList = [7;3;4;0;9;8;2;3;7;4;0;9;8;1;3;2;7;4;0;9;8;7;2;4;3;9;0;8] @>

    [<Fact>]
    let ``handles big negative number`` () =
        test <@ Digits.uncheckedParse "-7340982374098132740987243908" |> Seq.toList = [7;3;4;0;9;8;2;3;7;4;0;9;8;1;3;2;7;4;0;9;8;7;2;4;3;9;0;8] @>

    [<Fact>] 
    let ``allows non-integer characters but result is unpredictable`` () =
        test <@ Digits.uncheckedParse "a12b32c32" |> ignore ; true @> //we use a sequence expression to ignore the result and return true

//marking inline hides this function from xUnit.net test failure stacktraces, so the test function itself is prominant