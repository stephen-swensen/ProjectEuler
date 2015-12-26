namespace Swensen
module Seq =
    //('a -> 'b) -> seq<'a option> -> seq<'b>
    let mapSomeValues map input =
        input 
        |> Seq.filter Option.isSome
        |> Seq.map (map << Option.get)

    //('a -> 'b option) -> seq<'a> -> seq<'b>        
    let pickAll map input =
        input 
        |> Seq.map map
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
      
    //[Some(1); Some(2); None] |> Seq.mapSomeValues id