module ``parser tests``

open Predicated.Parse
open Firethorn.Red.Debug
open Xunit

[<Fact>]
let ``stub parser test`` () =
    let result = parse "123 ( 20 )"

    debugDump (mappedFormatter SyntaxKinds.greenToAst) result.Tree
