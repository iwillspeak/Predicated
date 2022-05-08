module ``parser tests``

open Predicated.Parse
open Firethorn.Red.Debug
open Xunit

[<Fact>]
let ``stub parser test`` () =
    let result = parse "title = \"hello world\" AND NOT size < 100"

    debugDump (mappedFormatter SyntaxKinds.greenToAst) result.Tree
