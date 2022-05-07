module Tests

open Predicated.Lex

open System
open Xunit

[<Theory>]
[<InlineData("123", TokenKind.Number)>]
[<InlineData("(", TokenKind.OpenParen)>]
[<InlineData(")", TokenKind.CloseParen)>]
[<InlineData("=", TokenKind.Equal)>]
[<InlineData("~", TokenKind.Like)>]
[<InlineData("<", TokenKind.Lt)>]
[<InlineData(">", TokenKind.Gt)>]
let ``My test`` input expected =
    let token = input |> tokenise |> Seq.exactlyOne

    Assert.Equal(expected, token)

[<Fact>]
let ``Complex Tokeniser`` () =
    let tokens = "123(3)" |> tokenise |> Array.ofSeq

    Assert.Equal(
        [ TokenKind.Number
          TokenKind.OpenParen
          TokenKind.Number
          TokenKind.CloseParen ],
        tokens
    )
