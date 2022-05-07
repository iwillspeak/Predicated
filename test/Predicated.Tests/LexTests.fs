module ``lex tests``

open Predicated.Lex

open Xunit

[<Theory>]
[<InlineData("123", TokenKind.Number)>]
// TODO: Identifier, string, and date literals
[<InlineData("(", TokenKind.OpenParen)>]
[<InlineData(")", TokenKind.CloseParen)>]
[<InlineData("=", TokenKind.Equal)>]
[<InlineData("~", TokenKind.Like)>]
[<InlineData("<", TokenKind.Lt)>]
[<InlineData(">", TokenKind.Gt)>]
let ``tokenise simple lexemes`` input expected =
    let token = input |> tokenise |> Seq.exactlyOne

    Assert.Equal(expected, token)

[<Fact>]
let ``tokenise multiple tokens`` () =
    let tokens = "123(3)" |> tokenise |> Array.ofSeq

    Assert.Equal(
        [ TokenKind.Number
          TokenKind.OpenParen
          TokenKind.Number
          TokenKind.CloseParen ],
        tokens
    )
