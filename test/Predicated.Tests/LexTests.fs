module ``lex tests``

open Predicated.Lex

open Xunit

[<Theory>]
[<InlineData("123", TokenKind.Number)>]
[<InlineData("(", TokenKind.OpenParen)>]
[<InlineData(")", TokenKind.CloseParen)>]
[<InlineData("=", TokenKind.Equal)>]
[<InlineData("~", TokenKind.Like)>]
[<InlineData("<", TokenKind.Lt)>]
[<InlineData(">", TokenKind.Gt)>]
[<InlineData("  ", TokenKind.Space)>]
[<InlineData("\n\r\t ", TokenKind.Space)>]
[<InlineData("\"hello world\"", TokenKind.String)>]
[<InlineData("\"unterminated", TokenKind.Error)>]
[<InlineData("t", TokenKind.Ident)>]
[<InlineData("test", TokenKind.Ident)>]
[<InlineData("c3p0", TokenKind.Ident)>]
// TODO: number and date literals
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
