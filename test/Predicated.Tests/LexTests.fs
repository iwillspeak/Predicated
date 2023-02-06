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
[<InlineData(",", TokenKind.Comma)>]
[<InlineData(".", TokenKind.Dot)>]
[<InlineData("  ", TokenKind.Space)>]
[<InlineData("\n\r\t ", TokenKind.Space)>]
[<InlineData("\"hello world\"", TokenKind.String)>]
[<InlineData("\'hello world\'", TokenKind.String)>]
[<InlineData("\"unterminated", TokenKind.Error)>]
[<InlineData("t", TokenKind.Ident)>]
[<InlineData("test", TokenKind.Ident)>]
[<InlineData("c3p0", TokenKind.Ident)>]
[<InlineData("or", TokenKind.Or)>]
[<InlineData("OR", TokenKind.Or)>]
[<InlineData("and", TokenKind.And)>]
[<InlineData("AND", TokenKind.And)>]
[<InlineData("andrewe", TokenKind.Ident)>]
[<InlineData("inordinate", TokenKind.Ident)>]
// TODO: number and date literals
let ``tokenise simple lexemes`` input expected =
    let (lexeme, kind) = input |> tokenise |> Seq.exactlyOne

    Assert.Equal(expected, kind)
    Assert.Equal(input, lexeme)

[<Fact>]
let ``tokenise multiple tokens`` () =
    let tokens =
        "123(3)"
        |> tokenise
        |> Seq.map (fun (_, b) -> b)
        |> Array.ofSeq

    Assert.Equal(
        [ TokenKind.Number
          TokenKind.OpenParen
          TokenKind.Number
          TokenKind.CloseParen ],
        tokens
    )
