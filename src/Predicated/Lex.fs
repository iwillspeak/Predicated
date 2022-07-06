module Predicated.Lex

open System
open System.Text

/// Token Kind Enum
///
/// Represents the different types of tokens that the lexer can produce. Each
/// token is represented as a pair of `(Kind, Lexeme)` where `Lexeme` is the raw
/// string that was matched in the input.
type public TokenKind =
    | Error = 0

    // Plain datum tokens
    | Number = 1
    | String = 2
    | Ident = 3
    | Date = 4

    | OpenParen = 5
    | CloseParen = 6

    // Comparison operator tokens
    | Equal = 7
    | Like = 8
    | Gt = 9
    | Lt = 10

    // Boolean grouping tokens
    | And = 11
    | Or = 12

    | Dot = 13

    // Whitespace
    | Space = 100
    | EOF = 101

/// The tokeniser state
///
/// Represents each point that the tokeniser state machine can be in as it
/// traverses the input text.
type private State =
    | Start
    | Space
    | Ident
    | InNumber
    | InString
    | SimpleToken of TokenKind

[<CompiledName("Tokenise")>]
let public tokenise input =

    let tokenForState lexeme state =
        let kind =
            match state with
            | Start -> TokenKind.Error
            | Ident ->
                if String.Equals(lexeme, "and", StringComparison.OrdinalIgnoreCase) then
                    TokenKind.And
                else if String.Equals(lexeme, "or", StringComparison.OrdinalIgnoreCase) then
                    TokenKind.Or
                else
                    TokenKind.Ident
            | Space -> TokenKind.Space
            | InNumber -> TokenKind.Number
            | SimpleToken kind -> kind
            | _ -> TokenKind.Error

        (lexeme, kind)

    let nextState char =
        function
        | Start ->
            match char with
            | c when Char.IsWhiteSpace(c) -> Some(Space)
            | c when Char.IsNumber(c) -> Some(InNumber)
            | c when Char.IsLetter(c) -> Some(Ident)
            | '.' -> Some(SimpleToken(TokenKind.Dot))
            | '"' -> Some(InString)
            | '(' -> Some(SimpleToken(TokenKind.OpenParen))
            | ')' -> Some(SimpleToken(TokenKind.CloseParen))
            | '=' -> Some(SimpleToken(TokenKind.Equal))
            | '~' -> Some(SimpleToken(TokenKind.Like))
            | '>' -> Some(SimpleToken(TokenKind.Gt))
            | '<' -> Some(SimpleToken(TokenKind.Lt))
            | _ -> Some(SimpleToken(TokenKind.Error))
        | InNumber ->
            match char with
            | c when Char.IsNumber(c) -> Some(State.InNumber)
            | _ -> None
        | InString ->
            match char with
            | '"' -> Some(SimpleToken(TokenKind.String))
            | _ -> Some(InString)
        | Space ->
            match char with
            | c when Char.IsWhiteSpace(c) -> Some(Space)
            | _ -> None
        | Ident ->
            match char with
            | c when Char.IsLetterOrDigit(c) -> Some(Ident)
            | _ -> None
        | _ -> None

    let mutable state = Start
    let mutable lexeme = StringBuilder()

    seq {
        for char in input do
            match nextState char state with
            | None ->
                yield tokenForState (lexeme.ToString()) state
                // HAXX: Assume we always return some state for start transitions
                lexeme <- lexeme.Clear().Append(char)
                state <- (nextState char Start).Value
            | Some next ->
                state <- next
                lexeme <- lexeme.Append(char)

        if state <> Start then
            yield tokenForState (lexeme.ToString()) state
    }
