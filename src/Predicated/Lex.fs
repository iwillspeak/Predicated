module Predicated.Lex

open System
open System.Text

type public TokenKind =
    | Error = 0

    | Number = 1
    | String = 2
    | Ident = 3
    | Date = 4

    | OpenParen = 5
    | CloseParen = 6
    | Equal = 7
    | Like = 8
    | Gt = 9
    | Lt = 10

    | Space = 11

type State =
    | Start
    | Space
    | Ident
    | InNumber
    | InString
    | SimpleToken of TokenKind

let public tokenise input =

    let tokenForState =
        function
        | Start -> TokenKind.Error
        | Ident -> TokenKind.Ident
        | Space -> TokenKind.Space
        | InNumber -> TokenKind.Number
        | SimpleToken kind -> kind
        | _ -> TokenKind.Error

    let nextState char =
        function
        | Start ->
            match char with
            | c when Char.IsWhiteSpace(c) -> Some(Space)
            | c when Char.IsNumber(c) -> Some(InNumber)
            | c when Char.IsLetter(c) -> Some(Ident)
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

    seq {
        for char in input do
            match nextState char state with
            | None ->
                yield tokenForState state
                // HAXX: Assume we always return some state for start transitions
                state <- (nextState char Start).Value
            | Some next -> state <- next

        if state <> State.Start then
            yield tokenForState state
    }
