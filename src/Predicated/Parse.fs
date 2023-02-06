module Predicated.Parse

open Firethorn.Red
open Firethorn.Green
open Predicated.Lex
open Predicated.Syntax

/// Parser Diagnostic
///
/// Holds instances of parser diagnostics.
type public Diagnostic = Diagnostic of string

/// Parse Response
///
/// Parser responses are the result of any parse call. A parse response always
/// returns some syntax item `Tree`, along with a list of `Diagnostics` that
/// were emitted during the parse.
type public ParseResponse<'a> =
    { Tree: 'a
      Diagnostics: Diagnostic list }

module ParseResponse =

    /// Map the Syntax of a Parse Response
    ///
    /// Given a tree map the contents with `mapping` leaving the diagnostics
    /// unchanged.
    [<CompiledName("Map")>]
    let public map mapping response =
        { Tree = mapping response.Tree
          Diagnostics = response.Diagnostics }

    /// Convert a parser response into a plain result type
    ///
    /// This drops any tree from the error, but opens up parser responses to
    /// being processed using standard error handling.
    [<CompiledName("ToResult")>]
    let public toResult response =
        match response.Diagnostics with
        | [] -> response.Tree |> Ok
        | _ -> response.Diagnostics |> Error

/// Internal Parser State
///
/// This record holds the state our parser will operate on. We keep the remaining
/// tokens that have yet to be consumed, paired with the list of diagnostics we
/// have generated up to this point.
type private ParserState =
    { Diagnostics: Diagnostic list
      Tokens: (string * TokenKind) list }

module private ParserState =

    /// Create a new parser state from a list of tokens
    let public fromTokens tokens =
        { Tokens = tokens |> List.ofSeq
          Diagnostics = [] }

    /// Buffer a diagnostic into the state to track some error in the parse.
    let public withDiagnostic state diagnostic =
        { state with ParserState.Diagnostics = (diagnostic :: state.Diagnostics) }

    /// Bump the token state. This will throw if called when there are no remaining tokens.
    let public bump state =
        (List.head state.Tokens, { state with Tokens = List.tail state.Tokens })

// ============================= Parser Utilities ==============================

let private currentKind state =
    match List.tryHead state.Tokens with
    | Some (_, kind) -> kind
    | _ -> TokenKind.EOF

let private lookingAt kind state = currentKind state = kind

let private lookingAtAny kinds state = List.contains (currentKind state) kinds

let private eat (builder: GreenNodeBuilder) kind state =
    let (lexeme, _), state = ParserState.bump state
    builder.Token(kind |> SyntaxKinds.astToGreen, lexeme)
    state

let private expect (builder: GreenNodeBuilder) tokenKind syntaxKind state =
    if lookingAt tokenKind state then
        eat builder syntaxKind state
    else
        sprintf "Expected %A" tokenKind
        |> Diagnostic
        |> ParserState.withDiagnostic state

let private optionalComma (builder: GreenNodeBuilder) state =
    if lookingAt TokenKind.Comma state then
        eat builder SyntaxKind.SPACE state
    else
        state

let private skipWs (builder: GreenNodeBuilder) state =
    if lookingAt TokenKind.Space state then
        eat builder SyntaxKind.SPACE state
    else
        state

let private bindingPower =
    function
    | TokenKind.And -> 1
    | TokenKind.Or -> 2
    | _ -> 0

// ================================= Parsers ===================================

let private parseLiteral (builder: GreenNodeBuilder) state =
    builder.StartNode(SyntaxKind.MATCH_ATOM |> SyntaxKinds.astToGreen)

    let synKind =
        match currentKind state with
        | TokenKind.Number -> SyntaxKind.NUMBER
        | TokenKind.String -> SyntaxKind.STRING
        | TokenKind.Ident -> SyntaxKind.IDENT
        | _ -> SyntaxKind.ERR


    let state =
        if synKind = SyntaxKind.ERR then
            currentKind state
            |> sprintf "Unexpected token %A"
            |> Diagnostic
            |> ParserState.withDiagnostic state
        else
            state
        |> eat builder synKind

    builder.FinishNode()
    state

let private parseOperator (builder: GreenNodeBuilder) kind state =
    builder.StartNode(SyntaxKind.OPERATOR |> SyntaxKinds.astToGreen)
    let state = eat builder kind state
    builder.FinishNode()
    state

let rec private parseClause (builder: GreenNodeBuilder) rbp state =
    let mark = builder.Mark()
    let mutable state = state |> parseNud builder |> skipWs builder

    while (currentKind state |> bindingPower) > rbp do
        state <- state |> parseLed builder
        builder.ApplyMark(mark, SyntaxKind.BOOL |> SyntaxKinds.astToGreen)
        state <- state |> skipWs builder

    state

/// Parse a list of .-separated idents as a 'path'.
and private parsePath (builder: GreenNodeBuilder) state =
    builder.StartNode(SyntaxKind.PATH |> SyntaxKinds.astToGreen)

    let mutable state = state |> eat builder SyntaxKind.IDENT

    while lookingAt TokenKind.Dot state do
        state <-
            state
            |> eat builder SyntaxKind.DOT
            |> expect builder TokenKind.Ident SyntaxKind.IDENT

    builder.FinishNode()

    state

and private parseNud builder state =
    match currentKind state with
    | TokenKind.OpenParen ->
        builder.StartNode(SyntaxKind.GROUP |> SyntaxKinds.astToGreen)

        let state =
            state
            |> eat builder SyntaxKind.OPEN_PAREN
            |> skipWs builder
            |> parseClauseList builder [ TokenKind.EOF; TokenKind.CloseParen ]
            |> expect builder TokenKind.CloseParen SyntaxKind.CLOSE_PAREN

        builder.FinishNode()
        state
    | TokenKind.Ident ->
        let mark = builder.Mark()

        let state = state |> parsePath builder |> skipWs builder

        let infix kind =

            // parse the operator node
            let state = parseOperator builder kind state

            // Then parse the operand
            let state = state |> skipWs builder |> parseLiteral builder

            builder.ApplyMark(mark, SyntaxKind.COMPARE |> SyntaxKinds.astToGreen)
            state

        match currentKind state with
        | TokenKind.Lt -> infix SyntaxKind.LT
        | TokenKind.Gt -> infix SyntaxKind.GT
        | TokenKind.Equal -> infix SyntaxKind.EQ
        | TokenKind.Like -> infix SyntaxKind.LIKE
        | TokenKind.OpenParen ->

            builder.StartNode(SyntaxKind.ARGUMENTS |> SyntaxKinds.astToGreen)

            let state =
                state
                |> eat builder SyntaxKind.OPEN_PAREN
                |> parseClauseList builder [ TokenKind.EOF; TokenKind.CloseParen ]
                |> expect builder TokenKind.CloseParen SyntaxKind.CLOSE_PAREN

            builder.FinishNode()
            builder.ApplyMark(mark, SyntaxKind.CALL |> SyntaxKinds.astToGreen)
            state
        | _ ->
            builder.ApplyMark(mark, SyntaxKind.MATCH_ATOM |> SyntaxKinds.astToGreen)
            state

    | _ -> state |> parseLiteral builder

and private parseLed builder state =

    let tokenKnd = currentKind state

    let state =
        match tokenKnd with
        | TokenKind.And -> parseOperator builder SyntaxKind.AND state
        | TokenKind.Or -> parseOperator builder SyntaxKind.OR state
        | _ -> failwith "unexpected kind in parseExprLed"

    state
    |> skipWs builder
    |> parseClause builder (tokenKnd |> bindingPower)

and private parseClauseList builder endKinds state =
    if lookingAtAny endKinds state then
        state
    else
        state
        |> parseTopLevelCause builder
        |> optionalComma builder
        |> skipWs builder
        |> parseClauseList builder endKinds

and private parseTopLevelCause (builder: GreenNodeBuilder) state = parseClause builder 0 state


let private parseQueryBody builder state =
    state
    |> skipWs builder
    |> parseClauseList builder [ TokenKind.EOF ]


// =============================== Public API ==================================

/// Parse a Predicated Query
///
/// Tokenises the `input` and parses it as a single query. A query can be made
/// up of any number of clauses, potentially grouped and nested.
///
/// This API returns the raw syntax tree for the query. The `parse` method
/// should be preferred.
[<CompiledName("ParseRaw")>]
let public parseRaw input =
    let tokens = input |> tokenise
    let builder = GreenNodeBuilder()

    let state =
        ParserState.fromTokens tokens
        |> parseQueryBody builder

    { Tree =
        builder.BuildRoot(SyntaxKind.QUERY |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
      Diagnostics = state.Diagnostics }

/// Parse a Predciated Query
///
/// Tokenises the `input` and parses it as a single query. A query can be made
/// up of any number of clauses, potentially grouped and nested. This API
/// returns a typed query.
[<CompiledName("Parse")>]
let public parse input =
    parseRaw input
    |> ParseResponse.map (fun x -> new Query(x))
