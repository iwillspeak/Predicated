module Predicated.Parse

open Firethorn.Red
open Firethorn.Green
open Predicated.Lex

/// Internal Kind for Syntax Items
type SyntaxKind =
    | ERR = -1

    // NODES
    | QUERY = 0
    | CLAUSE = 1
    | LITERAL = 2
    | GROUP = 3

    // TOKENS
    | SPACE = 100
    | NUMBER = 101
    | OPEN_PAREN = 102
    | CLOSE_PAREN = 103

module SyntaxKinds =

    /// Convert an internal syntax kind to a raw firethorn kind
    let public astToGreen (kind: SyntaxKind) = (int) kind |> Firethorn.SyntaxKind

    /// Convert a firethorn kind back to a syntax kind
    let public greenToAst =
        function
        | Firethorn.SyntaxKind k -> enum<SyntaxKind> k

/// Parser Diagnostic
///
/// Holds instances of parser diagnostics.
type public Diagnostic = Diagnostic of string

/// Parse Response
///
/// Parser responses are the result of any parse call. A parse response always
/// returns some syntax item `Tree`, along with a list of `Diagnostics` that
/// were emitted during the parse.
type public ParseResponse =
    { Tree: SyntaxNode
      Diagnostics: Diagnostic list }

module ParseResponse =

    /// Convert a parser response into a plain result type
    ///
    /// This drops any tree from the error, but opens up parser responses to
    /// being processed using standard error handling.
    let public toResult response =
        match response.Diagnostics with
        | [] -> response.Tree |> Ok
        | _ -> response.Diagnostics |> Error

type private ParserState =
    { Diagnostics: Diagnostic list
      Tokens: (string * TokenKind) list }

module private ParserState =
    let public fromTokens tokens = { Tokens = tokens; Diagnostics = [] }

    let public withDiagnostic state diagnostic =
        { state with ParserState.Diagnostics = (diagnostic :: state.Diagnostics) }

    let public bump state =
        (List.head state.Tokens, { state with Tokens = List.tail state.Tokens })

let private currentKind state =
    let (_, kind) = List.head state.Tokens
    kind

let private lookingAt kind state =
    match List.tryHead state.Tokens with
    | Some (_, k) when k = kind -> true
    | _ -> false

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

let private skipWs (builder: GreenNodeBuilder) state =
    if lookingAt TokenKind.Space state then
        eat builder SyntaxKind.SPACE state
    else 
        state

let rec private parseClause (builder: GreenNodeBuilder) state =
    match currentKind state with
    | TokenKind.Number ->
        builder.StartNode(SyntaxKind.LITERAL |> SyntaxKinds.astToGreen)
        let state = eat builder SyntaxKind.NUMBER state
        builder.FinishNode()
        state
    | TokenKind.OpenParen ->
        builder.StartNode(SyntaxKind.GROUP |> SyntaxKinds.astToGreen)
        let state = eat builder SyntaxKind.OPEN_PAREN state

        let state = eat builder SyntaxKind.CLOSE_PAREN state
        builder.FinishNode()
        state
    // TODO: Other expression types here...
    | _ -> eat builder SyntaxKind.ERR state
and private parseClauseLed (builder: GreenNodeBuilder) state =
    // FIXME: sort this out
    state
and private parseClauseNud (builder: GreenNodeBuilder) state =
    // FIXME: Sort this out
    state

let rec private parseClauses builder state =
    match state.Tokens with
    | [] -> state
    | _ ->
        state
        |> parseClause builder
        |> skipWs builder
        |> parseClauses builder

let public parse input =
    let tokens = input |> tokenise |> List.ofSeq
    let builder = GreenNodeBuilder()

    let state =
        ParserState.fromTokens tokens
        |> skipWs builder
        |> parseClauses builder

    { Tree =
        builder.BuildRoot(SyntaxKind.QUERY |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
      Diagnostics = state.Diagnostics }
