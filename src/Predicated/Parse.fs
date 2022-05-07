module Predicated.Parse

open Firethorn.Red
open Firethorn.Green
open Predicated.Lex

/// Internal Kind for Syntax Items
type SyntaxKind =
    | ERR = -1

    // NODES
    | PROGRAM = 0
    | EXPR = 1

    // TOKENS
    | SPACE = 100

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
    ; Tokens: TokenKind list }

module private ParserState =
    let public fromTokens tokens =
        { Tokens = tokens; Diagnostics = [] }

    let public withDiagnostic state diagnostic = 
        { state with ParserState.Diagnostics = (diagnostic :: state.Diagnostics) }

let private eat (builder: GreenNodeBuilder) kind state =
    let token = List.head state.Tokens
    builder.Token(kind |> SyntaxKinds.astToGreen, "<FIXME>")
    { state with Tokens = List.tail state.Tokens }

let private skipWs (builder: GreenNodeBuilder) state =
    match state.Tokens |> List.tryHead with
    | Some(TokenKind.Space) ->
        eat builder SyntaxKind.SPACE state
    | _ -> state

let private parseExpression builder state =
    match state.Tokens |> List.head with
    // TODO: Other expression types here...
    | _ -> eat builder SyntaxKind.ERR state

let rec private parseExpressions builder state =
    match state.Tokens with
    | [] -> state
    | _ ->
        state
        |> parseExpression builder 
        |> skipWs builder
        |> parseExpressions builder

let public parse input =
    let tokens = input |> tokenise |> List.ofSeq
    let builder = GreenNodeBuilder()

    let state = 
        ParserState.fromTokens tokens
        |> skipWs builder
        |> parseExpressions builder 

    { Tree =
        builder.BuildRoot(SyntaxKind.PROGRAM |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
      Diagnostics = state.Diagnostics }
