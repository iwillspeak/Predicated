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

// ================================= Parsers ===================================

let private parseLiteral builder state =
    let synKind =
        match currentKind state with
        | TokenKind.Number -> SyntaxKind.NUMBER
        | TokenKind.String -> SyntaxKind.STRING
        | TokenKind.Ident -> SyntaxKind.IDENT
        | _ -> SyntaxKind.ERR
    eat builder synKind state

let rec private parseGroupingBody builder state =
    if state.Tokens = [] || lookingAt TokenKind.CloseParen state then
        state
    else
        state
        |> parseClause builder
        |> skipWs builder
        |> parseGroupingBody builder

and private parseClause (builder: GreenNodeBuilder) state =
    match currentKind state with
    | TokenKind.OpenParen -> 
        builder.StartNode(SyntaxKind.GROUP |> SyntaxKinds.astToGreen)
        let state =
            state
            |> eat builder SyntaxKind.OPEN_PAREN
            |> skipWs builder
            |> parseGroupingBody builder
            |> expect builder TokenKind.CloseParen SyntaxKind.CLOSE_PAREN
        builder.FinishNode()
        state
    | _ -> parseLiteral builder state

let rec private parseClauseList builder state =
    if state.Tokens = [] then
        state
    else
        state
        |> parseClause builder
        |> skipWs builder
        |> parseClauseList builder

// TODO: implement this parser!
let private parseQueryBody builder state =
    state
    |> skipWs builder
    |> parseClauseList builder


// =============================== Public API ==================================

/// Parse a Predicated Query
///
/// Tokenises the `input` and parses it as a single query. A query can be made
/// up of any number of clauses, potentially grouped and nested.
let public parse input =
    let tokens = input |> tokenise
    let builder = GreenNodeBuilder()

    let state =
        ParserState.fromTokens tokens
        |> parseQueryBody builder

    { Tree =
        builder.BuildRoot(SyntaxKind.QUERY |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
      Diagnostics = state.Diagnostics }
