module ``parser tests``

open Predicated.Syntax
open Predicated.Parse
open Firethorn.Red.Debug
open Xunit
open Firethorn.Red

type private Event =
    | Enter of SyntaxKind
    | Leave of SyntaxKind
    | Token of SyntaxKind * string

let private checkwalk text (events: Event list) =
    let parsed = parse text

    let parsedEvents =
        Walk.walk parsed.Tree
        |> Seq.map (function
            | EnterNode n -> Enter(n.Kind |> SyntaxKinds.greenToAst)
            | LeaveNode n -> Leave(n.Kind |> SyntaxKinds.greenToAst)
            | OnToken t ->
                (t.Kind |> SyntaxKinds.greenToAst, t.Green.Text)
                |> Token)
        |> List.ofSeq

    Assert.Equal<Event list>(events, parsedEvents)

[<Fact>]
let ``checkwalk tests`` () =
    checkwalk
        ""
        [ Enter SyntaxKind.QUERY
          Leave SyntaxKind.QUERY ]

    checkwalk
        " \t\n "
        [ Enter SyntaxKind.QUERY
          Token(SyntaxKind.SPACE, " \t\n ")
          Leave SyntaxKind.QUERY ]

    checkwalk
        "hello"
        [ Enter SyntaxKind.QUERY
          Enter SyntaxKind.MATCH_ATOM
          Token(SyntaxKind.IDENT, "hello")
          Leave SyntaxKind.MATCH_ATOM
          Leave SyntaxKind.QUERY ]

    checkwalk
        "\"hello world\""
        [ Enter SyntaxKind.QUERY
          Enter SyntaxKind.MATCH_ATOM
          Token(SyntaxKind.STRING, "\"hello world\"")
          Leave SyntaxKind.MATCH_ATOM
          Leave SyntaxKind.QUERY ]

    checkwalk
        "title = \"hello world\""
        [ Enter SyntaxKind.QUERY
          Enter SyntaxKind.COMPARE
          Token(SyntaxKind.IDENT, "title")
          Token(SyntaxKind.SPACE, " ")
          Enter SyntaxKind.OPERATOR
          Token(SyntaxKind.EQ, "=")
          Leave SyntaxKind.OPERATOR
          Token(SyntaxKind.SPACE, " ")
          Enter SyntaxKind.MATCH_ATOM
          Token(SyntaxKind.STRING, "\"hello world\"")
          Leave SyntaxKind.MATCH_ATOM
          Leave SyntaxKind.COMPARE
          Leave SyntaxKind.QUERY ]
