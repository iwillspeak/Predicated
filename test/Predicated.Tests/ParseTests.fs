module ``parser tests``

open Predicated.Syntax
open Predicated.Syntax.Patterns
open Predicated.Parse
open Xunit
open Firethorn.Red

type private Event =
    | Enter of SyntaxKind
    | Leave of SyntaxKind
    | Token of SyntaxKind * string

let private checkwalk text (events: Event list) =
    let parsed = parseRaw text

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
          Enter SyntaxKind.PATH
          Token(SyntaxKind.IDENT, "hello")
          Leave SyntaxKind.PATH
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
        "page.title = \"hello world\""
        [ Enter SyntaxKind.QUERY
          Enter SyntaxKind.COMPARE
          Enter SyntaxKind.PATH
          Token(SyntaxKind.IDENT, "page")
          Token(SyntaxKind.DOT, ".")
          Token(SyntaxKind.IDENT, "title")
          Leave SyntaxKind.PATH
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

[<Fact>]
let ``syntax tree is traversable`` () =
    let parsed = parseRaw "hello and world"

    match parsed.Tree with
    | Query q ->
        let clause = q.Clauses |> Seq.exactlyOne

        match clause with
        | Bool b -> printfn "Got a query with (%A %A %A)" b.Left b.Operator b.Right
        | Match m -> printfn "MATCH %A" m
        | Compare c -> printfn "CMP %A" c
    | _ -> failwith "Expected a query"

[<Fact>]
let ``parse simple predicate`` () =
    let parsed = parse "document.commentCount < 100"

    Assert.Empty(parsed.Diagnostics)

    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Compare, x.Kind)
            let cmp = Assert.IsAssignableFrom<CompareClause>(x)
            Assert.True(cmp.Path.IsSome)

            Assert.Collection(
                cmp.Path.Value.Parts,
                (fun x -> Assert.Equal("document", x)),
                (fun x -> Assert.Equal("commentCount", x))
            )

            Assert.Equal(cmp.Operator.Value.Kind, Some(OperatorKind.LessThan))

            Assert.True(cmp.Right.IsSome)
            let matchClause = Assert.IsAssignableFrom<MatchClause>(cmp.Right.Value)
            Assert.True(matchClause.Pattern.IsSome)

            let numberPattern =
                Assert.IsAssignableFrom<NumberPattern>(matchClause.Pattern.Value)

            Assert.Equal(PatternKind.Number, numberPattern.Kind)
            Assert.Equal(100., numberPattern.DecimalValue))
    )

[<Fact>]
let ``parse trivial string clause`` () =
    let parsed = parse "\"hello world\""

    Assert.Empty(parsed.Diagnostics)

    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Match, x.Kind)
            let mat = Assert.IsAssignableFrom<MatchClause>(x)
            Assert.True(mat.Pattern.IsSome)

            Assert.Equal(PatternKind.String, mat.Pattern.Value.Kind)
            let pat = Assert.IsAssignableFrom<StringPattern>(mat.Pattern.Value)
            Assert.Equal("hello world", pat.CookedValue))
    )

[<Fact>]
let ``parse trivial string with single quotes`` () =
    let parsed = parse "'hello world'"

    Assert.Empty(parsed.Diagnostics)

    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Match, x.Kind)
            let mat = Assert.IsAssignableFrom<MatchClause>(x)
            Assert.True(mat.Pattern.IsSome)

            Assert.Equal(PatternKind.String, mat.Pattern.Value.Kind)
            let pat = Assert.IsAssignableFrom<StringPattern>(mat.Pattern.Value)
            Assert.Equal("hello world", pat.CookedValue))
    )

[<Fact>]
let ``parse trivial decimal clause`` () =
    let parsed = parse "101"

    Assert.Empty(parsed.Diagnostics)

    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Match, x.Kind)
            let mat = Assert.IsAssignableFrom<MatchClause>(x)
            Assert.True(mat.Pattern.IsSome)

            Assert.Equal(PatternKind.Number, mat.Pattern.Value.Kind)
            let pat = Assert.IsAssignableFrom<NumberPattern>(mat.Pattern.Value)
            Assert.Equal(101., pat.DecimalValue))
    )

[<Fact>]
let ``parse trivial pattern clause`` () =
    let parsed = parse "document.commentCount"

    Assert.Empty(parsed.Diagnostics)

    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Match, x.Kind)
            let mat = Assert.IsAssignableFrom<MatchClause>(x)
            Assert.True(mat.Pattern.IsSome)

            Assert.Equal(PatternKind.Path, mat.Pattern.Value.Kind)
            let pat = Assert.IsAssignableFrom<PathPattern>(mat.Pattern.Value)

            Assert.Collection(
                pat.Parts,
                (fun x -> Assert.Equal("document", x)),
                (fun x -> Assert.Equal("commentCount", x))
            ))
    )

[<Fact>]
let ``parse call with multiple parameters`` () =
    let parsed = parse "hello(123, 456 'test')"

    Assert.Empty(parsed.Diagnostics)
    Assert.Collection(
        parsed.Tree.Clauses,
        (fun (x: Clause) ->
            Assert.Equal(ClauseKind.Call, x.Kind)
            let call = Assert.IsAssignableFrom<CallClause>(x)

            Assert.True(call.Path.IsSome)            
            Assert.Collection(call.Path.Value.Parts,
                (fun x -> Assert.Equal("hello", x)))

            Assert.True(call.Arguments.IsSome)
            Assert.Collection(call.Arguments.Value.Clauses,
                // TODO: Fixup these asserts
                (fun x -> ()),
                (fun x -> ()),
                (fun x -> ())
                ))
    )