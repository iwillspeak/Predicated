namespace Predicated.Syntax

open Firethorn.Red
open Firethorn

/// Internal Kind for Syntax Items
type public SyntaxKind =
    | ERR = -1

    // NODES
    | QUERY = 0
    | COMPARE = 1
    | MATCH_ATOM = 2
    | GROUP = 3
    | OPERATOR = 4
    | BOOL = 5
    | PATH = 6
    | CALL = 7
    | ARGUMENTS = 8

    // TOKENS
    | SPACE = 100
    | OPEN_PAREN = 101
    | CLOSE_PAREN = 102
    | NUMBER = 103
    | STRING = 104
    | IDENT = 105
    | AND = 106
    | OR = 107
    | GT = 108
    | LT = 109
    | EQ = 110
    | LIKE = 111
    | DOT = 112

/// Type for each clause.
type public ClauseKind =
    | Match = 1
    | Compare = 2
    | Bool = 3
    | Call = 4

/// Type for atom match patterns
type public PatternKind =
    | String = 1
    | Number = 2
    | Path = 3

/// Operator kinds. This includes both boolean and comparision operators.
type public OperatorKind =
    | GreaterThan = 1
    | LessThan = 2
    | Equal = 3
    | Like = 4
    | And = 5
    | Or = 6

/// Utilities for working with syntax kinds
module SyntaxKinds =

    /// Convert an internal syntax kind to a raw firethorn kind
    [<CompiledName("AstToGreen")>]
    let public astToGreen (kind: SyntaxKind) = (int) kind |> Firethorn.SyntaxKind

    /// Convert a firethorn kind back to a syntax kind
    [<CompiledName("GreenToAst")>]
    let public greenToAst =
        function
        | Firethorn.SyntaxKind k -> enum<SyntaxKind> k

type public SyntaxItem internal (node: SyntaxNode) =

    member _.Range = node.Range

    member _.RawNode = node

type public Operator internal (node: SyntaxNode) =

    inherit SyntaxItem(node)

    member public _.Kind =
        let opToken =
            node.ChildrenWithTokens()
            |> Seq.choose (NodeOrToken.asToken)
            |> Seq.filter (fun x ->
                x.Kind
                <> (SyntaxKind.SPACE |> SyntaxKinds.astToGreen))
            |> Seq.tryExactlyOne

        opToken
        |> Option.bind (fun x ->
            match x.Kind |> SyntaxKinds.greenToAst with
            | SyntaxKind.GT -> Some(OperatorKind.GreaterThan)
            | SyntaxKind.LT -> Some(OperatorKind.LessThan)
            | SyntaxKind.LIKE -> Some(OperatorKind.Like)
            | SyntaxKind.EQ -> Some(OperatorKind.Equal)
            | SyntaxKind.AND -> Some(OperatorKind.And)
            | SyntaxKind.OR -> Some(OperatorKind.Or)
            | _ -> None)

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.OPERATOR then
            Some(Operator(node))
        else
            None

[<AbstractClass>]
type public Pattern internal () =

    abstract member Kind: PatternKind

    static member FromRaw(element: NodeOrToken<SyntaxNode, SyntaxToken>) =
        match element with
        | Node node ->
            PathPattern.FromRaw(node)
            |> Option.map (fun x -> x :> Pattern)
        | Token token ->
            let kind = token.Kind |> SyntaxKinds.greenToAst

            match kind with
            | SyntaxKind.STRING -> Some(StringPattern(token) :> Pattern)
            | SyntaxKind.NUMBER -> Some(NumberPattern(token))
            | _ -> None

and public PathPattern internal (node: SyntaxNode) =
    inherit Pattern()

    override _.Kind = PatternKind.Path

    member public _.Parts =
        node.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.filter (fun x -> x.Kind = (SyntaxKind.IDENT |> SyntaxKinds.astToGreen))
        |> Seq.map (fun x -> x.Green.Text)

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.PATH then
            Some(PathPattern(node))
        else
            None

and public StringPattern internal (node: SyntaxToken) =
    inherit Pattern()

    override _.Kind = PatternKind.String

    // FIXME: Properly cook the string here
    member _.CookedValue = node.Green.Text.Trim('\"', '\'')

and public NumberPattern internal (node: SyntaxToken) =
    inherit Pattern()

    override _.Kind = PatternKind.Number

    member _.DecimalValue = node.Green.Text |> float

[<AbstractClass>]
type public Clause internal (node: SyntaxNode) =
    inherit SyntaxItem(node)

    abstract Kind: ClauseKind

    static member FromRaw(node: SyntaxNode) =
        match (node.Kind |> SyntaxKinds.greenToAst) with
        | SyntaxKind.BOOL -> Some(BoolClause(node) :> Clause)
        | SyntaxKind.MATCH_ATOM -> Some(MatchClause(node))
        | SyntaxKind.COMPARE -> Some(CompareClause(node))
        | SyntaxKind.CALL -> Some(CallClause(node))
        | _ -> None

and public CallClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Call

    member _.Path =
        node.Children()
        |> Seq.choose PathPattern.FromRaw
        |> Seq.tryExactlyOne

    member _.Arguments =
        node.Children()
        |> Seq.choose Arguments.FromRaw
        |> Seq.tryExactlyOne

and public CompareClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Compare

    member _.Path =
        node.Children()
        |> Seq.choose PathPattern.FromRaw
        |> Seq.tryExactlyOne

    member _.Operator =
        node.Children()
        |> Seq.choose Operator.FromRaw
        |> Seq.tryExactlyOne

    member _.Right =
        node.Children()
        |> Seq.choose Clause.FromRaw
        |> Seq.tryHead


and public MatchClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Match

    member _.Pattern =
        node.ChildrenWithTokens()
        |> Seq.choose Pattern.FromRaw
        |> Seq.tryHead

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.MATCH_ATOM then
            Some(MatchClause(node))
        else
            None

and public BoolClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Bool

    member _.Left =
        node.Children()
        |> Seq.choose Clause.FromRaw
        |> Seq.tryHead

    member _.Operator =
        node.Children()
        |> Seq.choose Operator.FromRaw
        |> Seq.tryExactlyOne

    member _.Right =
        node.Children()
        |> Seq.choose Clause.FromRaw
        |> Seq.skip 1
        |> Seq.tryHead

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.BOOL then
            Some(BoolClause(node))
        else
            None

and public Arguments internal (node: SyntaxNode) =
    inherit SyntaxItem(node)

    member _.Clauses = node.Children() |> Seq.choose (Clause.FromRaw)

    static member FromRaw(node: SyntaxNode) =
        let kind = node.Kind |> SyntaxKinds.greenToAst

        if kind = SyntaxKind.ARGUMENTS then
            Some(Arguments(node))
        else
            None

type public Query internal (node: SyntaxNode) =
    inherit SyntaxItem(node)

    member _.Clauses = node.Children() |> Seq.choose Clause.FromRaw

    static member FromRaw(node: SyntaxNode) =
        let kind = node.Kind |> SyntaxKinds.greenToAst

        if kind = SyntaxKind.QUERY then
            Some(Query(node))
        else
            None

/// Active Patterns for F# Match Ergonomics
module Patterns =

    let (|Bool|Match|Compare|) (clause: Clause) =
        match clause.Kind with
        | ClauseKind.Bool -> Bool(clause :?> BoolClause)
        | ClauseKind.Match -> Match(clause :?> MatchClause)
        | ClauseKind.Compare -> Compare(clause :?> CompareClause)
        | _ -> failwithf "Unexpected clause kind %A" clause.Kind

    let (|Path|Number|String|) (pattern: Pattern) =
        match pattern.Kind with
        | PatternKind.Path -> Path(pattern :?> PathPattern)
        | PatternKind.Number -> Number(pattern :?> NumberPattern)
        | PatternKind.String -> String(pattern :?> StringPattern)
        | _ -> failwithf "Unexpected pattern kind %A" pattern.Kind

    let (|Query|_|) node = Query.FromRaw(node)
