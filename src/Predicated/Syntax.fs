namespace Predicated.Syntax

open Firethorn.Red

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

type public ClauseKind =
    | Match = 1
    | Compare = 2
    | Bool = 3

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

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.OPERATOR then
            Some(Operator(node))
        else
            None

type public Path internal (node: SyntaxNode) =
    inherit SyntaxItem(node)
    
    member public _.Parts =
        node.ChildrenWithTokens()
        |> Seq.choose (Firethorn.NodeOrToken.asToken)
        |> Seq.filter (fun x -> x.Kind = (SyntaxKind.IDENT |> SyntaxKinds.astToGreen))
        |> Seq.map (fun x -> x.Green.Text)

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.PATH then
            Some(Path(node))
        else
            None

[<AbstractClass>]
type public Clause internal (node: SyntaxNode) =
    inherit SyntaxItem(node)

    abstract Kind: ClauseKind with get

    static member FromRaw(node: SyntaxNode) =
        match (node.Kind |> SyntaxKinds.greenToAst) with
        | SyntaxKind.BOOL -> Some(BoolClause(node) :> Clause)
        | SyntaxKind.MATCH_ATOM -> Some(MatchClause(node))
        | SyntaxKind.COMPARE -> Some(CompareClause(node))
        | _ -> None

and public CompareClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Compare

    member _.Path =
        node.Children()
        |> Seq.choose Path.FromRaw
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

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.MATCH_ATOM then
            Some(MatchClause(node))
        else
            None

and public BoolClause internal (node: SyntaxNode) =
    inherit Clause(node)

    override _.Kind = ClauseKind.Bool

    member self.Left =
        node.Children()
        |> Seq.choose Clause.FromRaw
        |> Seq.tryHead

    member self.Operator =
        node.Children()
        |> Seq.choose Operator.FromRaw
        |> Seq.tryExactlyOne

    member self.Right =
        node.Children()
        |> Seq.choose Clause.FromRaw
        |> Seq.skip 1
        |> Seq.tryHead

    static member FromRaw(node: SyntaxNode) =
        if (node.Kind |> SyntaxKinds.greenToAst) = SyntaxKind.BOOL then
            Some(BoolClause(node))
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

    let (|Bool|Match|) (clause: Clause) =
        match clause with
        | :? BoolClause as b -> Bool b
        | :? MatchClause as m -> Match m
        | _ -> failwith "unexpected clause type"

    let (|Query|_|) node = Query.FromRaw(node)
