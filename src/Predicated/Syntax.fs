module Predicated.Syntax

/// Internal Kind for Syntax Items
type public SyntaxKind =
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