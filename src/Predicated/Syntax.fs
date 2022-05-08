module Predicated.Syntax

/// Internal Kind for Syntax Items
type public SyntaxKind =
    | ERR = -1

    // NODES
    | QUERY = 0
    | CLAUSE = 1
    | LITERAL = 2
    | GROUP = 3
    | OPERATOR = 4
    | BOOL = 5

    // TOKENS
    | SPACE = 100
    | OPEN_PAREN = 101
    | CLOSE_PAREN = 102
    | NUMBER = 103
    | STRING = 104
    | IDENT = 105
    | AND = 106
    | OR = 107

module SyntaxKinds =

    /// Convert an internal syntax kind to a raw firethorn kind
    let public astToGreen (kind: SyntaxKind) = (int) kind |> Firethorn.SyntaxKind

    /// Convert a firethorn kind back to a syntax kind
    let public greenToAst =
        function
        | Firethorn.SyntaxKind k -> enum<SyntaxKind> k