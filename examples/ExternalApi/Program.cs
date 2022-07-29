using Predicated;

Console.Write(">> ");
Console.Out.Flush();
var input = Console.ReadLine();

// Use the Lexer to dump the raw tokens.
foreach (var token in Lex.Tokenise(input))
{
    Console.WriteLine("TOKEN: {0}", token);
}

// Use the parser to get a typed tree
var result = Predicated.Parse.Parse(input);
Console.WriteLine("GOT TREE: {0}", result.Tree);
