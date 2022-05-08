open Predicated.Syntax
open Predicated.Parse

open Firethorn.Red.Debug
open System

let read () = ReadLine.Read(">> ")

let print result =
    result.Diagnostics
    |> List.iter (fun diag ->
        match diag with
        | Diagnostic message -> eprintfn "error: %s" message)

    debugDump (mappedFormatter SyntaxKinds.greenToAst) result.Tree

let rec repl () = read () |> parse |> print |> repl

ReadLine.HistoryEnabled <- true
repl ()
