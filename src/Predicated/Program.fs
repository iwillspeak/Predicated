open Predicated.Syntax
open Predicated.Parse

open Firethorn.Red.Debug
open System

// FIXME: This file should't be in the library. Split out into a test harness
//        and make `Predicated` a proper library project.

let read () = ReadLine.Read(">> ")

let print (result: ParseResponse<Query>) =
    result.Diagnostics
    |> List.iter (fun diag ->
        match diag with
        | Diagnostic message -> eprintfn "error: %s" message)

    debugDump (mappedFormatter SyntaxKinds.greenToAst) result.Tree.RawNode

let rec repl () = read () |> parse |> print |> repl

ReadLine.HistoryEnabled <- true
repl ()
