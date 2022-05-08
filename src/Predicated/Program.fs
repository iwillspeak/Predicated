// For more information see https://aka.ms/fsharp-console-apps

open Predicated.Parse
open Firethorn.Red.Debug
open System

let read () =
    ReadLine.Read(">> ")

let print result =
    debugDump (mappedFormatter SyntaxKinds.greenToAst) result.Tree

let rec repl () =
    read ()
    |> parse
    |> print
    |> repl

repl ()