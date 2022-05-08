# Grammar

Each query is made up of a squence of clauses.

## Match Atom Clause

The simplest kind of caluse. A match clause contains just a literal to match:

```
"hello"
world
123
```

Ths is modelled in the tree as a node of type `MATCH_ATOM`.

## Group Clause

A group clause uses `(` and `)` to explicitly group one or more sub-clauses.

```
((this and that) or the_other)
```

## Compare Clause

A compare clause consists of a path, an operator, and a simple value to compare
against:

```
title ~ "example"
posted < 2w
created = udpated
author = current_user()
```

## Boolean Clause

Top level query clauses are implicitly grouped together with `AND`. Bool caluses
allow grouping two claues together explicitly with a boolean `AND` or `OR`
operation. The operators are case insensitive.

```
"hello" AND "world"
"fiz" or "buzz"
```
