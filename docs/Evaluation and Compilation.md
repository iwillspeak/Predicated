# Evaluation and Compilation

Once a given piece of text has been parsed into a `Query` users will want to use
it to filter things. Queries could be traversed manually, converted to
third-party query forms, or evaluated directly

## The Visit

To handle the common case we should provide a `Visit` module, with methods for
visiting queries and clauses. This can then be expose in an C# friendly API
using an extension method and a vistor interface. 

Visitors can be extended to also support transforming queries. There are two
kinds of transform we might want to perform:

 * Re-writing the query
 * Converting the qurey some external kind

## Evaluation

A common form of traversal will be evaluation. WE should expose an `Eval` module
with methods for evaulating against both a typed value and untyped dictionary.
Evaluation can return a result where the OK represents the state of the match,
and the Err represents the place where the evaulation failed.

Evaluation is effectively a tree-walk interpreter for the query. It should take
some kind of interface to query over, so that users can implement predicateable
for their own types as they see fit.

## Compilation

For some cases a predicate will be used many times and the cost of walking a
deeply nested tree should be avoided. In these cases we could compile the
queries down to IL, potentially via expression trees. Then the predicate would
be available as a `Predciate<T>` delegate type.