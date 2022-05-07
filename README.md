
# Simple Data Types

 * `string` - `"hello world"`
 * `numbers` - `1`
 * `-2d +1w

# Comparison Expressions

```
text = "hello"
title ~ "world"
created < -2d
```

# Boolean Operators

```
AND OR NOT
()
```

# Examples

Find hello near world in recent documents.

```
near("hello", "world") AND created > -2w
```

```
near("hello", "world") AND created > date('2022-04-08')
```

Text containing `sum bugz` assigned to the current user.

```
text ~ "some bugz" AND assignee = currentUser()
```

Documents where the title and description are the same.

```
title = description
```

Identifiers on their own degrade to simple string matches. e.g. the following
searches for documents containing `fiz` and `buz`:

```
fiz buz
```

To check for properties existing we can defer to a function.

```
exists(fizz)
```