# NAME

[Fxtran::Formatter::Associate](../lib/Fxtran/Formatter/Associate.pm)

# DESCRIPTION

Formatter for Fortran `ASSOCIATE` statements. Inherits from
[Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md). The `expand` method rewrites the statement so
that each selector appears on its own line sorted alphabetically by associate
name. The `repack` method reformats a canonical statement back into a compact
multi-line form respecting the line-length limit.

# FUNCTIONS

## expand

Takes a Fortran `ASSOCIATE` statement node and an indentation string. Parses
the selector list, sorts the selectors alphabetically by associate name, and
rewrites the statement so that each selector appears on its own continuation
line. Returns the re-parsed statement node.

## repack

Takes an expanded `ASSOCIATE` statement node and an indentation string.
Extracts the sorted associate selectors and reassembles them into a compact
multi-line form that respects the project line-length limit, via
`repackCallLikeStatement`.
