# NAME

[Fxtran::Formatter::Associate](../lib/Fxtran/Formatter/Associate.pm)

# DESCRIPTION

Formatter for Fortran `ASSOCIATE` statements. Inherits from
[Fxtran::Formatter::block](Fxtran%3A%3AFormatter%3A%3Ablock.md). The `expand` method rewrites the statement so
that each selector appears on its own line sorted alphabetically by associate
name. The `repack` method reformats a canonical statement back into a compact
multi-line form respecting the line-length limit.

# FUNCTIONS
