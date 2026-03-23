# NAME

[Fxtran::Pointer::Object](../lib/Fxtran/Pointer/Object.pm)

# DESCRIPTION

Provides utilities for working with derived-type object fields in the context
of FIELD API pointer transformations. Functions look up type declarations,
determine whether a component belongs to an object, and translate field
expressions and component paths from their original names (e.g. `PT0`,
`DM0`) to their FIELD API field counterparts (e.g. `FT0`, `F_PT0`).

# FUNCTIONS

## getObjectDecl

Look up and cache the parsed declaration node for a given type component key.
Dies unless the key is found in the type hash or `allowConstant` is set.

## getObjectType

Return and cache the type name (`T-N` text) for the given symbol table entry.

## isField

Return true when the component path `@ctl` of symbol `$s` is backed by a
FIELD API entry in the `$types` hash.

## asFromDecl

Extract the `array-spec` node from a declaration node, or return `undef`.

## getFieldFromExpr

Clone a named-E expression and rewrite its last component name to the
corresponding FIELD API name (e.g. `PT0` -> `FT0`, `P*` -> `F_*`),
stripping trailing array and parentheses references.

## getFieldFromObjectComponents

Build a new `named-E` XML node for the FIELD API counterpart of an object
component path, applying the same naming rules as `getFieldFromExpr`.
