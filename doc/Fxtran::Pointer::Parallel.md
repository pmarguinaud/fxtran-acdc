# NAME

[Fxtran::Pointer::Parallel](../lib/Fxtran/Pointer/Parallel.pm)

# DESCRIPTION

Transforms FORTRAN program units that contain parallel-region pragmas into
code that dispatches at runtime between multiple parallelisation backends
(e.g. OpenMP, OpenACC). For each parallel region, an `IF/ELSEIF/ELSE`
construct is generated that selects the appropriate back-end based on a
runtime condition. Array references to FIELD API objects are rewritten to
use the correct view (host or device) for each target, and pointer
associations are updated accordingly.

# FUNCTIONS

## processSingleParallel

Transform one `parallel-section` node into an `IF/ELSEIF/ELSE` dispatch
construct that selects the appropriate back-end at runtime, cloning and
processing the section for each requested parallel target.

## processSingleRoutine

Apply the full pointer-parallel transformation to one program unit: rename the
subroutine, add required USE statements and local variables, build the symbol
table, fieldify declarations, process ABORT/PARALLEL sections, replace NPROMA
arguments in call statements, set up local FIELD API objects, and declare all
required pointers.

## getWhereTargetFromTarget

Strip the trailing `%WHERE` qualifier from a target string (in-place), and
optionally capture the stripped qualifier into the second argument.

## class

Return (and load) the back-end class corresponding to a target name (e.g.
`OpenMP`, `OpenACC`), discovering available back-ends from sibling `.pm`
files on first call.

## fieldifySync

Remove FIELD API array declarations and their dummy arguments, then replace
each use of a FieldAPI variable in every statement with an appropriate sync
call (host/device), and comment out or remove the original statement.

## fieldifyDecl

Transform NPROMA array declarations into FIELD API object / Fortran pointer
pairs: local arrays gain an extra block dimension and a POINTER attribute,
dummy arguments are replaced by `CLASS(FIELD_xD)` objects, and pointer
assignments are updated to reference the new field objects.

## replaceObjectExprByPointerExpr

Walk all `named-E` expressions in a parallel section and replace references to
FIELD API-backed object components or local NPROMA arrays with the corresponding
pointer variable, optionally appending a block index.

## makeParallel

Transform a single parallel section: handle filter masks, replace object and
array expressions with pointer expressions, set pointer associations from FIELD
API views, generate the block loop and parallel directives for the chosen
back-end, and insert sync/compute DR\_HOOK regions.

## addExtraIndex

Append an extra subscript `$ind` (typically `JBLK`) to an array or
parentheses reference node, creating the reference list if it does not yet
exist.

## callParallelRoutine

Rewrite a CALL statement outside parallel sections so that NPROMA array
arguments are replaced by FIELD API descriptor arguments, object-component
arguments are replaced by their FIELD counterparts, and `YDCPG_OPTS` is
appended when absent.

## setupLocalFields

Insert `FIELD_NEW`/`FIELD_DELETE` calls around the executable section (just
after/before the DR\_HOOK entry/exit calls) to allocate and release FIELD API
objects backing local NPROMA arrays.

## getPrivateVariables

Return a sorted list of variable names that are assigned or used as a DO
variable inside a parallel section and are not FIELD API arrays (i.e. are
candidates for `PRIVATE` in a parallel directive).

## getConstantObjects

Return a sorted list of variable names that are classified as constant objects
in the symbol table and appear in the given parallel section.

# SEE ALSO

[Fxtran::Pointer](Fxtran%3A%3APointer.md), [Fxtran::Pointer::SymbolTable](Fxtran%3A%3APointer%3A%3ASymbolTable.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
