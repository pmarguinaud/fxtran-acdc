# NAME

[Fxtran::FieldAPI::Parallel](../lib/Fxtran/FieldAPI/Parallel.pm)

# DESCRIPTION

Transforms Fortran subroutines so that local NPROMA-sized arrays are
wrapped in FieldAPI array objects (`ARRAY_xD` types) suitable for
parallel execution on CPU and GPU.  The main entry point,
`wrapArrays`, locates array declarations dimensioned on `KLON` or
`YDCPG_OPTS%KLON`, replaces them with the appropriate FieldAPI
wrapper type, initialises and finalises the wrappers around the
executable section, and rewrites all array references inside parallel
regions to go through the `%P` data pointer.

# FUNCTIONS

## tsToArray

Convert an intrinsic type-spec node and a rank `$nd` to the name of the
corresponding FieldAPI array wrapper type (e.g. `ARRAY_2D`, `ARRAY_INT1D`).

## wrapArrays

Replace NPROMA-dimensioned local array declarations with FieldAPI wrapper
types, insert `INIT`/`FINAL` calls around the executable section, and
rewrite in-code array references to go through the `%P` data pointer inside
parallel regions.

## makeBlockViewSection

Wrap a parallel section in a block-index `DO IBL` loop, inserting
`UPDATE_VIEW` calls for all updatable FieldAPI objects found in the section,
and attach an OpenMP `PARALLEL DO` directive.

## getUpdatables

Scan all declarations and return a hash-ref mapping each derived-type variable
name to a boolean indicating whether it is an updatable FieldAPI object.

## makeUpdatablesInout

For each updatable FieldAPI variable that appears as a dummy argument, promote
its INTENT attribute to `INOUT` and remove it from the updatable set if it
has no INTENT at all.

## makeBlockFieldAPISection

Transform a parallel section into a block-loop construct that calls
`YDCPG_BNDS%UPDATE_VIEW` per iteration, rewrites pointer references for the
requested target (host/device), appends a call-suffix, and wraps the loop with
an OpenMP `PARALLEL DO` directive.

## makePostSyncSection

Generate a host-sync variant of an already-outlined parallel section (renaming
`SYNC_DEVICE` to `SYNC_HOST` throughout) and insert the corresponding call
and interface include after the original section.

## saveFileWithSubroutineName

Write a cloned program-unit document to a `.F90` file whose name is derived
from the subroutine name found inside the document; returns that name.

## makeSyncSection

Outline a parallel section into a separate subroutine, generate a FieldAPI
sync variant of that routine, write both to disk, and optionally produce a
host-sync post-processing section.  Returns an array-ref of
`[$outline, $call, $include]`.

## makeSingleColumnFieldAPIOutlineSection

Handle a single-column parallel section by placing the computation in a
separate outlined vector subroutine (workaround for a PGI compiler bug with
nested derived-type pointers).  Wraps the call in a block `DO IBL` loop,
inserts a `DO JLON` single-column loop inside the outlined routine, and emits
the appropriate OpenMP or OpenACC directives.

## makeSingleColumnFieldAPISection

Transform a parallel section in-place into a nested `DO IBL`/`DO JLON` loop
construct that iterates over blocks and single columns, rewrites pointer
references for the target (host/device), removes JLON constructs, and attaches
OpenMP or OpenACC directives.

## makeParallel

Top-level driver for the FieldAPI parallel transformation: resolves associates,
forces single declarations, inlines contained subroutines, wraps NPROMA arrays,
then dispatches each parallel section to the appropriate section-building helper
(block, single-column, or view layout) based on its attributes.
