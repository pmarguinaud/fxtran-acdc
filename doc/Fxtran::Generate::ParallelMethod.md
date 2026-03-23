# NAME

[Fxtran::Generate::ParallelMethod](../lib/Fxtran/Generate/ParallelMethod.pm)

# DESCRIPTION

Generates a C stub file (`parallelmethod_<name`.c>) that embeds, in named ELF
sections, a mapping from code sections to their preferred parallel method
(OPENMP, OPENMPSINGLECOLUMN, OPENACCSC).  The mapping is read from
`FXTRAN_ACDC_LPARALLELMETHOD` macro calls inside the Fortran source.  It also
inserts a call to the generated C stub into the program unit so that the linker
can resolve the symbol.

# FUNCTIONS

## generateCCode

Generate a C stub file (`parallelmethod_<name`.c>) containing ELF-section
strings that map code-section names to their parallel methods, and insert a
call to the stub function into the program unit so the linker can resolve the
symbol.
