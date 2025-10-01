The purpose of this script is to extract the list of parallel methods embedded in the ELF 
sections of an executable.

The lists of possible methods, per ACDC section are saved in `.fxtran.acdc.*` ELF sections. We use
`objdump` and `objcopy` to extract this information and save it into plain text files.
# NAME

[fxtran-parallel-method](../bin/fxtran-parallel-method)

# DESCRIPTION

The purpose of this script is to extract the list of parallel methods embedded in the ELF 
sections of an executable.

The lists of possible methods, per ACDC section are saved in `.fxtran.acdc.*` ELF sections. We use
`objdump` and `objcopy` to extract this information and save it into plain text files.

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

[fxtran-f90](fxtran-f90.md), `objcopy`, `objdump`

# COPYRIGHT

Meteo-France 2025
