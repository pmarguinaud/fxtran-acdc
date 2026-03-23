# NAME

[Fxtran::Vendor::ROCM](../lib/Fxtran/Vendor/ROCM.pm)

# DESCRIPTION

Vendor adaptor for the AMD ROCm (amdflang/amdlang) Fortran compiler.
Overrides `preprocessOptions` to apply two ROCm-specific fixups to the
compiler argument list: it honours per-file optimisation level overrides
specified via `!amdrocm -ON` comment directives in the source, and it
deduplicates `.a` static library arguments while ensuring that
`-lflang_rt.hostdevice` is followed by `-lm` exactly once.

# FUNCTIONS
