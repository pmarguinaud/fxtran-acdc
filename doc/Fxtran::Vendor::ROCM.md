# NAME

[Fxtran::Vendor::ROCM](../lib/Fxtran/Vendor/ROCM.pm)

# DESCRIPTION

Vendor adaptor for the AMD ROCm (amdflang/amdlang) Fortran compiler.
Overrides `preprocessOptions` to apply two ROCm-specific fixups to the
compiler argument list: it honours per-file optimisation level overrides
specified via `!amdrocm -ON` comment directives in the source, and it
deduplicates `.a` static library arguments while ensuring that
`-lflang_rt.hostdevice` is followed by `-lm` exactly once.

## preprocessOptions

ROCm-specific compiler argument fixup.  Applies two transformations to the
argument list before returning it:

1. If the argument list contains a `.F90` or `.f90` source file, the file is
scanned for `!amdrocm -ON` comment directives.  Any `-O` flag already
present in the argument list is replaced by the level specified in the
directive.
2. `.a` static-library arguments are deduplicated (only the first occurrence is
kept).  The flag `-lflang_rt.hostdevice` is emitted at most once and is
always immediately followed by `-lm`.
