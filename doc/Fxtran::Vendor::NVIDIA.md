# NAME

[Fxtran::Vendor::NVIDIA](../lib/Fxtran/Vendor/NVIDIA.pm)

# DESCRIPTION

Vendor adaptor for the NVIDIA HPC SDK (pgf90/nvfortran) Fortran compiler.
Overrides `preprocessOptions` to apply two fixups to the link command line
(compilation command lines, containing `-c`, are left unchanged):

1. If GPU offload is enabled (`-cuda`, `-acc`, `-acc=gpu`, `-mp=gpu`,
`-stdpar=gpu`) and relocatable device code is not disabled (`-gpu=nordc`),
the CUDA runtime libraries `cudadevrt` and `cudart_static` are removed
(either as `-lcudadevrt`, `-lcudart_static` or as full paths to
`libcudadevrt.a`, `libcudart_static.a`). These are added by CMake for
targets containing CUDA sources; the NVIDIA driver already adds
`-lcudadevrt -lcudart` at the right place, and ignores `-lcudart_static`.
Leaving `cudadevrt` in the user part of the command line makes nvlink fail
with "unexpected object after cudadevrt" when a library with device code
(such as `libFxtranACDC.a`) is appended afterwards.
2. If `-cuda` is present and no `-cudalib=` option is given, `-cudalib=cublas`
is added.

# SEE ALSO

[Fxtran::Vendor](Fxtran%3A%3AVendor.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2026
