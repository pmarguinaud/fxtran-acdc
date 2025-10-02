`fxtran-make` is a wrapper around `make`, performing a few tasks before starting the
actual `make` command:

- Look in the fxtran-acdc `user-in` directory and see if the corresponding file 
has been erased from the `user-out` directory.

    The origin file (the file from which the file in `user-in` is derived), is
    touched so that it be rebuild by `make`.

- Look in the `user-out` directory and see whether some files have been modified
(ie compare the file mtime with its time metadata, written at the end of the file).

    When a modified file is encountered, then it will be copied to the `user-in` directory,
    and the origin file will be touched, to trigger recompilation.

The purpose of this processing is to allow the user to edit the result of the pre-processing;
when a file in `user-out` is edited, it will be copied to `user-in` and the compilation
will start again and take the result in `user-in`.

If the user want to go back to the normal situation where the file from the git repo is 
pre-processed and the result of the pre-processing is compiled, then the user just needs
to remove the file from the `user-out` directory.
# NAME

[fxtran-make](../bin/fxtran-make)

# SYNOPSIS

    $ fxtran-make --config fxtran.conf -- make -j 4

# DESCRIPTION

`fxtran-make` is a wrapper around `make`, performing a few tasks before starting the
actual `make` command:

- Look in the fxtran-acdc `user-in` directory and see if the corresponding file 
has been erased from the `user-out` directory.

    The origin file (the file from which the file in `user-in` is derived), is
    touched so that it be rebuild by `make`.

- Look in the `user-out` directory and see whether some files have been modified
(ie compare the file mtime with its time metadata, written at the end of the file).

    When a modified file is encountered, then it will be copied to the `user-in` directory,
    and the origin file will be touched, to trigger recompilation.

The purpose of this processing is to allow the user to edit the result of the pre-processing;
when a file in `user-out` is edited, it will be copied to `user-in` and the compilation
will start again and take the result in `user-in`.

If the user want to go back to the normal situation where the file from the git repo is 
pre-processed and the result of the pre-processing is compiled, then the user just needs
to remove the file from the `user-out` directory.

# EXAMPLE

Let us assume that we want to edit `lascaw_openacc.F90`:

    $ vi build.GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24/.fxtran/generated/lascaw_openacc.F90

then invoke `ecbundle` for compiling:

    $ ./ial-bundle \
    build  \
    --build-dir build.GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24 \
    -j 32 \
    --arch arch/ecmwf/hpc2020/nvhpc+fxtran+generic/ \
    --verbose \
    --build-type FXTRAN_GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24 \
    --with-openacc

In principle, `cmake` should recompile `lascaw.F90`, and use the file `lascaw_openacc.F90`
from `user-in`.

`lascaw_openacc.F90` should be in `user-in` :

    $ ls -lrt build.GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24/.fxtran/user-in/lascaw_openacc.F90 
    -rw-r--r-- 1 sor fr 27525 Oct  2 12:53 build.GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24/.fxtran/user-in/lascaw_openacc.F90

Removing `lascaw_openacc.F90` from `build.GPUCC80_O1_NVHPC25.9_CUDA13.0_HPCX2.24/.fxtran/generated/` brings back
to the initial situation.

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

make

# COPYRIGHT

Meteo-France 2025
