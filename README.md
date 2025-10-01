# Adaptation des Codes à Divers Calculateurs

![](./images/ACDC.png)


# [fxtran-ar ...](./doc/fxtran-ar.md)

`fxtran-ar` is a wrapper around the traditional `ar` utility. It is
supposed to behave similarly to `ar` and accept the same options.

For now `fxtran-` accept only the following commands:

- q
- qc

These two command strings are used by `cmake` to create static
libraries.

The purpose of `fxtran-ar` is to manage situations where objects
to be aggregated in a static library may actually be static libraries
themselves.

For instance, in the following command:

    $ fxtran-ar q lib.a a.o b.o ...

`a.o` may be a static library:

    $ ar tv a.o 
    rw-r--r-- 0/0   1784 Jan  1 01:00 1970 O_a.o
    rw-r--r-- 0/0   1784 Jan  1 01:00 1970 O_b.o

In this case, `fxtran-ar` will extract these objects, and build `lib.a`
using them:

    $ ar rv a.o
    $ ar crv liba.o O_a.o O_b.o b.o ...

# [fxtran-boot ...](./doc/fxtran-boot.md)

`fxtran-boot` compiles and install the fxtran-acdc libraries. Compilers and
their options may be passed directly as options, or inferred from the `ecbuild`
environment.

# [fxtran-cc ...](./doc/fxtran-cc.md)

This script is a wrapper for the C and the C++ compilers. The ACDC library is included in the link,
and archive may be replaced by list of objects if the script is invoked to perform a link.

# [fxtran-cxx ...](./doc/fxtran-cxx.md)

This script is a wrapper for the C and the C++ compilers. The ACDC library is included in the link,
and archive may be replaced by list of objects if the script is invoked to perform a link.

# [fxtran-f90 ...](./doc/fxtran-f90.md)

`fxtran-f90` is compiler wrapper performing the following tasks:

- Apply some transformations on user-written source code and generate FORTRAN source code ready 
for GPU accelerators.
- Compile the original code and code produced at previous step.
- Link all objects into a single one.

`fxtran-f90` relies on `fxtran` for parsing and transforming FORTRAN source code.

# [fxtran-fix ...](./doc/fxtran-fix.md)

The purpose of `fxtran-fix` is to trap compiler errors and give the user 
a chance to debug them, without interrupting the compiling process (cmake
or gmkpack).

When a compilation error occurs, `fxtran-fix` will catch it, if the environment
variable `FXTRANFIX` is not null, and create a new shell (via xterm or screen)
which the user can use to find and fix the issue.

In this shell session, two commands are provided:

- e

    Edit the file currently being compiled.

- r

    Rerun the compiler command.

When the user is done with editing/debugging, he has to exit the shell; `fxtran-fix` 
will then attempt to compile the code once more and return control to the 
build system.

# [fxtran-gen ...](./doc/fxtran-gen.md)

`fxtran-gen` is a `click` frontend to `Fxtran::Generate` and other generation modules. It
is invoked by `fxtran-f90` to transform FORTRAN source code.

The first argument of fxtran-gen is the name of the generation module (this should be 
a clickable module), but this argument is optional and defaults to `Fxtran::Generate`. Then
comes the method to be invoked, followed by its options and arguments.

See `Fxtran::Generate` for the details of methods and options accepted by `fxtran-gen`.

# [fxtran-makemaker ...](./doc/fxtran-makemaker.md)

`fxtran-makemaker` is a simple `Makefile` generator for external test
cases build with `fxtran-f90`. 

When invoked, `fxtran-makemaker` will:

- Generate include files for subroutines.
- Scan source code and detect dependencies.
- Write a `Makefile` to compile the code.

The user is expected to provide a file named `Makefile.inc` which contains
the compiler name and options.

# [fxtran-mergetool ...](./doc/fxtran-mergetool.md)

Wrap kdiff3, meld or vimdiff. Expand the following FORTRAN statements before calling merge utilities:

- call
- associate
- subroutine

Repack after merging.
