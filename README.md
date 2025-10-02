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

# [fxtran-checker ...](./doc/fxtran-checker.md)

Apply norm checking to a FORTRAN file. The file will be scanned for ACDC directives, unless
the `--method` option is used. `fxtran-f90` will be invoked.

# [fxtran-cxx ...](./doc/fxtran-cxx.md)

This script is a wrapper for the C and the C++ compilers. The ACDC library is included in the link,
and archive may be replaced by list of objects if the script is invoked to perform a link.

# [fxtran-difftool ...](./doc/fxtran-difftool.md)

Reformat FORTRAN code before invoking a diff utility such as `kdiff3` or `meld`.

This is meant to ease difference visualization; for instance, the two following `CALL`
statements are best diffed if they have a single argument per line:

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

    CALL ACDRAG (YDMODEL%YRCST, YDMODEL%YRML_PHY_MF, KIDIA, KFDIA, KLON, &
    & NTDRAG, KFLEVG, PAPRS, PAPRSF, PDELP, ZNBVNO, YDMF_PHYS_BASE_STATE%YCPG_PHY%XYB%RDELP, &
    & PU, PV, YDVARS%GEOMETRY%RCORI%T0, YDMF_PHYS_SURF%GSD_VF%PGETRL, LLFLAG, ZGWDCS, YDMF_PHYS_SURF%GSD_VF%PVRLAN, &
    & YDMF_PHYS_SURF%GSD_VF%PVRLDI, YDMF_PHYS%OUT%STRDU, YDMF_PHYS%OUT%STRDV, ZTRAJGWD)

That is, reformatting the `CALL` statements make the diff trivial:

    CALL ACDRAG (                                 CALL ACDRAG (
    & YDMODEL%YRCST, &                            & YDMODEL%YRCST, &
    ...
    & PV, &                                       & PV, &
    & YDVARS%GEOMETRY%RCORI%T0, &                 & YDVARS%GEOMETRY%RCORI%T0, &
    & YDMF_PHYS_SURF%GSD_VF%PGETRL, &             & YDMF_PHYS_SURF%GSD_VF%PGETRL, &
    -------------------------------------------   & LLFLAG, &
    & ZGWDCS, &                                   & ZGWDCS, &
    ...
    & YDMF_PHYS%OUT%STRDV, &                      & YDMF_PHYS%OUT%STRDV, &
    & ZTRAJGWD)                                   & ZTRAJGWD)

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

# [fxtran-formatter ...](./doc/fxtran-formatter.md)

The purpose of `fxtran-formatter` is to reformat some statements.

For now we reformat only the following statements (these happen to be the
most sensitive to merge process) :

- `CALL` statements.
- `SUBROUTINE` statements.
- `ASSOCIATE` statements.

# [fxtran-gen ...](./doc/fxtran-gen.md)

`fxtran-gen` is a `click` frontend to `Fxtran::Generate` and other generation modules. It
is invoked by `fxtran-f90` to transform FORTRAN source code.

The first argument of fxtran-gen is the name of the generation module (this should be 
a clickable module), but this argument is optional and defaults to `Fxtran::Generate`. Then
comes the method to be invoked, followed by its options and arguments.

See `Fxtran::Generate` for the details of methods and options accepted by `fxtran-gen`.

# [fxtran-make ...](./doc/fxtran-make.md)

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

# [fxtran-makemaker ...](./doc/fxtran-makemaker.md)

`fxtran-makemaker` is a simple `Makefile` generator for external test
cases build with `fxtran-f90`. 

When invoked, `fxtran-makemaker` will:

- Generate include files for subroutines.
- Scan source code and detect dependencies.
- Write a `Makefile` to compile the code.

The user is expected to provide a file named `Makefile.inc` which contains
the compiler name and options.

# [fxtran-markdown ...](./doc/fxtran-markdown.md)

Generate markdown documentation from POD inline documentation. This documentation is
meant to be visible on github.com.

A `README.md` file is also generated; it contains pointers to fxtran-acdc utilities.

# [fxtran-mergetool ...](./doc/fxtran-mergetool.md)

Wrap kdiff3, meld or vimdiff. Expand the following FORTRAN statements before calling merge utilities:

- call
- associate
- subroutine

Repack after merging.

# [fxtran-modules ...](./doc/fxtran-modules.md)

The purpose of this script is to compile all modules of fxtran-acdc 
and check they do not contain any errors.

# [fxtran-parallel-method ...](./doc/fxtran-parallel-method.md)

The purpose of this script is to extract the list of parallel methods embedded in the ELF 
sections of an executable.

The lists of possible methods, per ACDC section are saved in `.fxtran.acdc.*` ELF sections. We use
`objdump` and `objcopy` to extract this information and save it into plain text files.

# [fxtran-test ...](./doc/fxtran-test.md)

The purpose of this script is to run tests, that is, to pre-process
FORTRAN source code and compare the results to a reference.

The FORTRAN source code is supposed to be organized in a pack-like
directory.

See the `tests` directory of the fxtran-acdc repository for examples.
