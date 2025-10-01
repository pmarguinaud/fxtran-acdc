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
# NAME

[fxtran-ar](../bin/fxtran-ar)

# DESCRIPTION

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

# COPYRIGHT

Meteo-France 2025

# AUTHOR

philippe.marguinaud@meteo.fr

# SEE ALSO

`ar`, `fxtran-f90`
