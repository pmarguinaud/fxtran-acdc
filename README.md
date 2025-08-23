# Adaptation des Codes à Divers Calculateurs

![](./images/ACDC.png)


# [fxtran-f90 ...](./doc/fxtran-f90.md)

`fxtran-f90` is compiler wrapper performing the following tasks:

- Apply some transformations on user-written source code and generate FORTRAN source code ready 
for GPU accelerators.
- Compile the original code and code produced at previous step.
- Link all objects into a single one.

`fxtran-f90` relies on `fxtran` for parsing and transforming FORTRAN source code.

# [fxtran-boot ...](./doc/fxtran-boot.md)

`fxtran-boot` compiles and install the fxtran-acdc libraries. Compilers and
their options may be passed directly as options, or inferred from the `ecbuild`
environment.
