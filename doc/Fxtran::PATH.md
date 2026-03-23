# NAME

[Fxtran::PATH](../lib/Fxtran/PATH.pm)

# DESCRIPTION

Automatically prepends the `bin/` directory of the fxtran-acdc installation
to `$ENV{PATH}` when the module is loaded.  The installation root is derived
from the location of this module file inside `@INC`.
