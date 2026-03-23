# NAME

[Fxtran::Finder::Basic](../lib/Fxtran/Finder/Basic.pm)

# DESCRIPTION

Base class for all finder implementations. Provides common logic for locating
interface files, supporting both the ARPEGE/IFS convention (`.intfb.h` and
`.h` files) and the MesoNH convention (`modi_*.F90` files). Concrete
subclasses must implement the `resolve` method.

# FUNCTIONS

## new

Constructor. Accepts a hash of keyword arguments and blesses them into the
class. Subclasses typically call `SUPER::new` and then add their own
initialisation on top.

## getInterface

Given a subroutine or function `name` (via the `name` named argument),
attempts to locate its interface file. First tries the ARPEGE/IFS convention
(`name.intfb.h`, then `name.h`); if neither is found it falls back to the
MesoNH convention (`modi_name.F90`). Returns the resolved path, or undef if
no interface file can be found. Relies on the `resolve` method implemented by
the concrete subclass.
