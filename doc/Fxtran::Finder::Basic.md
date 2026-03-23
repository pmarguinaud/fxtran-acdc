# NAME

[Fxtran::Finder::Basic](../lib/Fxtran/Finder/Basic.pm)

# DESCRIPTION

Base class for all finder implementations. Provides common logic for locating
interface files, supporting both the ARPEGE/IFS convention (`.intfb.h` and
`.h` files) and the MesoNH convention (`modi_*.F90` files). Concrete
subclasses must implement the `resolve` method.

# FUNCTIONS
