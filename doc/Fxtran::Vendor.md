# NAME

[Fxtran::Vendor](../lib/Fxtran/Vendor.pm)

# DESCRIPTION

Factory class for vendor-specific compiler adaptors.  The constructor inspects
the compiler executable name and returns an appropriate subclass instance; for
AMD ROCm compilers (amdlang/amdflang) it returns a `Fxtran::Vendor::ROCM`
object.  The base class `preprocessOptions` method is a pass-through that
returns its arguments unchanged.

## new

Constructor.  Takes a compiler executable path as its sole argument.  If the
basename of the executable matches `amd.*lang` (i.e. amdlang or amdflang),
returns a blessed `Fxtran::Vendor::ROCM` object; otherwise returns a
plain `Fxtran::Vendor` object.

## preprocessOptions

Base-class pass-through.  Returns its argument list unchanged.  Subclasses
override this method to apply vendor-specific transformations to the compiler
argument list before it is passed to the preprocessor invocation.

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
