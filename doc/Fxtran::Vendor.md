# NAME

[Fxtran::Vendor](../lib/Fxtran/Vendor.pm)

# DESCRIPTION

Factory class for vendor-specific compiler adaptors.  The constructor inspects
the compiler executable name and returns an appropriate subclass instance; for
AMD ROCm compilers (amdlang/amdflang) it returns a `Fxtran::Vendor::ROCM`
object.  The base class `preprocessOptions` method is a pass-through that
returns its arguments unchanged.

# FUNCTIONS
