# NAME

[Fxtran::Finder::CMake](../lib/Fxtran/Finder/CMake.pm)

# DESCRIPTION

Finder implementation for CMake-based projects. Resolves source files by first
searching the include paths (via [Fxtran::Finder::Include](Fxtran%3A%3AFinder%3A%3AInclude).md) and then falling
back to a full recursive scan of the CMake home directory read from
`CMakeCache.txt`. The environment variable `CMAKE_BUILD_DIRECTORY` must point
to the CMake build directory. Directories containing a `.fxtran_acdc_cmake_ignore`
file are excluded from the scan.

# FUNCTIONS
