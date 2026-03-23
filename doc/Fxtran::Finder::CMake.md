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

## new

Constructor. Calls the parent constructor, then reads the
`CMAKE_BUILD_DIRECTORY` environment variable (converting it to an absolute
path) and instantiates an [Fxtran::Finder::Include](Fxtran%3A%3AFinder%3A%3AInclude.md) helper that will be tried
first on every `resolve` call.

## resolve

Resolves a filename (passed as the `file` named argument) to an absolute
path. First delegates to [Fxtran::Finder::Include](Fxtran%3A%3AFinder%3A%3AInclude.md) (which searches the
`-I` include paths). If that fails, falls back to
`resolveInCMakeHomeDirectory`. Returns the absolute path on success, or undef
if the file cannot be found.

## resolveInCMakeHomeDirectory

Resolves a filename against the CMake home directory. On the first call,
parses `CMakeCache.txt` to discover `CMAKE_HOME_DIRECTORY` and then builds a
basename-to-path index by recursively scanning that directory (directories
containing a `.fxtran_acdc_cmake_ignore` file are skipped). Subsequent calls
reuse the cached index. Dies if the file is found in more than one location.
Returns the absolute path, or undef if the file is not present in the tree.
