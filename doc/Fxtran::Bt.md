# NAME

[Fxtran::Bt](../lib/Fxtran/Bt.pm)

# DESCRIPTION

This module provides the `bt` function and uses it to handle `__WARN__` and
`__DIE__` Perl signal. The `bt` function will then print a stack trace 
and abort when an error or a warnings is encountered.

# SEE ALSO

[Devel::StackTrace](Devel%3A%3AStackTrace.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2022

## showFrame

Print a single stack frame to standard output.  Given the filename, line
number, subroutine name, and argument list of a caller frame, this routine
draws a separator, formats the call signature (with arguments abbreviated
for blessed references, hashes and arrays), and then prints a short context
window (`$size` lines before and after) of the source file around the
indicated line.

## bt

Backtrace handler installed for both `$SIG{__WARN__}` and `$SIG{__DIE__}`.
Prints the message, then walks the call stack.  If `Devel::StackTrace` is
available each frame is rendered via `showFrame`; otherwise a compact
filename:line listing is used.  After printing the trace the handler calls
`die "\n"` to abort the program.
