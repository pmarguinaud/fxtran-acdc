# NAME

[Fxtran::Tool](../lib/Fxtran/Tool.pm)

# DESCRIPTION

General-purpose helper utilities for the fxtran-acdc toolchain.  Provides a
simple logging function (`ll`) that writes caller location and message to a
log file, `runCommand` to execute a shell command and die on failure (with an
optional interactive debug mode that opens an xterm), and `which` to locate
an executable in `$PATH`.

# FUNCTIONS

## ll

Append the current file/line location and a message to `fxtran-tool.txt`,
opening the log file on first use.

## debugCommand

Open an interactive xterm with the failing command pre-aliased, letting the
developer re-run or edit the offending file in an interactive shell session.

## runCommand

Execute a shell command and die with a descriptive message on failure.  When
`debug` is set, opens a `debugCommand` xterm before retrying once.

## which

Search `$PATH` for `$prog` and return the first executable path found, or
`undef` if not found.
