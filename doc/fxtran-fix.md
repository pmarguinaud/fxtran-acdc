The purpose of `fxtran-fix` is to trap compiler errors and give the user 
a chance to debug them, without interrupting the compiling process (cmake
or gmkpack).

When a compilation error occurs, `fxtran-fix` will catch it, if the environment
variable `FXTRANFIX` is not null, and create a new session (xterm, screen or
agent) which can be used to find and fix the issue.

In interactive shell sessions (xterm, screen), two commands are provided:

- e

    Edit the file currently being compiled.

- r

    Rerun the compiler command.

When the user is done with editing/debugging, he has to exit the shell; `fxtran-fix` 
will then attempt to compile the code once more and return control to the 
build system.

## Session types

The session type may be selected with the `--type` option or with the
`FXTRANFIX_TYPE` environment variable (the latter takes precedence over
the option, which is hard-coded in some compiler wrappers).

### xterm (default), screen

An interactive shell is started in a new xterm window or screen session.

### agent

A file-based rendezvous is created, meant for non-interactive sessions
(scripted tools, AI agents); no terminal is required. `fxtran-fix` creates
a session directory:

    $CMAKE_BUILD_DIRECTORY/.fxtran/fix-sessions/<pid>-<basename>/

containing:

- `SESSION` — metadata: trigger (error or break), date, pid, source file,
  working directory and full command line.
- `env.sh` — the environment the command was started with, as a list of
  `export VAR='value'` statements.
- `cwd` — the working directory the command was started from.
- `run.sh` — a self-contained script re-running the exact command: it
  sources `env.sh`, changes to the saved working directory, and executes
  the command.
- `DONE` — to be created by the agent when the session is over. If it
  contains the word `ABORT`, `fxtran-fix` exits with a non-zero status
  (and the build fails); otherwise the command is run once more and its
  status is returned to the build system.

`fxtran-fix` waits until the `DONE` file appears.

## Environment variables

- `FXTRANFIX` — enables the mechanism when non-null.
- `FXTRANFIX_TYPE` — session type: `xterm`, `screen` or `agent`; takes
  precedence over the `--type` option.
- `FXTRANFIX_BREAK` — a regular expression matched against the source file
  being compiled; when it matches, a session is started even if the
  compilation succeeds. This allows one to stop the build on a chosen file,
  typically to develop or debug code generation.
- `CMAKE_BUILD_DIRECTORY` — used to locate the `fix-sessions` directory
  (agent sessions).

# NAME

[fxtran-fix](../bin/fxtran-fix)

# SYNOPSIS

    $ FXTRANFIX=1 fxtran-fix --type xterm -- fxtran-f90 ...

    $ FXTRANFIX=1 FXTRANFIX_TYPE=agent FXTRANFIX_BREAK='^cuascn\.F90$' fxtran-fix -- fxtran-f90 ...

# DESCRIPTION

The purpose of `fxtran-fix` is to trap compiler errors and give the user 
a chance to debug them, without interrupting the compiling process (cmake
or gmkpack).

When a compilation error occurs, `fxtran-fix` will catch it, if the environment
variable `FXTRANFIX` is not null, and create a new session (xterm, screen or
agent) which can be used to find and fix the issue.

In interactive shell sessions (xterm, screen), two commands are provided:

- e

    Edit the file currently being compiled.

- r

    Rerun the compiler command.

When the user is done with editing/debugging, he has to exit the shell; `fxtran-fix` 
will then attempt to compile the code once more and return control to the 
build system.

Agent sessions provide a file-based rendezvous for non-interactive use:
`fxtran-fix` writes a session directory under
`$CMAKE_BUILD_DIRECTORY/.fxtran/fix-sessions/` (metadata, saved environment,
saved working directory, self-contained `run.sh`), waits for a `DONE` file,
and either aborts (if `DONE` contains `ABORT`) or re-runs the command once
more and returns its status to the build system.

A session can also be requested on a chosen file, even when the compilation
succeeds, by setting `FXTRANFIX_BREAK` to a regular expression matching the
source file name.

# SEE ALSO

[fxtran-f90](fxtran-f90.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
