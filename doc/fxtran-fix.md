# NAME

[fxtran-fix](../bin/fxtran-fix)

# SYNOPSIS

    $ FXTRANFIX=1 fxtran-fix --type xterm -- fxtran-f90 ...

    $ FXTRANFIX=1 FXTRANFIX_TYPE=agent FXTRANFIX_BREAK='^cuascn\.F90$' fxtran-fix -- fxtran-f90 ...

    $ FXTRANFIX=1 FXTRANFIX_TYPE=agent FXTRANFIX_BREAK='phys_ec/cu\w+\.F90$' fxtran-fix -- fxtran-f90 ...

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

# SESSION TYPES

The session type may be selected with the `--type` option or with the
`FXTRANFIX_TYPE` environment variable (the latter takes precedence over
the option, which is hard-coded in some compiler wrappers).

## xterm (default), screen

An interactive shell is started in a new xterm window or screen session.

## agent

A file-based rendezvous is created, meant for non-interactive sessions
(scripted tools, AI agents); no terminal is required. `fxtran-fix` creates
a session directory:

    $CMAKE_BUILD_DIRECTORY/.fxtran/fix-sessions/<pid>-<basename>-XXXXXX/

(the `XXXXXX` suffix is random, so that a session directory is always new,
even if a pid is reused by a later build) containing:

- SESSION

    Metadata about the session: trigger (error or break), date, pid, source
    file, working directory and full command line.

- env.sh

    The environment the command was started with, as a list of `export VAR='value'`
    statements.

- cwd

    The working directory the command was started from.

- run.sh

    A self-contained script re-running the exact command: it sources `env.sh`,
    changes to the saved working directory, and executes the command.

- DONE

    To be created by the agent when the session is over. If it contains the
    word `ABORT`, `fxtran-fix` exits with a non-zero status (and the build
    fails); otherwise the command is run once more and its status is returned
    to the build system.

- TIMEOUT

    Created by `fxtran-fix` if the agent showed no activity for
    `FXTRANFIX_TIMEOUT` seconds (see below); the session is then aborted (the
    build fails).

The build waits for the agent: `fxtran-fix` waits until the `DONE` file
appears, or until the timeout expires. Several sessions may be waiting at
the same time (parallel make); the following files, in the `fix-sessions`
directory itself, are shared by all sessions:

- DONE-ALL

    Terminates all sessions started before it was written, as if their own
    `DONE` file contained the same text (this text is copied to each session's
    `DONE`). This is useful when several files fail for the same reason
    (typically a code generation bug), and a single fix solves them all.
    A `DONE-ALL` older than a session is ignored by this session.

- ALIVE

    A heartbeat: touching it tells waiting sessions that the agent is still
    at work.

The timeout is measured from the last agent activity, not from the session
creation: writing a `DONE` file in any session, or touching `ALIVE`, resets
the clock of all waiting sessions. Sessions waiting for their turn therefore
do not time out while the agent is busy with another one; an agent working
for a long time on a single session should touch `ALIVE` periodically.

# ENVIRONMENT VARIABLES

- FXTRANFIX

    Enables the mechanism when non-null.

- FXTRANFIX\_TYPE

    Session type: `xterm`, `screen` or `agent`. Takes precedence over the
    `--type` option.

- FXTRANFIX\_BREAK

    A regular expression matched against the source file being compiled; when
    it matches, a session is started even if the compilation succeeds. This
    allows one to stop the build on a chosen file, typically to develop or
    debug code generation. The regular expression is matched against the
    basename of the file (eg `cuascn.F90`) and against the file name as passed
    on the command line (usually an absolute path, as with cmake); a session is
    started if either matches.

- FXTRANFIX\_TIMEOUT

    Agent sessions only: the maximum time (in seconds) without agent activity
    (see `ALIVE`) before a session is aborted; default is 7200 (2 hours).
    A value of 0 means no timeout. When the timeout expires, the session is
    aborted and the build fails.

- CMAKE\_BUILD\_DIRECTORY

    Used to locate the `fix-sessions` directory (agent sessions).

# SEE ALSO

[fxtran-f90](fxtran-f90.md)

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
