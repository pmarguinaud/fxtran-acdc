# NAME

[Fxtran::Message](../lib/Fxtran/Message.pm)

# DESCRIPTION

Simple diagnostic output module.  Formats and prints human-readable warning
or error messages, optionally accompanied by the text of the Fortran statement
that triggered the message.  The `error` function additionally terminates the
program after printing the message.

## message

Print a formatted diagnostic message to standard output.  The output is
preceded by a line of 80 dashes.  If a second argument `$stmt` (a DOM node)
is given, the message is printed as a label followed by the wrapped text
content of the statement; otherwise only the wrapped message text is printed.

## error

Print a diagnostic message via `message` and then terminate the program by
calling `die` with a bare newline (so that no additional "at file line N"
suffix is appended).
