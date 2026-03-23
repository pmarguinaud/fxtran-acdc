# NAME

[Fxtran::local::lib](../lib/Fxtran/local/lib.pm)

# DESCRIPTION

A bundled, self-contained copy of `local::lib` (version 2.000029) shipped
with fxtran-acdc as a fallback for systems where the `local::lib` CPAN module
is not installed.  It sets up a local Perl library tree by adjusting `@INC`,
`PERL5LIB`, `PATH`, `PERL_LOCAL_LIB_ROOT`, and the `PERL_MM_OPT` /
`PERL_MB_OPT` environment variables.  All shell types supported by the
upstream `local::lib` module (bourne, csh, fish, cmd, powershell) are
handled.
