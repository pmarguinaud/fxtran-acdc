package Fxtran::Style::NONE;

=head1 NAME

Fxtran::Style::NONE

=head1 DESCRIPTION

A no-op style class used as a neutral fallback when no specific coding
style is applicable or when style-specific behaviour should be suppressed.
This class derives from C<Fxtran::Style> and overrides all style-sensitive
methods to return empty or false values: unused includes are never removed,
no routine is flagged as a non-compute routine, OpenACC pre-processing is
a no-op, and there is no custom iterator or actual nproma.

=cut

use strict;

use base qw (Fxtran::Style);

use Fxtran;

sub removeUnusedIncludes
{
  return 0;
}

sub noComputeRoutine
{
  return 0;
}

sub preProcessForOpenACC
{

}

sub customIterator
{
}

sub updateCustomIterator
{
}

sub getActualNproma
{
}

1;
