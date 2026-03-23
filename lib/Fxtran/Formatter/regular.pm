package Fxtran::Formatter::regular;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Formatter::regular

=head1 DESCRIPTION

Base class for formatters that handle ordinary (non-block) Fortran statements.
Inherits from L<Fxtran::Formatter>. Provides C<canonic> and C<reparse> methods
that parse a single statement in isolation using fxtran, without needing a
matching C<END> construct.

=cut

use base qw (Fxtran::Formatter);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub reparse
{
  shift;
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));
  return &s ($stmt);
}

sub canonic
{
  shift;
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));
  return &parse (statement => $stmt, fopts => [qw (-line-length 10000 -canonic)]);
}

1;
