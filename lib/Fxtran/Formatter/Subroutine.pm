package Fxtran::Formatter::Subroutine;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Formatter::Subroutine

=head1 DESCRIPTION

Formatter for Fortran C<SUBROUTINE> statements. Inherits from
L<Fxtran::Formatter::block>. The C<expand> method rewrites the dummy argument
list so that each argument appears on its own continuation line. The C<repack>
method reassembles the argument list into a compact form respecting the
line-length limit.

=head1 FUNCTIONS

=cut

use base qw (Fxtran::Formatter::block);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{

=head2 expand

Takes a Fortran C<SUBROUTINE> statement node and an indentation string.
Normalises the statement to its canonical form, then rewrites the dummy
argument list so that each argument appears on its own continuation line.
Returns the re-parsed statement node.

=cut

  my $class = shift;
  my ($stmt, $indent) = @_;

  $stmt = $class->canonic ($stmt);

  my @arg = &F ('./dummy-arg-LT/arg-N', $stmt, 1);

  my ($name) = &F ('./subroutine-N', $stmt, 1);

  $stmt = "SUBROUTINE $name ( &\n$indent  " .  join ("\n$indent, ", map { "$_ & " } @arg) . "\n$indent)";

  $stmt = $class->reparse ($stmt);

  return $stmt;
}

sub repack
{

=head2 repack

Takes an expanded C<SUBROUTINE> statement node and an indentation string.
Extracts the subroutine name and the dummy argument list, then reassembles
them into a compact multi-line form that respects the line-length limit via
C<repackCallLikeStatement>.

=cut

  my $class = shift;
  my ($stmt, $indent) = @_;

  my @arg = &F ('./dummy-arg-LT/arg-N', $stmt, 1);

  my ($name) = &F ('./subroutine-N', $stmt, 1);

  $class->repackCallLikeStatement ("SUBROUTINE $name (", @arg, ")", $indent);
}

1;
