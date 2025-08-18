package Fxtran::Formatter::Subroutine;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Formatter::block);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
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
  my $class = shift;
  my ($stmt, $indent) = @_;

  my @arg = &F ('./dummy-arg-LT/arg-N', $stmt, 1);

  my ($name) = &F ('./subroutine-N', $stmt, 1);

  $class->repackCallLikeStatement ("SUBROUTINE $name (", @arg, ")", $indent);
}

1;
