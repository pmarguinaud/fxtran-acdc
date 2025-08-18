package Fxtran::Formatter::Call;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Formatter::regular);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
  my $class = shift;
  my ($stmt, $indent) = @_;

  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  my ($proc) = &F ('./procedure-designator', $stmt, 1);

  $stmt = "CALL $proc (& \n  " . join (", ", map { "$indent $_ &\n" } @arg) . "$indent)";

  $stmt = $class->reparse ($stmt);

  return $stmt;
}

sub repack
{
  my $class = shift;
  my ($stmt, $indent) = @_;
  my ($proc) = &F ('./procedure-designator', $stmt, 1);
  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  return $class->repackCallLikeStatement ("CALL $proc (", @arg, ")", $indent);
}

1;
