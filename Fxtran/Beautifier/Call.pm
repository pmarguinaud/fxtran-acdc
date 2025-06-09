package Fxtran::Beautifier::Call;

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
  my ($stmt, $indent) = @_;

  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  my ($proc) = &F ('./procedure-designator', $stmt, 1);

  $stmt = "CALL $proc (& \n  " . join (", ", map { "$indent $_ &\n" } @arg) . "$indent)";

  $stmt = &s ($stmt);

  return $stmt;
}

1;
