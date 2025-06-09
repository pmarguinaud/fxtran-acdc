package Fxtran::Beautifier::Call;

use base qw (Fxtran::Beautifier);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub expand
{
  shift;
  my ($stmt, $indent) = @_;

  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  my ($proc) = &F ('./procedure-designator', $stmt, 1);

  $stmt = "CALL $proc (& \n  " . join (", ", map { "$indent $_ &\n" } @arg) . "$indent)";

  $stmt = &s ($stmt);

  return $stmt;
}

sub repack
{
  my $class = shift;
  my ($stmt, $indent) = @_;
  my ($proc) = &F ('./procedure-designator', $stmt, 1);
  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  $class->repackCallLikeStatement ("CALL $proc (", @arg, ")", $indent);
}

sub reparse
{
  shift;
  my $stmt = shift;
  return &s ($stmt);
}

sub canonic
{
  shift;
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));
  &parse (statement => $stmt, fopts => [qw (-line-length 10000 -canonic)]);
}

1;
