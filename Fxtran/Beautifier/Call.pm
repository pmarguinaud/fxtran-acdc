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

sub repack
{
  my ($stmt, $indent) = @_;
  my ($proc) = &F ('./procedure-designator', $stmt, 1);
  my @arg = &F ('./arg-spec/arg', $stmt, 1);
  &Fxtran::Beautifier::repackCallLikeStatement (\&reparse, "CALL $proc (", @arg, ")", $indent);
}

sub reparse
{
  my $stmt = shift;
  return &s ($stmt);
}

sub canonic
{
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));
  &parse (statement => $stmt, fopts => [qw (-line-length 10000 -canonic)]);
}

1;
