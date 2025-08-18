package Fxtran::Formatter::block;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Formatter);

use strict;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

sub endStatement
{
  my $class = shift;
  $class =~ s/.*:://o;
  return "END " . uc ($class);
}

sub canonic
{
  my $class = shift;
  my $stmt = shift;
  $stmt = $stmt->textContent if (ref ($stmt));

  my $endStmt = $class->endStatement ();
  ($stmt) = &parse (fragment => "$stmt\n$endStmt\n", fopts => [qw (-line-length 10000 -canonic)]);

  return $stmt;
}

sub reparse
{
  my $class = shift;
  my $stmt = shift;

  my $endStmt = $class->endStatement ();
  ($stmt) = &parse (fragment => "$stmt\n$endStmt\n", fopts => [qw (-line-length 10000)]);

  return $stmt;
}

1;
