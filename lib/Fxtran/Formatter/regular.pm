package Fxtran::Formatter::regular;

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
