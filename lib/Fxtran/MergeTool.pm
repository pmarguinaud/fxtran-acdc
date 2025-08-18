package Fxtran::MergeTool;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  $class = join ('::', $class, ucfirst ($opts{mergetool}));

  &Fxtran::Formatter::loadClass ($class) 
    or die ("Cannot load $class\n");

  $class->merge ($base, $local, $remote, $merged, %opts);
}

1;
