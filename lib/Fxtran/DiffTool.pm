package Fxtran::DiffTool;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $class = join ('::', $class, ucfirst ($opts{difftool}));

  &Fxtran::Formatter::loadClass ($class) 
    or die ("Cannot load $class\n");

  $class->diff ($local, $remote, %opts);
}

1;
