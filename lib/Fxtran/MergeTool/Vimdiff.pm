package Fxtran::MergeTool::Vimdiff;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::MergeTool);

use strict;
use File::Copy;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  &copy ($base, $merged);

  $opts{runcommand}->(cmd => ['vimdiff', $local, $merged, $remote], debug => 0);
}

1;
