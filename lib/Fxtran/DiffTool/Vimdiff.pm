package Fxtran::DiffTool::Vimdiff;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['vimdiff', $local, $remote], debug => 0);
}

1;
