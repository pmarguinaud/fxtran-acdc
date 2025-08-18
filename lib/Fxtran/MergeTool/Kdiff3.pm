package Fxtran::MergeTool::Kdiff3;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::MergeTool);

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['kdiff3', -o => $merged, $base, $local, $remote], debug => 0);
}

1;
