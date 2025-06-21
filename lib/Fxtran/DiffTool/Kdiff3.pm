package Fxtran::DiffTool::Kdiff3;

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['kdiff3', $local, $remote], debug => 0);
}

1;
