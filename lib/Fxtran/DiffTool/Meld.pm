package Fxtran::DiffTool::Meld;

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['meld', $local, $remote], debug => 0);
}

1;
