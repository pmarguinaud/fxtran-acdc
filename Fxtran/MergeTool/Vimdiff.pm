package Fxtran::MergeTool::Vimdiff;

use base qw (Fxtran::MergeTool);

use strict;
use File::Copy;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  &copy ($base, $merged);

  &Fxtran::Beautifier::runCommand (cmd => ['vimdiff', $local, $merged, $remote], debug => 0);
}

1;
