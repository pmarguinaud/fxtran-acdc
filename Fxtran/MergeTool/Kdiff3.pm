package Fxtran::MergeTool::Kdiff3;

use base qw (Fxtran::MergeTool);

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  &Fxtran::Formatter::runCommand (cmd => ['kdiff3', -o => $merged, $base, $local, $remote], debug => 0);
}

1;
