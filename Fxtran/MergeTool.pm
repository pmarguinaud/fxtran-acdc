package Fxtran::MergeTool;

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  $class = join ('::', $class, ucfirst ($opts{mergetool}));

  &Fxtran::Beautifier::loadClass ($class) 
    or die ("Cannot load $class\n");

  $class->merge ($base, $local, $remote, $merged, %opts);
}

1;
