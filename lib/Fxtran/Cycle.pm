package Fxtran::Cycle;

use strict;

sub simplify
{
  shift;

  my ($d, %opts) = @_;
  my $cycle = $opts{cycle};

  my $class = 'Fxtran::Cycle' . $cycle;

  eval "use $class";
  my $c = $@;

  die ($c) if ($c);

  $class->simplify ($d, %opts);
}

1;
