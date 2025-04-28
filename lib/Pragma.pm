package Pragma;

use strict;

sub new
{
  my $class = shift;
  my %args = @_;
  if ($args{pragma})
    {
      my $class = "Pragma::$args{class}";
      eval "use $class";
      $@ && die ($@);
      return $class->new ();
    }
  else
    {
      return;
    }
  return $class->new (%args);
}

1;
