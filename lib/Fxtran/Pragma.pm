package Fxtran::Pragma;

use strict;

sub new
{
  my $class = shift;
  my %args = @_;

  if ($args{pragma})
    {
      my $class = "Fxtran::Pragma::$args{pragma}";
      eval "use $class";
      $@ && die ($@);
      return $class->new ();
    }
  elsif ($class ne __PACKAGE__)
    {
      return bless \%args, $class;
    }
}

1;
