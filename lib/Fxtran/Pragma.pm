package Fxtran::Pragma;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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
