package Fxtran::Finder;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use Data::Dumper;

use strict;

use Fxtran::Finder::Pack;
use Fxtran::Finder::Pack::Build;
use Fxtran::Finder::Include;
use Fxtran::Finder::Files;
use Fxtran::Finder::CMake;

sub new
{
  my $class = shift;

  my %args;

  if ((scalar (@_) % 2) == 0)
    {
      %args = @_;
    }

  if ($ENV{CMAKE_BUILD_DIRECTORY})
    {
      return 'Fxtran::Finder::CMake'->new (@_);
    }
  elsif ($ENV{TARGET_PACK})
    {
      return 'Fxtran::Finder::Pack::Build'->new (@_);
    }
  elsif (-f '.gmkview')
    { 
      return 'Fxtran::Finder::Pack'->new (@_);
    }
  elsif ($args{files} && @{ $args{files} })
    {
      return 'Fxtran::Finder::Files'->new (@_);
    }
  else
    {
      return 'Fxtran::Finder::Include'->new (@_);
    }
}

1;
