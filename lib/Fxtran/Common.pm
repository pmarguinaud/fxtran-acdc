package Fxtran::Common;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use strict;

sub import
{
  my $c;

  eval "use local::lib";

  return unless ($c = $@);

  eval "use Fxtran::local::lib";
      
  if ($c = $@)
    {
      die ($c);
    }
}

1;
