package Fxtran::PATH;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::PATH

=head1 DESCRIPTION

Automatically prepends the C<bin/> directory of the fxtran-acdc installation
to C<$ENV{PATH}> when the module is loaded.  The installation root is derived
from the location of this module file inside C<@INC>.

=cut

use strict;
use File::Basename;

(my $pm = __PACKAGE__) =~ s,::,/,go;
$pm .= '.pm';

my $TOP = $INC{$pm};

for (1 .. 3)
  {
    $TOP = &dirname ($TOP);
  }

$ENV{PATH} = "$TOP/bin:$ENV{PATH}";



1;
