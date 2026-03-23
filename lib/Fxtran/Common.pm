package Fxtran::Common;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Common

=head1 DESCRIPTION

Bootstrap module that sets up the local::lib environment.  When imported, it
tries to load the system C<local::lib> and falls back to the bundled
C<Fxtran::local::lib> if the system one is unavailable.

=cut

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

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
