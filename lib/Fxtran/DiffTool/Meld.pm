package Fxtran::DiffTool::Meld;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::DiffTool::Meld

=head1 DESCRIPTION

Backend for the meld diff tool. Implements the C<diff> method from
L<Fxtran::DiffTool> by invoking the C<meld> external command to display
a side-by-side comparison of two files.

=cut

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['meld', $local, $remote], debug => 0);
}

=head1 SEE ALSO

L<Fxtran::DiffTool>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
