package Fxtran::DiffTool::Kdiff3;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::DiffTool::Kdiff3

=head1 DESCRIPTION

Backend for the kdiff3 diff tool. Implements the C<diff> method from
L<Fxtran::DiffTool> by invoking the C<kdiff3> external command to display
a side-by-side comparison of two files.

=cut

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['kdiff3', $local, $remote], debug => 0);
}

=head1 SEE ALSO

L<Fxtran::DiffTool>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
