package Fxtran::DiffTool::Vimdiff;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::DiffTool::Vimdiff

=head1 DESCRIPTION

Backend for the vimdiff diff tool. Implements the C<diff> method from
L<Fxtran::DiffTool> by invoking the C<vimdiff> external command to display
a side-by-side comparison of two files inside Vim.

=cut

use base qw (Fxtran::DiffTool);

use strict;

sub diff
{
  my $class = shift;
  my ($local, $remote) = splice (@_, 0, 2);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['vimdiff', $local, $remote], debug => 0);
}

=head1 SEE ALSO

L<Fxtran::DiffTool>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
