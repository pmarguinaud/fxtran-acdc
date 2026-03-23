package Fxtran::MergeTool::Vimdiff;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::MergeTool::Vimdiff

=head1 DESCRIPTION

Backend for the vimdiff merge tool. Implements the C<merge> method from
L<Fxtran::MergeTool> by invoking the C<vimdiff> external command to perform
a three-way merge inside Vim. The base file is first copied to the merged
output path, which is then opened alongside the local and remote files for
interactive editing.

=cut

use base qw (Fxtran::MergeTool);

use strict;
use File::Copy;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  &copy ($base, $merged);

  $opts{runcommand}->(cmd => ['vimdiff', $local, $merged, $remote], debug => 0);
}

=head1 SEE ALSO

L<Fxtran::MergeTool>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
