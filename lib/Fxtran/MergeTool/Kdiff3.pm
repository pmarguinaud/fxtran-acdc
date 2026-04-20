package Fxtran::MergeTool::Kdiff3;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::MergeTool::Kdiff3

=head1 DESCRIPTION

Backend for the kdiff3 merge tool. Implements the C<merge> method from
L<Fxtran::MergeTool> by invoking the C<kdiff3> external command to perform
a three-way merge of base, local, and remote files, writing the result to
the merged output file.

=cut

use base qw (Fxtran::MergeTool);

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['kdiff3', -o => $merged, $base, $local, $remote], debug => 0);
}

=head1 SEE ALSO

L<Fxtran::MergeTool>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
