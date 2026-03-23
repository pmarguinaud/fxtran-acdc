package Fxtran::MergeTool::Meld;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::MergeTool::Meld

=head1 DESCRIPTION

Backend for the meld merge tool. Implements the C<merge> method from
L<Fxtran::MergeTool> by invoking the C<meld> external command to perform
a three-way merge of base, local, and remote files, writing the result to
the merged output file.

=head1 FUNCTIONS

=cut

use base qw (Fxtran::MergeTool);

use strict;

sub merge
{
  my $class = shift;
  my ($base, $local, $remote, $merged) = splice (@_, 0, 4);
  my %opts = @_;

  $opts{runcommand}->(cmd => ['meld', -o => $merged, $base, $local, $remote], debug => 0);
}

1;
