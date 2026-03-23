package Fxtran::DrHook;

=head1 NAME

Fxtran::DrHook

=head1 DESCRIPTION

This module provides utilities to remove DrHook calls.

=head1 SEE ALSO

L<Fxtran::NVTX>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

use strict;

use Fxtran;

sub remove
{

=head2 remove

Remove all DrHook instrumentation from the given program-unit XML node C<$d>.
This covers three kinds of nodes:

=over 4

=item *

C<USE YOMHOOK> statements in the use-part.

=item *

C<IF (...) CALL DR_HOOK (...)> statements in the execution-part.

=item *

Declarations of C<ZHOOK_HANDLE*> variables in the declaration-part.

=back

=cut

  my $d = shift;

  my ($ep) = &F ('./execution-part', $d);
  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($up) = &F ('./specification-part/use-part', $d);

  for my $use (&F ('./use-stmt[string(module-N)="YOMHOOK"]', $up)) 
    {
      $use->unbindNode ();
    }

  for my $call (&F ('.//if-stmt[.//call-stmt[string(procedure-designator)="DR_HOOK"]]', $ep)) 
    {
      $call->unbindNode ();
    }

  for my $decl (&F ('./T-decl-stmt[.//EN-decl[starts-with(string(EN-N),"ZHOOK_HANDLE")]]', $dp))
    {
      $decl->unbindNode ();
    }
}

1;
