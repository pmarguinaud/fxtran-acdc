package Fxtran::NVTX;

=head1 NAME

Fxtran::NVTX

=head1 DESCRIPTION

This module provides the C<drHookToNVTX> function, whose purpose is
to convert C<DR_HOOK> calls to C<NVTX> calls. 

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use strict;

use Fxtran::Common;
use Fxtran::Construct;
use Fxtran;

sub ifdef
{
  my ($macro, $stmt) = @_;

  my $p = $stmt->parentNode ();

  $p->insertBefore ($_, $stmt) for (&n ("<cpp>#ifdef $macro</cpp>"), &t ("\n"));
  $p->insertAfter ($_, $stmt) for (&n ('<cpp>#endif</cpp>'), &t ("\n"));
}

sub drHookToNVTX
{
  my $pu = shift;

  my $cpp = 1;

  my ($up) = &F ('./specification-part/use-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  return unless  (my ($use) = &F ('./use-stmt[string(module-N)="YOMHOOK"]', $up));

  $use->replaceNode (my $use_nvtx = &s ('USE NVTX, ONLY : NVTXSTARTRANGE, NVTXENDRANGE'));

  &ifdef (__PGI => $use_nvtx) if ($cpp);

  if (my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="ZHOOK_HANDLE"]]', $dp))
    {
      $decl->unbindNode ();
    }

  return unless (my ($ep) = &F ('./execution-part', $pu));

  &Fxtran::Construct::apply ($ep, './/named-E[string(N)="LHOOK"]' => &e ('.TRUE.'));

  for my $call (&F ('.//call-stmt[string(procedure-designator)="DR_HOOK"]', $ep))
    {
      my ($tag, $switch, $handle) = &F ('./arg-spec/arg/ANY-E', $call);

      my $call_nvtx = $switch->textContent eq '1'
                    ? &s ('CALL NVTXENDRANGE ()')
                    : &s ('CALL NVTXSTARTRANGE (' . $tag->textContent . ')');
        
      $call->replaceNode ($call_nvtx);

      &ifdef (__PGI => $call_nvtx) if ($cpp);
    }

}

1;
