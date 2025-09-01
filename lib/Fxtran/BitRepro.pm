package Fxtran::BitRepro;

=head1 NAME

Fxtran::BitRepro

=head1 DESCRIPTION

The purpose of this module is to provide functions to make the code bit-reproducible
when results are compared between the CPU and the GPU; this involves two steps:

=over 4

=item

Replacing transcendental intrinsics with portable functions.

=item

Adding brackets in additions to force the compiler to add
number in a well defined order.

=back

The generated code has to be compiled with the C<-O0> option, so that results 
be reproduced between CPU and GPU.

=head1 SEE ALSO

L<fxtran_acdc_br_intrinsics.F90|url:../src/fxtran_acdc_br_intrinsics.F90>,
L<fxtran_acdc_br_transcendentals.cc|url:../src/fxtran_acdc_br_transcendentals.cc>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran::Intrinsic;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Module;
use Fxtran;

sub makeBitReproducible
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      my $stmt = $pu->firstChild;
      (my $kind = $stmt->nodeName) =~ s/-stmt$//o;

      if ($kind eq 'module')
        {
          &processSingleModule ($pu, %opts);
        }
      elsif ($kind eq 'subroutine')
        {
          &processSingleRoutine ($pu, %opts);
        }
      else
        {
          die;
        }

    }

}

sub processSingleModule
{
  my ($d, %opts) = @_;

  for my $pu (&F ('./program-unit', $d))
    {
      &processSingleRoutine ($pu, %opts);
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-bitrepro'});

}

sub traverseAdditionSubstractionExpr
{
  my $expr = shift;

  my ($e1, $e2) = &F ('./ANY-E', $expr);
  my ($op) = &F ('./op', $expr);

  return ($expr) unless ($e1 && $op && $e2);

  my $ops = $op->textContent;

  return ($expr) unless (($ops eq '+') || ($ops eq '-'));

  return (&traverseAdditionSubstractionExpr ($e1), $op, &traverseAdditionSubstractionExpr ($e2));
}

sub addBitReproParens
{
  my $s = shift;


  for my $expr (&F ('.//op-E[(string(./op)="+" or string (./op)="-") and count (./ANY-E)=2][not(parent::op-E[string(./op)="+" or string (./op)="-"])]', $s))
    {
      my @o = &traverseAdditionSubstractionExpr ($expr);
  
      my $e = shift (@o);
  
      while (my ($op, $e2) = splice (@o, 0, 2))
        {
          my $s = &n ('<op-E/>');
          $s->appendChild ($_) for ($e, $op, $e2);
  
          if (@o)
            {
              my $p = &n ('<parens-E/>');
              $p->appendChild ($_) for (&t ('('), $s, &t (')'));
              $e = $p;
            }
          else
            {
              $e = $s;
            }
        }
  
      $expr->replaceNode ($e);
    }


}

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

my $dbg = $pu->textContent =~ m/computes the non-conservative variables/goms;

print &Dumper ([$pu->toString]) if ($dbg);


  &Fxtran::Intrinsic::makeBitReproducible ($pu, %opts);

  my ($ep) = &F ('./execution-part', $pu);

  if ($opts{'use-bit-repro-parens'})
    {
      &addBitReproParens ($ep);
    }

  &Fxtran::Call::addSuffix 
  (
    $pu,
    section => $ep,
    suffix => $opts{'suffix-bitrepro'},
    'merge-interfaces' => $opts{'merge-interfaces'},
    match => sub { my $proc = shift; ($proc ne 'ABOR1') && ($proc ne 'PRINT_MSG') },
    contained => 1,
  );

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-bitrepro'});

  for my $pu (&F ('./program-unit', $pu))
    {
      &processSingleRoutine ($pu, %opts);
    }

}

1;
