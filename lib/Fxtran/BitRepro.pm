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

=head2 makeBitReproducible

Entry point for the bit-reproducibility transformation. Iterates over all
program units in the document and dispatches to C<processSingleModule> or
C<processSingleRoutine> depending on the kind of program unit.

=cut

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

=head2 processSingleModule

Apply the bit-reproducibility transformation to every program unit contained
in a module, and then rename the module by appending the bit-repro suffix.

=cut

  my ($d, %opts) = @_;

  for my $pu (&F ('./program-unit', $d))
    {
      &processSingleRoutine ($pu, %opts);
    }

  &Fxtran::Module::addSuffix ($d, $opts{'suffix-bitrepro'});

}

sub traverseAdditionSubstractionExpr
{

=head2 traverseAdditionSubstractionExpr

Recursively decompose an addition/subtraction expression into a flat list of
operand and operator nodes. Returns the expression unchanged if it is not a
two-operand addition or subtraction.

=cut

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

=head2 addBitReproParens

Insert explicit parentheses around addition/subtraction chains in a statement
so that the evaluation order is fixed and results are reproducible across
different compiler optimisation levels.

=cut

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

=head2 processSingleRoutine

Apply the full bit-reproducibility transformation to a single subroutine: replace
transcendental intrinsics, optionally bracket additions, rename call targets and
the subroutine itself with the bit-repro suffix, then recurse into contained
program units.

=cut

  my ($pu, %opts) = @_;

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


=head1 SEE ALSO

L<fxtran_acdc_br_intrinsics.F90|url:../src/fxtran_acdc_br_intrinsics.F90>,
L<fxtran_acdc_br_transcendentals.cc|url:../src/fxtran_acdc_br_transcendentals.cc>,
L<Fxtran::Intrinsic>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;
