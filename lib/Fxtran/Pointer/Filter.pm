package Fxtran::Pointer::Filter;

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Decl;

sub preprocess
{
  shift;

  my ($pu, $p, %opts) = @_;

  my $SUBROUTINE = uc ($p->getAttribute ('subroutine'));

  for my $call (&F ('.//call-stmt[string(procedure-designator)="?"]', $SUBROUTINE, $p))
    {
      my @arg = &F ('./arg-spec/arg', $call);
      for my $arg (@arg)
        {
          $arg->setAttribute (origin => 1);
        }
    }

  &Fxtran::Decl::use ($pu, 'USE FIELD_GATHSCAT_MODULE');
  &Fxtran::Decl::declare ($pu, 'TYPE(FIELD_GATHSCAT) :: YL_FGS');
}

sub apply
{
  shift;

  my ($pu, $p, %opts) = @_;

  my ($prep) = &F ('./prep', $p);

  for my $get (&F ('.//named-E[string(N)="GET_HOST_DATA_RDONLY" '
                    .     ' or string(N)="GET_HOST_DATA_RDWR" '
                    .     ' or string(N)="GET_DEVICE_DATA_RDONLY" '
                    .     ' or string(N)="GET_DEVICE_DATA_RDWR" '
                    .     ' ]', $prep))
    {
      my ($tt) = &F ('./N/n/text()', $get);

      (my $t = $tt->data) =~ s/^GET_/GATHER_/go;
      $tt->setData ($t);

      my ($elt) = &F ('./R-LT/parens-R/element-LT', $get);
      $elt->insertBefore ($_, $elt->firstChild) for (&t(', '), &e ('YL_FGS'));
    }

  for my $call (&F ('.//call-stmt[string(procedure-designator)="YLCPG_BNDS%INIT"]', $p))
    {
      $call->replaceNode (&s ("CALL YLCPG_BNDS%INIT (YL_FGS%KLON, YL_FGS%KGPTOT)"));
    }

  for my $do_stmt_jblk (&F ('.//do-stmt[string(do-V)="JBLK"', $p))
    {
      my ($lb, $ub) = &F ('./ANY-bound/ANY-E', $do_stmt_jblk);
      $lb->replaceNode (&e ('1'));
      $ub->replaceNode (&e ('YL_FGS%KGPBLKS'));
    }

  for my $do_stmt_jlon (&F ('.//do-stmt[string(do-V)="JLON"', $p))
    {
      my ($lb, $ub) = &F ('./ANY-bound/ANY-E', $do_stmt_jlon);
      $ub->replaceNode (&e ('MIN (YL_FGS%KLON, YL_FGS%KGPTOT - (JBLK - 1) * YL_FGS%KLON)'));
    }

  $prep->insertBefore ($_, $prep->firstChild) for (&t ("\n"), &s ("CALL YL_FGS%INIT (YL_LLTRIG1, YDCPG_OPTS%KGPTOTB)"));

  for my $stel (&F ('.//named-E[starts-with(string(N),"fxtran_acdc_stack_")]/R-LT/parens-R/element-LT', $p))
    {
      $_->unbindNode () for ($stel->childNodes ());
      $stel->appendChild ($_) for (&e ('YFXTRAN_ACDC_STACK'), &t (', '), &e ('(JBLK-1)+1'), &t (', '), &e ('YL_FGS%KGPBLKS'));
    }

  my $SUBROUTINE = uc ($p->getAttribute ('subroutine'));
  (my $SUB = $SUBROUTINE) =~ s/_SELECT$//o;

  for my $call (&F ('.//call-stmt[starts-with(string(procedure-designator),"?")]', $SUBROUTINE, $p))
    {
      my ($proc) = &F ('./procedure-designator/ANY-E', $call);
      (my $suff = $proc->textContent) =~ s/^$SUBROUTINE//;
      $proc->replaceNode (&e ("$SUB$suff"));

      my @arg = grep { $_->getAttribute ('origin')  } &F ('./arg-spec/arg', $call);
      
      for ($arg[-1]->previousSibling, $arg[-1])
        {
          $_->unbindNode ();
        }
    }

  my ($comp) = &F ('./comp', $p);

  $comp->appendChild ($_) for (&t ("\n"), &s ('CALL YL_FGS%SCATTER ()'));

  my @C = &F ('./C[starts-with(string(.),"!$")]', $comp);

  for my $C (@C)
    {
      if ($C->textContent =~ m/^!\$ACC PARALLEL LOOP GANG/o)
        {
          my $p = $C->parentNode;
          $p->insertAfter ($_, $C) for (&n ("<C>!\$ACC&amp;COPYIN (YL_FGS) &amp;</C>"), &t ("\n"));
        }
    }



}

1;
