package Fxtran::Pointer::Filter;

use Data::Dumper;
use List::Util;

use strict;

use Fxtran;
use Fxtran::Decl;

sub preprocess
{
  shift;

  my ($pu, $parallel, $t, %opts) = @_;

  my $SUBROUTINE = uc ($parallel->getAttribute ('subroutine'));

  for my $call (&F ('.//call-stmt[string(procedure-designator)="?"]', $SUBROUTINE, $parallel))
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

  my ($pu, $parallel, $t, %opts) = @_;

# pu = program unit
# parallel = parallel section
# t = symbol table

  my $filter = $parallel->getAttribute ('filter');

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my ($comp) = &F ('./comp', $parallel);
  my ($prep) = &F ('./prep', $parallel);

  my %data;

# Change simple field GET by a GATHER operation

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

      my $stmt = &Fxtran::stmt ($get);
     
      my ($n) = &F ('./E-1', $stmt, 1);
      $data{$n} = 1;
    }

# Add pointers for gathered fields

  for my $n (sort keys (%data))
    {
      next if (&F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', "${n}_GATHER", $dp));

      my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $n, $dp);
      my $declg = $decl->cloneNode (1);
      my ($t) = &F ('.//EN-N/N/n/text()', $declg);
      $t->setData ("${n}_GATHER");
      $dp->insertAfter ($_, $decl) for ($declg, &t ("\n"));
    }

  for my $n (&F ('.//named-E/N/n/text()', $parallel))
    {
      next unless ($data{$n->textContent});
      $n->setData ("${n}_GATHER");
    }

  for my $call (&F ('.//call-stmt[string(procedure-designator)="YLCPG_BNDS%INIT"]', $parallel))
    {
      $call->replaceNode (&s ("CALL YLCPG_BNDS%INIT (YL_FGS%KLON, YL_FGS%KGPTOT)"));
    }

  for my $do_stmt_jblk (&F ('.//do-stmt[string(do-V)="JBLK"', $parallel))
    {
      my ($lb, $ub) = &F ('./ANY-bound/ANY-E', $do_stmt_jblk);
      $lb->replaceNode (&e ('1'));
      $ub->replaceNode (&e ('YL_FGS%KGPBLKS'));
    }

  for my $do_stmt_jlon (&F ('.//do-stmt[string(do-V)="JLON"', $parallel))
    {
      my ($lb, $ub) = &F ('./ANY-bound/ANY-E', $do_stmt_jlon);
      $ub->replaceNode (&e ('MIN (YL_FGS%KLON, YL_FGS%KGPTOT - (JBLK - 1) * YL_FGS%KLON)'));
    }

  $prep->insertBefore ($_, $prep->firstChild) for (&t ("\n"), &s ("CALL YL_FGS%INIT (YL_${filter}, YDCPG_OPTS%KGPTOTB)"));

  for my $stel (&F ('.//named-E[starts-with(string(N),"fxtran_acdc_stack_")]/R-LT/parens-R/element-LT', $parallel))
    {
      $_->unbindNode () for ($stel->childNodes ());
      $stel->appendChild ($_) for (&e ('YFXTRAN_ACDC_STACK'), &t (', '), &e ('(JBLK-1)+1'), &t (', '), &e ('YL_FGS%KGPBLKS'));
    }

  my $SUBROUTINE = uc ($parallel->getAttribute ('subroutine'));
  (my $SUB = $SUBROUTINE) =~ s/_SELECT$//o;

  for my $call (&F ('.//call-stmt[starts-with(string(procedure-designator),"?")]', $SUBROUTINE, $parallel))
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

  $comp->appendChild ($_) for (&t ("\n"), &s ('CALL YL_FGS%SCATTER ()'));

  my $pragma = $opts{pragma};

  for my $parallel ($pragma->findParallel ($comp))
    {
      for my $n (&t (' '), &n ("<cnt>&amp;</cnt>"), &t ("\n"), $pragma->sentinel (), &t (' '), &n ("<cnt>&amp;</cnt>"), &t (' '), $pragma->copyin ('YL_FGS'))
        {
          $parallel->appendChild ($n) 
        }
    }

}

1;
