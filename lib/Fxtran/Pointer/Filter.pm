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
  my $collapse = $parallel->getAttribute ('collapse') || 1;

  for my $call (&F ('.//call-stmt[string(procedure-designator)="?"]', $SUBROUTINE, $parallel))
    {
      my @arg = &F ('./arg-spec/arg', $call);
      for my $arg (@arg)
        {
          $arg->setAttribute (origin => 1);
        }
    }

  if ($collapse == 2)
    {
      &Fxtran::Decl::use ($pu, "USE FIELD_GATHSCAT_COLLAPSE${collapse}_MODULE");
      &Fxtran::Decl::declare ($pu, "TYPE(FIELD_GATHSCAT_COLLAPSE${collapse}) :: YL_FGS");
    }
  elsif ($collapse == 1)
    {
      &Fxtran::Decl::use ($pu, 'USE FIELD_GATHSCAT_MODULE');
      &Fxtran::Decl::declare ($pu, 'TYPE(FIELD_GATHSCAT) :: YL_FGS');
    }
  else
    {
      die;
    }
}

sub apply
{
  shift;

  my ($pu, $parallel, $t, %opts) = @_;

  my $collapse = $parallel->getAttribute ('collapse') || 1;

# pu = program unit
# parallel = parallel section
# t = symbol table

  my $filter = $parallel->getAttribute ('filter');

  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my ($comp    ) = &F ('./comp',    $parallel);
  my ($prep    ) = &F ('./prep',    $parallel);
  my ($nullify ) = &F ('./nullify', $parallel);
  my ($synchost) = &F ('.//synchost', $parallel);

  my %data;


# Change simple field GET by a GATHER operation

  for my $get (&F ('.//call-stmt[string(procedure-designator)="SGET_HOST_DATA_RDONLY" '
                       .    ' or string(procedure-designator)="SGET_HOST_DATA_RDWR" '
                       .    ' or string(procedure-designator)="SGET_DEVICE_DATA_RDONLY" '
                       .    ' or string(procedure-designator)="SGET_DEVICE_DATA_RDWR" '
                       .    ' ]', $prep))
    {
      my ($tt) = &F ('./procedure-designator/named-E/N/n/text()', $get);

      (my $t = $tt->data) =~ s/^SGET_/SGATHER_/go;
      $tt->setData ($t);


      my ($argspec) = &F ('./arg-spec', $get);
      $argspec->insertBefore ($_, $argspec->firstChild) for (&t(', '), &n ('<arg>' . &e ('YL_FGS')->toString . '</arg>'));

      my @arg = &F ('./arg', $argspec);

      my $n = $arg[1]->textContent;
      $data{$n} = 1;
    }

# Add pointers for gathered fields

  for my $n (sort keys (%data))
    {
      next if (&F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', "${n}_GATHER", $dp));

      my ($decl) = &F ('./T-decl-stmt[./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $n, $dp);

      my $declg = $decl->cloneNode (1);

      my ($en_decl) = &F ('.//EN-decl',, $declg);
      my ($t) = &F ('./EN-N/N/n/text()', $en_decl);

      $t->setData ("${n}_GATHER");
      $dp->insertAfter ($_, $decl) for ($declg, &t ("\n"));

      if ($collapse == 2) # Drop first dimension
        {
          my @ss = &F ('./array-spec/shape-spec-LT/node()', $en_decl);
          for ($ss[0], $ss[1])
            {
              $_->unbindNode ();
            }
        }
    }

  if ($collapse == 2) # Merge first two dimensions in compute section
    {
      for my $expr (&F ('.//named-E', $comp))
        {
          my $stmt = &Fxtran::stmt ($expr);
          next unless ($stmt); # expr may belong to an OpenACC or OpenMP directive

          my ($n) = &F ('./N', $expr, 1);
          next unless ($data{$n});

          my @ss = &F ('./R-LT/array-R/section-subscript-LT/node()', $expr);

          if ( @ss >= 3) 
            {
             for ($ss[1], $ss[2])
               {
                 $_->unbindNode ();
               }
            }
        }
    }
 
  # Replace ZFLD by ZFLD_GATHER
 
  for my $p ($prep, $comp, $nullify)
    {
      for my $n (&F ('.//named-E/N/n/text()', $p))
        {
          next unless ($data{$n->textContent});
          $n->setData ("${n}_GATHER");
        }
    }

  # Nullify pointers after synchronization on the host
  
  for my $pa (&F ('./call-stmt', $synchost))
    {
      my ($n) = &F ('./arg-spec/arg/named-E', $pa, 1);
      next unless ($data{$n});
      $synchost->insertAfter ($_, $pa) for (&s ("$n => NULL ()"), &t ("\n"));
    }

  # Initialize bounds with the set of gathered points 

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

  # Replace with target subroutine 

  my $SUBROUTINE = uc ($parallel->getAttribute ('subroutine'));


  my $SUB = $parallel->getAttribute ('targetsubroutine');

  if ($SUB)
    {
      $SUB = uc ($SUB);
    }
  else
    {
      ($SUB = $SUBROUTINE) =~ s/_SELECT$//o;
    }

  for my $call (&F ('.//call-stmt[starts-with(string(procedure-designator),"?")]', $SUBROUTINE, $parallel))
    {
      my ($proc) = &F ('./procedure-designator/ANY-E', $call);
      (my $suff = $proc->textContent) =~ s/^$SUBROUTINE//;
      $proc->replaceNode (&e ("$SUB$suff"));
    }

  # Scatter back after computations

  $comp->appendChild ($_) for (&t ("\n"), &s ('CALL YL_FGS%SCATTER ()'));

  # Load gather object on the device

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
