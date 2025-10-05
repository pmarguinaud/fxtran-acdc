package Fxtran::TopLevelSP;

use Data::Dumper;
use FileHandle;

use strict;

use Fxtran::Decl;
use Fxtran;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::Decl::use ($pu, &s ('USE FXTRAN_ACDC_PARALLELMETHOD_MOD'));

  my ($up) = &F ('./specification-part/use-part', $pu);

  $up->appendChild ($_) for (&t ("\n"), &s ('USE FIELD_ACCESS_MODULE,ONLY:GET_DEVICE_DATA_RDWR'));

  my ($name) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);

  my $count = 0;

  for my $par (&F ('.//parallel-section', $pu))
    {

      my @get;

      my $par0 = $par->cloneNode (1);

      my $par1 = $par->cloneNode (1);

      for my $call (&F ('.//call-stmt', $par1))
        {
          my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          next unless ((my $tt = $proc->textContent) =~ m/^SP\w+$/o);
          $proc->setData ($tt. $opts{'suffix-singleblock'});
          my ($argspec) = &F ('./arg-spec', $call);
          $argspec->appendChild ($_) 
            for (&t (", "), &n ('<arg><arg-N n="LDACC"><k>LDACC</k></arg-N>=' . &e ('.FALSE.') . '</arg>'));
        }

      my $par2 = $par1->cloneNode (1);

      for my $pa (&F ('.//pointer-a-stmt', $par2))
        {
          my ($E2) = &F ('./E-2/named-E', $pa);
          my ($N) = &F ('./N/n/text()', $E2);

          my $tt = $N->textContent;

          next unless ($tt =~ m/^GET_HOST_DATA_/o);

          push @get, $pa->cloneNode (1);

          $tt =~ s/^GET_HOST_DATA_/GET_DEVICE_DATA_/o;

          $N->setData ($tt);
        }

      for my $ldacc (&F ('.//call-stmt/arg-spec/arg[string(arg-N)="LDACC"]/ANY-E', $par2))
        {
          $ldacc->replaceNode (&t ('.TRUE.'));
        }

      my ($if_block_parallel) = &Fxtran::parse (fragment => << "EOF");
IF (FXTRAN_ACDC_LPARALLELMETHOD ('OPENACCSINGLEBLOCK', '$name:$count')) THEN
!
ELSEIF (FXTRAN_ACDC_LPARALLELMETHOD ('SINGLEBLOCK', '$name:$count')) THEN
!
ELSE
!
ENDIF
EOF

      my ($C2, $C1, $C0) = &F ('.//C', $if_block_parallel);

      $C2->replaceNode ($par2);
      $C1->replaceNode ($par1);
      $C0->replaceNode ($par0);

      $par->replaceNode ($if_block_parallel);

      # Synchronization to host

      my ($if_block_synchost) = &Fxtran::parse (fragment => << "EOF");
IF (FXTRAN_ACDC_LSYNCHOST ('$name:$count')) THEN
!
ENDIF
EOF

      my ($C) = &F ('.//C', $if_block_synchost);

      for my $get (@get)
        {
          $C->parentNode->insertBefore ($_, $C) for ($get, &t ("\n"));
        }

      $C->unbindNode ();

      $if_block_parallel->parentNode->insertAfter ($_, $if_block_parallel)
         for ($if_block_synchost, &t ("\n"));

      $count++;
    }

}


1;
