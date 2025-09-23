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

  my ($name) = &F ('./subroutine-stmt/subroutine-N', $pu, 1);

  my $count = 0;

  for my $par (&F ('.//parallel-section', $pu))
    {
      my $par0 = $par->cloneNode (1);

      my $par1 = $par->cloneNode (1);

      for my $call (&F ('.//call-stmt', $par1))
        {
          my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
          my ($argspec) = &F ('./arg-spec', $call);
          $argspec->appendChild ($_) 
            for (&t (", "), &n ('<arg><arg-N n="LDACC"><k>LDACC</k></arg-N>=' . &e ('.TRUE.') . '</arg>'));
        }

      for my $pa (&F ('.//pointer-a-stmt', $par1))
        {
          my ($E2) = &F ('./E-2/named-E', $pa);
          my ($N) = &F ('./N/n/text()', $E2);

          my $tt = $N->textContent;

          next unless ($tt =~ s/^GET_HOST_DATA_/GET_DEVICE_DATA_/o);
          $N->setData ($tt);
        }

      my ($if_block) = &Fxtran::parse (fragment => << "EOF");
IF (FXTRAN_ACDC_LPARALLELMETHOD ('OPENACC', '$name:$count')) THEN
!
ELSE
!
ENDIF
EOF

      my ($C1, $C0) = &F ('.//C', $if_block);

      $C1->replaceNode ($par1);
      $C0->replaceNode ($par0);

      $par->replaceNode ($if_block);

      $count++;
    }

}


1;
