package Loop;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use FieldAPI;
use Ref;
use Scope;


sub removeJlonConstructs
{
  my $d = shift;

  my @do = &F ('.//do-construct[./do-stmt[string(do-V)="JLON"]]', $d);
  
  for my $do (@do)
    {
      $do->firstChild->unbindNode;
      $do->lastChild->unbindNode;
      my @nodes = &F ('./node()', $do);
      for (@nodes)
        {
          $do->parentNode->insertBefore ($_, $do);
        }
      $do->unbindNode ();
    }
}


sub fixSUMIdiom
{
  my $d = shift;

# The following is used sometimes (eg acbl89.F90)
#
# ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))
#
# DO JJLEV=JLEV,KTDIAN+1,-1
#   IF (ZTESTM > 0.0_JPRB) THEN
#     DO JLON=KIDIA,KFDIA
#       ZDLUP1   = ZGDZF(JLON,JJLEV)
#       ZZTHVL   =(ZTHETA (JLON,JJLEV)+ZTHETA (JLON,JJLEV-1))/2.0_JPRB
#       ...
#       ZEN   (JLON)=ZEN  (JLON)-ZINCR*ZTEST0
#     ENDDO
#     ZTESTM=SUM(ZTESTSAVE(KIDIA:KFDIA))
#   ENDIF
# ENDDO

# my @sum = &F ('//E-2/named-E[string(N)="SUM"]', $d);

  for my $R ('parens', 'function')
    {
      my @sum = 
      (
        &F ('//E-2/named-E[string(N)="SUM"]'
       . "[./R-LT/$R-R/element-LT/element/named-E/R-LT/array-R/section-subscript-LT"
       . '/section-subscript[string(.)="KIDIA:KFDIA"]]', $d),
      );

# So we need to replace the SUM by a scalar assignment

      for my $sum (@sum)
        {
          my ($N) = &F ("./R-LT/$R-R/element-LT/element/named-E/N", $sum, 1);
          my $N_JLON = &e ("$N(JLON)");
          $sum->replaceNode ($N_JLON->cloneNode (1));
        }
    }
}


sub removeJlonLoops
{
  my $d = shift;
  my %opts = @_;

  my @KLON = qw (YDCPG_OPTS%KLON KLON);
  my $KIDIA = 'KIDIA';

  @KLON = @{ $opts{KLON} } if ($opts{KLON} );
  $KIDIA = $opts{KIDIA} if ($opts{KIDIA});

  my $noexec = &Scope::getNoExec ($d);

  &fixSUMIdiom ($d);
 
  unless (&F ('.//T-decl-stmt[.//EN-decl[string(EN-N)="JLON"]]', $d))
    {
      my $indent = "\n" . (' ' x &Fxtran::getIndent ($noexec));
      $noexec->parentNode->insertAfter (my $decl = &s ("INTEGER (KIND=JPIM) :: JLON"), $noexec);
      $noexec->parentNode->insertAfter (&t ($indent), $noexec);
      $noexec = $decl;
    }


  my ($YDCPG_BNDS) = &F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDCPG_BNDS"]', $d);

  $noexec->parentNode->insertAfter ($YDCPG_BNDS ? &s ("JLON = YDCPG_BNDS%KIDIA") : &s ("JLON = $KIDIA"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  
  &removeJlonConstructs ($d);
  
  # NPROMA variables

  my @en_decl;

  for my $KLON (@KLON)
    {
      push @en_decl, &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="?"]]', $KLON, $d);
    }

  my %NPROMA;

  for my $en_decl (@en_decl)
    {
      my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
      my ($N) = &F ('./EN-N', $en_decl, 1);
      $NPROMA{$N} = scalar (@ss);
    }

  if ($opts{fieldAPI})
    {
      &removeJlonLoopsFieldAPI ($d, $d);
    }


  for my $NPROMA (sort keys (%NPROMA))
    {
      my $nd = $NPROMA{$NPROMA};
      my @expr = &F ('.//named-E[string(N)="?"][not(ancestor::call-stmt)]', $NPROMA, $d);
      for my $expr (@expr)
        {
          &setJLON ($expr, $nd);
        }
    }

}

sub removeJlonLoopsFieldAPI
{
  my ($d, $s) = @_;
  my %args = @_;

  my $TI = &FieldAPI::getTypeInfo ();
  my @T = sort keys (%$TI);

  # Find objects containing field API pointers and map them to their types

  my %T;
  for my $T (@T)
    {
      my @F = &F ('.//T-decl-stmt[./_T-spec_/derived-T-spec[string(T-N)="?"]]//EN-decl/EN-N/N/n/text()', $T, $d, 1);
      for my $F (@F)
        {
          $T{$F} = $T;
        }
    }

  my @F = sort keys (%T);

  my @ptr = &F ('.//named-E[./R-LT/component-R[string(ct)="DEVPTR" or string(ct)="PTR"]][not(ancestor::call-stmt)]', $s);
  
  for my $ptr (@ptr)
    {
      my ($N) = &F ('./N', $ptr, 1);
      my @ct = &F ('./R-LT/component-R/ct', $ptr, 1); pop (@ct);
      my $fd = &FieldAPI::isFieldAPIMember ($T{$N}, @ct);
      my $nd = $fd->[1] + 1;

      &setJLON ($ptr, $nd);
    }

}

sub setJLON
{
  my ($expr, $nd) = @_;

  if (my $p = &Fxtran::expr ($expr))
    {
      if ($p->nodeName eq 'named-E')
        {
          my ($n) = &F ('./N', $p, 1);
          if ($n eq 'PRESENT') # Do not force JLON indexing for PRESENT (X)
            {
              return;
            }
        }
    }

  my ($rlt) = &F ('./R-LT', $expr);

  unless ($rlt)
    {
      $expr->appendChild ($rlt = &n ('<R-LT/>'));
    }

  my @r = &F ('./ANY-R', $rlt);

  my $ar;

  if (@r && ($r[-1]->nodeName =~ m/^(?:array-R|parens-R)$/o))
    {
      $ar = $r[-1];
    }
  else
    {
      # Add array reference
      $ar = &n ('<array-R>(<section-subscript-LT>' . join (',', ('<section-subscript>:</section-subscript>') x $nd) . '</section-subscript-LT>)</array-R>');
      @r ? $rlt->insertAfter ($ar, $r[-1]) : $rlt->appendChild ($ar); 
    }
  

  # Force first dimension to JLON

  if ($ar->nodeName eq 'parens-R')
    {
      &Ref::parensToArrayRef ($ar);
    }

  my @ss = &F ('./section-subscript-LT/section-subscript', $ar);
  $ss[0]->replaceNode (&n ('<section-subscript><lower-bound><named-E><N><n>JLON</n></N></named-E></lower-bound></section-subscript>'));

}

1;
