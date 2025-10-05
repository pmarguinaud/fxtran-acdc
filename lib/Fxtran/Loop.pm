package Fxtran::Loop;

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
use Fxtran::Common;
use Fxtran;
use Fxtran::FieldAPI;
use Fxtran::Ref;
use Fxtran::Scope;


sub removeNpromaConstructs
{
  my $d = shift;
  my %opts = @_;

  my $jlon = $opts{style}->jlon ();

  my @do = &F ('.//do-construct[./do-stmt[string(do-V)="?"]]', $jlon, $d);
  
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

sub fixCOUNTIdiom
{
  my $s = shift;

  # MesoNH only (hopefully)
 
  my @E2 = &F ('.//a-stmt/E-2/named-E[string(N)="COUNT"]', $s);

  for my $E2 (@E2)
    {
      my ($T) = &F ('.//named-E[./R-LT/array-R[string(section-subscript-LT)="IIJB:IIJE"]]', $E2);
      next unless ($T);
      my ($N) = &F ('./N', $T, 1);
      $E2->replaceNode (&e ("MERGE (1, 0, $N)"));
    }

}

sub fixSUMIdiom
{
  my ($s, %opts) = @_;

  my $kidia = $opts{style}->kidia ();
  my $kfdia = $opts{style}->kfdia ();
  my $jlon  = $opts{style}->jlon ();

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

  for my $R ('function')
    {

      my $xpath =
         './/named-E[string(N)="SUM"]'
       . "[./R-LT/$R-R/element-LT/element/named-E/R-LT/array-R/section-subscript-LT"
       . "/section-subscript[string(.)=\"$kidia:$kfdia\"]]";

      my @sum = &F ($xpath, $s);

# So we need to replace the SUM by a scalar expression

      for my $sum (@sum)
        {
          my ($expr) = &F ("./R-LT/$R-R/element-LT/element/named-E", $sum);
          my ($sslt) = &F ('./R-LT/array-R/section-subscript-LT', $expr);
          my @ss = &F ('./section-subscript', $sslt);
          $ss[0]->replaceNode (&n ('<section-subscript><lower-bound>' . &e ($jlon) . '</lower-bound></section-subscript>'));
          $sum->replaceNode ($expr);
        }
    }
}


sub removeNpromaLoopsInSection
{
  my $s = shift;
  my %opts = @_;

  my $var2dim = $opts{var2dim};

  &fixSUMIdiom ($s, %opts);
  &fixCOUNTIdiom ($s, %opts);
 
  &removeNpromaConstructs ($s, %opts);
  
  for my $var (sort keys (%$var2dim))
    {
      my $nd = $var2dim->{$var};
      my @expr = &F ('.//named-E[string(N)="?"][not(ancestor::call-stmt)]', $var, $s);
      for my $expr (@expr)
        {
          &setJlon ($expr, $nd, %opts);
        }
    }


}

sub getVarToDim
{
  my $pu = shift;
  my %opts = @_;

  my $wantarray = wantarray (); # Return var2dim & var2pos

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  my $style = $opts{style};

  my @klon  = $style->nproma ();

  # NPROMA variables

  my @en_decl;

  for my $klon (@klon)
    {
      if ($klon =~ m/:/o)
        {
          push @en_decl, &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(.)="?"]]', $klon, $dp);
        }
      else
        {
          push @en_decl, &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="?"]]', $klon, $dp);
        }
    }

  my (%var2dim, %var2pos);

  for my $en_decl (@en_decl)
    {
      my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl, 1);
      my ($N) = &F ('./EN-N', $en_decl, 1);
      $var2dim{$N} = scalar (@ss);

      next unless ($wantarray);

      for my $i (0 .. $#ss)
        {
          my $ss = $ss[$i];
          for my $klon (@klon)
            {
              if ($ss eq $klon)
                {
                  $var2pos{$N} = $i;
                  goto DONE;
                }
            }
        }
DONE:
      next;
    }

  my %N1toN2;

  for my $ptrastmt (&F ('.//pointer-a-stmt', $ep))
    {
      my ($N1) = &F ('./E-1/named-E/N', $ptrastmt, 1);
      my ($N2) = &F ('./E-2/named-E/N', $ptrastmt, 1);

      if ($N1toN2{$N1} && (! $var2dim{$N2}))
        {
          die ("Pointer $N1 is ambiguous; it points to the field variable `$N1toN2{$N1}' and to the non-field variable `$N2'");
        }
      elsif ($var2dim{$N2})
        {
          $N1toN2{$N1} = $N2;
        }
    }

   for my $N1 (sort keys (%N1toN2))
     {
       my $N2 = $N1toN2{$N1};
       $var2dim{$N1} = $var2dim{$N2};
     }

  if ($wantarray)
    {
      return (\%var2dim, \%var2pos);
    }
  else
    {
      return \%var2dim;
    }
}

sub removeNpromaLoops
{
  my $pu = shift;
  my %opts = @_;
  
  my $style = $opts{style};

  my $kidia = $style->kidia ();
  my $jlon  = $style->jlon ();

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  unless (&F ('./T-decl-stmt[.//EN-decl[string(EN-N)="?"]]', $jlon, $dp))
    {
      my $decl = $opts{style}->declareJlon ();
      $ep->appendChild ($_) for (&t ("\n", $decl));
    }

  &Fxtran::Decl::declare ($pu, $style->declareJlon ());

  $ep->insertBefore ($_, $ep->firstChild) for (&t ("\n"), &t ("\n"), &s ("$jlon = $kidia"), &t ("\n"));

  my $var2dim = $opts{var2dim} || &getVarToDim ($pu, %opts);

  &removeNpromaLoopsInSection
  (
    $ep,
    var2dim => $var2dim,
    %opts,
  );
}

sub removeNpromaLoopsFieldAPI
{
  my ($d, $s, %opts) = @_;

  my $TI = &Fxtran::FieldAPI::getTypeInfo ();
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
      my $fd = &Fxtran::FieldAPI::isFieldAPIMember ($T{$N}, @ct);
      my $nd = $fd->[1] + 1;

      &setJlon ($ptr, $nd, %opts);
    }

}
 
sub setJlon
{
  my ($expr, $nd, %opts) = @_;

  my $jlon = $opts{style}->jlon ();

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
  
  # Do not change anything in pointer assignment statements
  if (my $p = $expr->parentNode)
    {
      if (($p->nodeName =~ m/^E-[12]$/o) && (my $q = $p->parentNode))
        {
          return if ($q->nodeName eq 'pointer-a-stmt');
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
      &Fxtran::Ref::parensToArrayRef ($ar);
    }

  my @ss = &F ('./section-subscript-LT/section-subscript', $ar);

  my $pos = 0;

  if (my $var2pos = $opts{var2pos})
    {
      my ($N) = &F ('./N', $expr, 1);
      $pos = $var2pos->{$N};
    }

  $ss[$pos]->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>$jlon</n></N></named-E></lower-bound></section-subscript>"));

}

1;
