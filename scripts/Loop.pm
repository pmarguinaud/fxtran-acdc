package Loop;
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use FieldAPI;
use Ref;


sub removeJlonLoops
{
  my $d = shift;
  
  my ($noexec) = do  
  {
    my ($exec) = grep { &Fxtran::stmt_is_executable ($_) } &F ('.//ANY-stmt', $d);
    my @prev = &F ('preceding::*', $exec);
  
    my $prev;
    for my $p (reverse (@prev))
      {   
        next if ($p->nodeName eq '#text');
        next if ($p->nodeName eq 'C');
        $prev = $p; 
        last;
      }   
  
    my @anc = &F ('ancestor::*', $prev);
  
    for my $anc (reverse (@anc))
      {   
        if (($anc->nodeName =~ m/-(?:construct|stmt)$/o) || ($anc->nodeName eq 'include'))
          {
            $prev = $anc;
          }
      }   
  
    $prev
  };  
  
  
  my ($YDCPG_BNDS) = &F ('./object/file/program-unit/subroutine-stmt//arg[string(arg-N)="YDCPG_BNDS"]', $d);

  $noexec->parentNode->insertAfter ($YDCPG_BNDS ? &s ("JLON = YDCPG_BNDS%KIDIA") : &s ("JLON = KIDIA"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  
  
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
  
  # NPROMA variables

  my @en_decl = (&F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="YDCPG_OPTS%KLON"]]', $d),
                 &F ('.//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(upper-bound)="KLON"]]', $d));

  my %NPROMA;

  for my $en_decl (@en_decl)
    {
      my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
      my ($N) = &F ('./EN-N', $en_decl, 1);
      $NPROMA{$N} = scalar (@ss);
    }

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


  my @ptr = &F ('.//named-E[./R-LT/component-R[string(ct)="PTR"]][not(ancestor::call-stmt)]', $d);
  
  for my $ptr (@ptr)
    {
      my ($N) = &F ('./N', $ptr, 1);
      my @ct = &F ('./R-LT/component-R/ct', $ptr, 1); pop (@ct);
      my $fd = &FieldAPI::isFieldAPIMember ($T{$N}, @ct);
      my $nd = $fd->[1] + 1;

      &setJLON ($ptr, $nd);
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

sub setJLON
{
  my ($expr, $nd) = @_;

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
  $ss[0]->replaceNode (&n ('<section-subscript><named-E><N><n>JLON</n></N></named-E></section-subscript>'));
}

1;
