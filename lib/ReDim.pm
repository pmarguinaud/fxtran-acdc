package ReDim;

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

sub reDim
{
  my $d = shift;
  my %args = @_;

  my @KLON = @{ $args{KLON} || ['KLON'] };
  
  my @en_decl = 
    (map { my $n = $_; &F ('.//EN-decl[./array-spec/shape-spec-LT[string(shape-spec)="?"]]', $n, $d) } @KLON);
  
  EN_DECL : for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);

      unless ($args{'redim-arguments'})
        {
          next if (&F ('.//attribute-N[string(.)="INTENT"]', $stmt));
          next if (&F ('.//call-stmt[.//named-E[string(N)="?"]', $N, $d));
        }
  

      my ($as) = &F ('./array-spec', $en_decl);

      my @ss = &F ('./shape-spec-LT/shape-spec', $as);

      for my $ss (@ss[1..$#ss])
        {
          my ($lb) = &F ('./lower-bound/*', $ss);
          my ($ub) = &F ('./upper-bound/*', $ss);
          for ($lb, $ub)
            {
              next unless ($_);
              next EN_DECL unless ($_->nodeName eq 'literal-E');
            }
        }

      if (@ss > 1)
        {
          $ss[0]->nextSibling->unbindNode ();
          $ss[0]->unbindNode ();  
        }
      else
        {
          $as->unbindNode ();
        }

      my @rlt = &F ('.//named-E[string(N)="?"]/R-LT', $N, $d);
  
      for my $rlt (@rlt)
        {
          my ($r) = &F ('./ANY-R[1]', $rlt);

          if ($r->nodeName eq 'parens-R')
            {
              my @e = &F ('./element-LT/element', $r);
              if (scalar (@e) > 1)
                {
                  $e[0]->nextSibling->unbindNode ();
                  $e[0]->unbindNode ();
                }
              else
                {
                  $rlt->unbindNode ();
                }
            }
          elsif ($r->nodeName eq 'array-R')
            {
              my @ss = &F ('./section-subscript-LT/section-subscript', $r);
              if (scalar (@ss) > 1)
                {
                  $ss[0]->nextSibling->unbindNode (); 
                  $ss[0]->unbindNode ();
                }
              else
                {
                  $rlt->unbindNode ();
                }
            }
          else
            {
              die &Dumper ([$r->toString, &Fxtran::expr ($r)->textContent]);
            }
        }
  
    }
}

sub redimArguments
{
  my $d = shift;

  my @ss = &F ('.//named-E/R-LT/array-R/section-subscript-LT[count(./section-subscript)=2]' # Two subscripts
             . '[./section-subscript[string(.)="JBLK"]]'                                    # Second one is JBLK
             . '/section-subscript[string(.)=":"]/text()'                                   # First is :
             , $d);

  for my $ss (@ss)
    {
      $ss->replaceNode (&n ('<lower-bound><named-E><N><n>JLON</n></N></named-E></lower-bound>'));
    }
}

1;
