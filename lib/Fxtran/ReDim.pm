package Fxtran::ReDim;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::ReDim

=head1 DESCRIPTION

Removes the NPROMA (horizontal) dimension from array declarations and
references when converting to single-column layout.  For every variable whose
first dimension matches NPROMA, the module strips that dimension from the
declaration and from every array subscript in the code.  A separate helper
replaces bare C<:> section subscripts with a scalar JLON index in actual
subroutine arguments that are passed as JBLK-blocked arrays.

=head1 FUNCTIONS

=cut

use strict;
use FileHandle;
use Data::Dumper;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

sub reDim
{

=head2 reDim

Removes the NPROMA (first) dimension from array declarations and all
corresponding array subscripts in the parse tree.  When the
C<redim-arguments> option is set, also replaces bare C<:> first subscripts
in actual subroutine arguments with the scalar JLON index.  Variables whose
remaining dimensions are not simple literals are left unchanged.

=cut

  my $d = shift;
  my %args = @_;

  my $jlon = $args{style}->jlon ();
  my @klon = $args{style}->nproma ();

  my @en_decl = 
    (map { my $n = $_; &F ('.//EN-decl[./array-spec/shape-spec-LT[string(shape-spec)="?"]]', $n, $d) } @klon);

  EN_DECL : for my $en_decl (@en_decl)
    {
      my ($N) = &F ('./EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);

      unless ($args{'redim-arguments'}) # Skip arguments
        {
          next if (&F ('.//attribute-N[string(.)="INTENT"]', $stmt));
          next if (&F ('.//call-stmt[.//named-E[string(N)="?"]', $N, $d));
        }

      if ($args{'redim-arguments'}) # Change : to JLON for actual arguments where actual dimension is 1 (ie we have a single ':')
        {
          my @ss = &F ('.//arg/named-E[string(N)="?"]/R-LT/array-R/section-subscript-LT'     # Subroutine argument
                     . '[count(./section-subscript/text()[string(.)=":"])=1]'                # Single ':'
                     . '[./section-subscript[1]/text()[string(.)=":"]]'                      # First subscript is :
                     . '/section-subscript[1]/text()'                                        # Get ':'
                     , $N, $d);

          for my $ss (@ss)
            {
              $ss->replaceNode (&n ("<lower-bound><named-E><N><n>$jlon</n></N></named-E></lower-bound>"));
            }

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

=head2 redimArguments

Replaces the bare C<:> first subscript with the scalar JLON index in actual
subroutine arguments that are arrays whose last subscript is JBLK, targeting
the JBLK-blocked calling convention used in the outer parallel loop.

=cut

  my $d = shift;
  my %args = @_;

  my $jlon = $args{style}->jlon ();

  my @ss = &F ('.//arg/named-E/R-LT/array-R/section-subscript-LT'     # Subroutine argument
             . '[count(./section-subscript/text()[string(.)=":"])=1]' # Single ':'
             . '[./section-subscript[1]/text()[string(.)=":"]]'       # First subscript is :
             . '[./section-subscript[last()][string(.)="JBLK"]]'      # Last subscript is JBLK
             . '/section-subscript[1]/text()'                         # Get ':'
             , $d);

  for my $ss (@ss)
    {
      $ss->replaceNode (&n ("<lower-bound><named-E><N><n>$jlon</n></N></named-E></lower-bound>"));
    }
}

1;
