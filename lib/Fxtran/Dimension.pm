package Fxtran::Dimension;

=head1 NAME

Fxtran::Dimension

=head1 DESCRIPTION

This module provides functions for manipulating array dimensions.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Ref;

sub attachArraySpecToEntity
{

=head2 attachArraySpecToEntity

Transform statements such as:

  REAL, DIMENSION (N) :: X, Y

into:

  REAL :: X (N), Y (N)

=cut

  my $d = shift;

  # Remove dimension attributes and attach the array spec to entities

  my @decl = &F ('.//T-decl-stmt[./attribute[string(attribute-N)="DIMENSION"]]', $d);

  for my $decl (@decl)
    {
      my ($dim) = &F ('./attribute[string(attribute-N)="DIMENSION"]', $decl);
      my ($as) = &F ('./array-spec', $dim);
      my @en_decl = &F ('./EN-decl-LT/EN-decl', $decl);
      for my $en_decl (@en_decl)
        {
          next if (&F ('./array-spec', $en_decl));
          my ($N) = &F ('./EN-N', $en_decl);
          $en_decl->insertAfter ($as->cloneNode (1), $N);
        }
      $dim->previousSibling->unbindNode ();
      $dim->unbindNode ();
    }
}

sub fuseOuterDimensions
{

=head2 fuseOuterDimensions

This routine takes as argument a program unit, and a hash whose keys are the
symbols to process and the values the number of dimensions to fuse.

For instance:

  {
    Z => 2,
  }

  REAL :: Z (NPROMA, NFLEVG, NDIM)
  
  INTEGER :: JLON, JLEV, JDIM

  DO JDIM = 1, NDIM
    DO JLEV = 1, NFLEVG
      DO JLON = 1, NPROMA
        Z (JLON, JLEV, JDIM) = ...
      ENDDO
    ENDDO
  ENDDO

will yield:

  REAL :: Z (NPROMA, NFLEVG*NDIM)

  DO JDIM = 1, NDIM
    DO JLEV = 1, NFLEVG
      DO JLON = 1, NPROMA
        Z (JLON, 1 + (JLEV-1) + NFLEVG* (JDIM-1)) = ...
      ENDDO
    ENDDO
  ENDDO

=cut

  my $pu = shift;
  my %opts = @_;

  my ($ep) = &F ('./execution-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);

  my %nd = %{ $opts{'fuse-outer-dimension-names' } };
  
  my (%EU, %EL);
  
  for my $en_decl (&F ('.//EN-decl', $dp))
    {
      my ($n) = &F ('./EN-N', $en_decl, 1);
      next unless (my $nd = $nd{$n});
    
      my ($sslt) = &F ('./array-spec/shape-spec-LT', $en_decl);
      
      my @ss = &F ('./shape-spec', $sslt); @ss = splice (@ss, -$nd, +$nd); # Last nd
  
      for my $i (1 .. $nd-1)
       {
         for ($ss[$i]->previousSibling, $ss[$i])
           {
             $_->unbindNode ();
           }
       }
      
      my @u = my @EU = map { &F ('./upper-bound/ANY-E', $_) } @ss;
      @EU = map { $_->nodeName eq 'op-E' ? '(' . $_->textContent . ')' : $_->textContent } @EU;
  
      my @EL = (1) x scalar (@EU);
  
      $u[0]->replaceNode (&e (join ('*', @EU)));
  
      $EU{$n} = \@EU;
      $EL{$n} = \@EL;
    }
      
  for my $expr (&F ('.//named-E', $ep))
    {
      my ($n) = &F ('./N', $expr, 1);
  
      next unless (my $nd = $nd{$n});
  
      my @EU = @{ $EU{$n} };
      my @EL = @{ $EL{$n} };
  
      if (my ($p) = &F ('./R-LT/parens-R', $expr))
        {
          &Fxtran::Ref::parensToArrayRef ($p);
        }
  
      next unless (my ($ar) = &F ('./R-LT/array-R', $expr));
   
      my @ss = &F ('./section-subscript-LT/section-subscript', $ar); @ss = splice (@ss, -$nd, +$nd); # Last nd
  
      for my $i (1 .. $nd-1)
        {
          for ($ss[$i]->previousSibling, $ss[$i])
            {
              $_->unbindNode ();
            }
        }
  
      my @l = map { my ($x) = &F ('./lower-bound/ANY-E', $_)    ; $x } @ss;
      my @u = map { my ($x) = &F ('./upper-bound/ANY-E', $_)    ; $x } @ss;
      my @d = map { my ($x) = &F ('./text()[string(.)=":"]', $_, 1); $x } @ss;
  
      for (@u, @l)
        {
          next unless (defined ($_));
          if ($_->nodeName eq 'op-E')
            {
              $_ = '(' . $_->textContent . ')';
            }
          else
            {
              $_ = $_->textContent;
            }
        }
  
      for my $i (0 .. $nd-1)
        {
          if (defined ($d[$i]))
            {
              $l[$i] = $EL[$i] unless (defined ($l[$i]));
              $u[$i] = $EU[$i] unless (defined ($u[$i]));
            }
          else
            {
              $u[$i] = $l[$i];
            }
        }
      
      my $s = 0;
  
      for my $i (0 .. $nd-1)
        {
           if ($s)
             {
               die if ($l[$i] ne $u[$i]);
             }
  
           $s++ if (($l[$i] ne $EL[$i]) || ($u[$i] ne $EU[$i]));
        }
  
      my ($ll, $uu) = ($l[$nd-1], $u[$nd-1]);
  
      for (my $i = $nd-2; $i >= 0; $i--)
        {
          $ll = "$l[$i]+$EU[$i]*($ll-1)";
          $uu = "$u[$i]+$EU[$i]*($uu-1)";
        }
  
      if ($d[1] || $d[0])
        {
          $ss[0]->replaceNode (&n ('<section-subscript><lower-bound>' . &e ($ll) . '</lower-bound>' . 
                                                     ':<upper-bound>' . &e ($uu) . '</upper-bound></section-subscript>'));
        }
      else
        {
          $ss[0]->replaceNode (&n ('<section-subscript><lower-bound>' . &e ($ll) . '</lower-bound></section-subscript>'));
        }
    }
  
}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

1;
