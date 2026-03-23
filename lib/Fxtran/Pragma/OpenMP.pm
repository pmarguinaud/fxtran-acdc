package Fxtran::Pragma::OpenMP;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Pragma::OpenMP

=head1 DESCRIPTION

Pragma class for generating OpenMP directives in FORTRAN source code.
This class derives from C<Fxtran::Pragma> and currently provides one
directive-insertion method:

=over 4

=item C<parallelDo>

Inserts a C<!$OMP PARALLEL DO> directive with optional clauses (e.g.
C<PRIVATE>, C<REDUCTION>) before a given node in the fxtran XML document
tree. Long clause lists are automatically wrapped across continuation
lines.

=back

=cut

use base qw (Fxtran::Pragma);

use strict;
use Fxtran;
use List::MoreUtils qw (uniq);

sub parallelDo
{
  my ($p, %c)  = @_;

  my $P = $p->parentNode;

  my @d = (' PARALLEL DO ');

  my $N = 5;

  for my $c (sort keys (%c))
    {
      next unless (my @l = sort &uniq (@{ $c{$c} }));
      my $f = 1;
      while (my @x = splice (@l, 0, $N))
        {
          push @d, join (', ', @x); $d[-1] .= ', ' if (scalar (@l));
          if ($f)
            {
              $d[-1] = "$c (" . $d[-1];
              $f = 0;
            }
          else
            {
              $d[-1] = (' ' x (2 + length ($c))) . $d[-1];
            }
          unless (@l)
            {
              $d[-1] = $d[-1] . ") ";
            }
        }
    }


  
  for my $i (0 .. $#d)
    {
      if ($i < $#d)
        {
          $d[$i] = $d[$i] . '&amp;'
        }
      if ($i > 0)
        {
          $d[$i] = '&amp;' . $d[$i];
        }
      $d[$i] = "!\$OMP$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  for my $d (@d)
    {
      $P->insertBefore (&n ("<C>$d</C>"), $p);
      $P->insertBefore (&t ("\n"), $p);
    }


  $P->insertBefore (&t ("\n"), $p);
      
}

=head1 SEE ALSO

L<Fxtran::Pragma>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
