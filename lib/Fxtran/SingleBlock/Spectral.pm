package Fxtran::SingleBlock::Spectral;

=head1 NAME

Fxtran::SingleBlock::Spectral

=head1 DESCRIPTION

The purpose of this module is to apply the singleblock transform to spectral (horizontal diffusion mainly)
routines.

We rely on the C<Fxtran::SingleBlock> method and inherit from this class. 

The differences with C<Fxtran::SingleBlock> are:

=over 4

=item

Most arrays are dimensioned with C<NFLEVL> and C<NSPEC2>, or C<NSPEC2> alone for C<PSPSP> (surface pressuse):

  REAL :: PSPDIV (NFLEVG, NSPEC2)
  REAL :: PSPSP (NSPEC2)

=item

C<NPROMA> is C<NSPEC> (looks weird, but there is no other alternative).

=item

We overload the C<makeParallel> method, so that loops over levels and wavenumbers be collapsed into a single
loop when possible.

=back

=head1 AUTHOR

philippe.marguinaud@meteo.fr
=cut

use Data::Dumper;

use strict;

use base qw (Fxtran::SingleBlock);

use Fxtran;

sub makeParallel
{

=head2 makeParallel

Overloaded parallelisation method for spectral routines.  Given a program
unit and a candidate C<DO> construct, this method first checks whether all
arrays that are written inside the loop are updated within a C<JLEV> loop
over levels.  If some are not, only a simple gang/vector loop over the
spectral wavenumber is inserted.  Otherwise all existing C<JLEV> loops
inside the construct are hoisted out, a single enclosing C<JLEV> loop is
wrapped around the contents, and a collapsed (level x wavenumber) parallel
loop is generated.

=cut

  my $class = shift;
  my ($pu, $do, %opts) = @_;

  my $pragma = $opts{pragma};
  my $present = $opts{present} || [];
  my $private = $opts{private} || [];
  my $var2dim = $opts{var2dim};
  my $var2pos = $opts{var2pos};


  # Check all involved variables are updated inside a loop over levels

  my $jlev = 1;

  for my $var (sort keys (%$var2dim))
    {
      for my $expr (&F ('.//a-stmt/E-1/named-E[string(N)="?"]', $var, $do))
        {
          my ($do_jlev) = &F ('ancestor::do-construct/do-stmt'  
                            . '[string(./do-V)="JLEV"]'                 
                            . '[string(./lower-bound)="1"]'
                            . '[string(./upper-bound)="YDGEOMETRY%YRDIMV%NFLEVL"]', 
                              $expr);
          $jlev = 0 unless ($do_jlev);
        }
    }

  unless ($jlev) # Some variables are updated without the loop over levels : parallelize only over the spectral wavenumber
    {
      $pragma->insertParallelLoopGangVector ($do, PRESENT => $present, PRIVATE => $private, IF => ['LDACC']);
      return;
    }

  # Remove all loops over levels ...

  for my $do_jlev (&F ('.//do-construct[./do-stmt[string(./do-V)="JLEV"]]', $do))
    {
      my @n = &F ('./node()', $do_jlev); pop (@n) for (1 .. 2); shift (@n) for (1 .. 2);

      my $p = $do_jlev->parentNode;

      for my $n (@n)
        {
          $p->insertBefore ($n, $do_jlev);
        }

      $do_jlev->unbindNode ();
    }

  # ... and replace them by a single loop over levels 

  my ($do_jlev) = &Fxtran::parse (fragment => << "EOF");
DO JLEV = 1, YDGEOMETRY%YRDIMV%NFLEVL
ENDDO
EOF

  my @n = &F ('./node()', $do); pop (@n) for (1 .. 2); shift (@n) for (1 .. 2);

  for my $n (@n)
    {
      $do_jlev->insertBefore ($n, $do_jlev->lastChild);
    }
  
  $do->insertBefore ($_, $do->lastChild) for ($do_jlev, &t ("\n"));

  # Parallelize over wavenumbers and levels

  $pragma->insertParallelLoopGangVector ($do, PRESENT => $present, PRIVATE => $private, IF => ['LDACC'], COLLAPSE => [2]);
}


=head1 SEE ALSO

L<Fxtran::SingleBlock>

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;
