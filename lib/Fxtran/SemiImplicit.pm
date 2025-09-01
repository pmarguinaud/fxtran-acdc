package Fxtran::SemiImplicit;

=head1 NAME

Fxtran::SemiImplicit

=head1 DESCRIPTION

This transformation relies mostly on the C<Fxtran::SingleBlock> transformation.

After applying the singleblock transformation, horizontal sections 
(marked with C<!$ACDC HORIZONTAL>) are
scanned and call to routines implementing horizontal operators are added
the C<LDACC=LDACC> optional argument.

=head1 SEE ALSO

L<Fxtran::SingleBlock>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::SingleBlock;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  &Fxtran::SingleBlock::processSingleRoutine 
    ($pu, %opts, 'suffix-singleblock' => '_SINGLEBLOCK'); 


  for my $hor (&F ('.//horizontal-section', $pu))
    {
      for my $call (&F ('.//call-stmt', $hor))
        {
          my ($argspec) = &F ('./arg-spec', $call); 
          $argspec->appendChild ($_) for (&t (','), &n ('<arg><arg-N n="LDACC"><k>LDACC</k></arg-N>=' . &e ('LDACC') . '</arg>'));
        }
    }

}

1;
