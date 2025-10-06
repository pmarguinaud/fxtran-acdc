package Fxtran::TopLevel::SemiImplicit;

=head1 NAME

Fxtran::TopLevel::SemiImplicit

=head1 DESCRIPTION

Process high level routines of semi-implicit calculations. 

=head1 SEE ALSO

L<Fxtran::TopLevel::Spectral>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;
use FileHandle;

use strict;

use base qw (Fxtran::TopLevel::Spectral);

use Fxtran::Decl;
use Fxtran;

sub processSingleRoutine
{
  __PACKAGE__->processSingleRoutineMethod (@_);
}

sub renameProc
{
  my $class = shift;
  my ($pu, $proc, %opts) = @_;

  next unless ((my $tt = $proc->textContent) =~ m/^SP\w+SI$/o);
  $proc->setData ($tt . $opts{'suffix-semiimplicit'});

  return 1;
}

1;
