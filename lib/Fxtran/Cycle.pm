package Fxtran::Cycle;

=head1 NAME

Fxtran::Cycle

=head1 DESCRIPTION

The purpose of this module is to apply simplifications
(ie set some expressions to constant values and simplify
the code accordingly), these simplifications being cycle 
dependent.

The C<Fxtran::Cycle::simplify> will invoke the appropriate
C<Fxtran::CycleNN::simplify> method, using the cycle value.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use strict;

sub simplify
{
  shift;

  my ($d, %opts) = @_;
  return unless (my $cycle = $opts{cycle});

  my $class = 'Fxtran::Cycle' . $cycle;

  eval "use $class";
  my $c = $@;

  die ($c) if ($c);

  $class->simplify ($d, %opts);
}

1;
