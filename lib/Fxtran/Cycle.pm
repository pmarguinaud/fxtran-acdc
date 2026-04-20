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
=cut

use strict;

sub simplify
{

=head2 simplify

Dispatch cycle-dependent simplifications to the appropriate
C<Fxtran::CycleNN> module, where C<NN> is the numeric value supplied via
the C<cycle> option.  The target module is loaded dynamically with C<use>
and its own C<simplify> class method is then called with the same
arguments.  Returns immediately if no C<cycle> option is provided.

=cut

  shift;

  my ($d, %opts) = @_;
  return unless (my $cycle = $opts{cycle});

  my $class = 'Fxtran::Cycle' . $cycle;

  eval "use $class";
  my $c = $@;

  die ($c) if ($c);

  $class->simplify ($d, %opts);
}


=head1 SEE ALSO

L<Fxtran::Cycle49>, L<Fxtran::Cycle50>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;
