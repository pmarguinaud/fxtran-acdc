package Fxtran::Pragma;

=head1 NAME

Fxtran::Pragma

=head1 SYNOPSIS

  use Fxtran::Pragma;

  my $pragma = 'Fxtran::Pragma'->new (pragma => 'OpenMP');

  # $do is a do construct

  $pragma->parallelDo ($do, PRIVATE => ['I', 'J']);

=head1 DESCRIPTION

This module implements a base class for pragma annotations
languages such as C<OpenMP> or C<OpenACC>. Objects
of class C<Fxtran::Pragma> have methods to generate such
annotations.

The C<new> constructor also serves as an entry point for
creating such objects.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use strict;

sub new
{
  my $class = shift;
  my %args = @_;

  if ($args{pragma})
    {
      my $class = "Fxtran::Pragma::$args{pragma}";
      eval "use $class";
      $@ && die ($@);
      return $class->new ();
    }
  elsif ($class ne __PACKAGE__)
    {
      return bless \%args, $class;
    }
}

1;
