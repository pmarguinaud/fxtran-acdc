package Fxtran::Vendor::NVIDIA;

=head1 NAME

Fxtran::Vendor::NVIDIA

=cut

use Data::Dumper;
use File::Basename;
use FileHandle;

use strict;

sub preprocessOptions
{
  my $class = shift;

  my @argv = @_;

  goto RETURN if (grep { $_ eq '-c' } @argv);

  # linker only : add cuda libraries if missing and cuda is on

  goto RETURN unless (grep { $_ eq '-cuda' } @argv);

  goto RETURN if (grep { m/^-cudalib=/o } @argv);

  push @argv, '-cudalib=cublas';

RETURN:

  return @argv;
}


=head1 SEE ALSO

L<Fxtran::Vendor>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2026

=cut

1;
