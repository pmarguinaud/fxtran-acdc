package Fxtran::Vendor::NVIDIA;

=head1 NAME

Fxtran::Vendor::NVIDIA

=head1 DESCRIPTION

Vendor adaptor for the NVIDIA HPC SDK (pgf90/nvfortran) Fortran compiler.
Overrides C<preprocessOptions> to apply two fixups to the link command line
(compilation command lines, containing C<-c>, are left unchanged):

=over 4

=item 1.

If GPU offload is enabled (C<-cuda>, C<-acc>, C<-acc=gpu>, C<-mp=gpu>,
C<-stdpar=gpu>) and relocatable device code is not disabled (C<-gpu=nordc>),
the CUDA runtime libraries C<cudadevrt> and C<cudart_static> are removed
(either as C<-lcudadevrt>, C<-lcudart_static> or as full paths to
C<libcudadevrt.a>, C<libcudart_static.a>). These are added by CMake for
targets containing CUDA sources; the NVIDIA driver already adds
C<-lcudadevrt -lcudart> at the right place, and ignores C<-lcudart_static>.
Leaving C<cudadevrt> in the user part of the command line makes nvlink fail
with "unexpected object after cudadevrt" when a library with device code
(such as C<libFxtranACDC.a>) is appended afterwards.

=item 2.

If C<-cuda> is present and no C<-cudalib=> option is given, C<-cudalib=cublas>
is added.

=back

=cut

use Data::Dumper;
use File::Basename;
use FileHandle;

use strict;

sub preprocessOptions
{
  my $class = shift;

  my @argv = @_;

  goto RETURN if (grep { "$_" eq '-c' } @argv);

  # linker only : remove cuda runtime libraries added by CMake if gpu offload is on;
  # the driver adds them at the right place (cudadevrt must be the last device library for nvlink)

  my $gpu = 0;

  # CUDA Fortran
  $gpu ||= grep { "$_" eq '-cuda' } @argv;

  # OpenACC (GPU is the default target of -acc)
  $gpu ||= grep { "$_" eq '-acc' } @argv;

  # OpenACC, OpenMP, stdpar with explicit gpu target, eg -acc=gpu, -mp=gpu, -stdpar=gpu,multicore
  $gpu ||= grep { m/^-(?:acc|mp|stdpar)=(?:\S*,)?gpu(?:,\S*)?$/o } @argv;

  # No relocatable device code : there is no device link
  my $nordc = grep { m/^-gpu=(?:\S*,)?nordc(?:,\S*)?$/o } @argv;

  if ($gpu && (! $nordc))
    {
      @argv = grep { ! ("$_" =~ m/^-l(?:cudadevrt|cudart_static)$/o) } @argv;
      @argv = grep { ! ("$_" =~ m/(?:^|\/)lib(?:cudadevrt|cudart_static)\.a$/o) } @argv;
    }

  # linker only : add cuda libraries if missing and cuda is on

  goto RETURN unless (grep { "$_" eq '-cuda' } @argv);

  goto RETURN if (grep { "$_" =~ m/^-cudalib=/o } @argv);

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
