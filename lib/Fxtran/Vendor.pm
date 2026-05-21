package Fxtran::Vendor;

=head1 NAME

Fxtran::Vendor

=head1 DESCRIPTION

Factory class for vendor-specific compiler adaptors.  The constructor inspects
the compiler executable name and returns an appropriate subclass instance; for
AMD ROCm compilers (amdlang/amdflang) it returns a C<Fxtran::Vendor::ROCM>
object.  The base class C<preprocessOptions> method is a pass-through that
returns its arguments unchanged.

=cut

use File::Basename;

use strict;

use Fxtran::Vendor::ROCM;
use Fxtran::Vendor::NVIDIA;

sub new
{

=head2 new

Constructor.  Takes a compiler executable path as its sole argument.  If the
basename of the executable matches C<amd.*lang> (i.e. amdlang or amdflang),
returns a blessed C<Fxtran::Vendor::ROCM> object; otherwise returns a
plain C<Fxtran::Vendor> object.

=cut

  my $class = shift;

  my $compiler = shift;

  $compiler = &basename ($compiler);

  if ($compiler =~ m/^amd.*lang/o)
    {
      $class = 'Fxtran::Vendor::ROCM';
    }
  elsif ($compiler =~ m/^(?:pgf90|nvfortran)$/o)
    {
      $class = 'Fxtran::Vendor::NVIDIA';
    }

  return bless {}, $class;
}

sub preprocessOptions
{

=head2 preprocessOptions

Base-class pass-through.  Returns its argument list unchanged.  Subclasses
override this method to apply vendor-specific transformations to the compiler
argument list before it is passed to the preprocessor invocation.

=cut

  shift;
  return @_;
}

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
