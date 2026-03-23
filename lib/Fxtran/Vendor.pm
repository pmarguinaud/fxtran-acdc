package Fxtran::Vendor;

=head1 NAME

Fxtran::Vendor

=head1 DESCRIPTION

Factory class for vendor-specific compiler adaptors.  The constructor inspects
the compiler executable name and returns an appropriate subclass instance; for
AMD ROCm compilers (amdlang/amdflang) it returns a C<Fxtran::Vendor::ROCM>
object.  The base class C<preprocessOptions> method is a pass-through that
returns its arguments unchanged.

=head1 FUNCTIONS

=cut

use File::Basename;

use strict;

use Fxtran::Vendor::ROCM;

sub new
{
  my $class = shift;

  my $compiler = shift;

  if (&basename ($compiler) =~ m/^amd.*lang/o)
    {
      $class = 'Fxtran::Vendor::ROCM';
    }

  return bless {}, $class;
}

sub preprocessOptions
{
  shift;
  return @_;
}

1;
