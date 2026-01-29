package Fxtran::Vendor;

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
