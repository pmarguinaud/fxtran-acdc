package Fxtran::Vendor::ROCM;

use Data::Dumper;
use File::Basename;

use strict;

sub preprocessOptions
{
  my $class = shift;

  my $seen_flang_rt_hostdevice; 

  my %lib;

  my @argv;

  for (my $i = 0; $i <= $#_; $i++)
    {
      if ($_[$i] eq '-lflang_rt.hostdevice')
        {
          push (@argv, $_[$i], '-lm') unless ($seen_flang_rt_hostdevice++);
        }
      elsif ($_[$i] =~ m/\.a$/o)
        {
          push @argv, $_[$i] unless ($lib{$_[$i]}++);
        }
      else
        {
          push @argv, $_[$i];
        }
    }

  return @argv;
}


1;
