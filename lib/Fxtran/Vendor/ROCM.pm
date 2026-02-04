package Fxtran::Vendor::ROCM;

use Data::Dumper;
use File::Basename;
use FileHandle;

use strict;

sub preprocessOptions
{
  my $class = shift;

  if (my ($F90) = grep { m/\.(?:F90|f90)$/o } @_)
    {
       my @code = do { my $fh = 'FileHandle'->new ("<$F90"); <$fh> };
       
       for my $amdrocm (grep { s/^!amdrocm\s+//o } @code)
         { 
           chomp ($amdrocm);
           if ($amdrocm =~ m/^-O\d$/goms)
             {
               for (@_)
                 {
                   $_ = $amdrocm if (m/^-O\d+$/o);
                 }
             }
         }
    }

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
