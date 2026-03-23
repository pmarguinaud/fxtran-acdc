package Fxtran::Vendor::ROCM;

=head1 NAME

Fxtran::Vendor::ROCM

=head1 DESCRIPTION

Vendor adaptor for the AMD ROCm (amdflang/amdlang) Fortran compiler.
Overrides C<preprocessOptions> to apply two ROCm-specific fixups to the
compiler argument list: it honours per-file optimisation level overrides
specified via C<!amdrocm -ON> comment directives in the source, and it
deduplicates C<.a> static library arguments while ensuring that
C<-lflang_rt.hostdevice> is followed by C<-lm> exactly once.

=cut

use Data::Dumper;
use File::Basename;
use FileHandle;

use strict;

sub preprocessOptions
{

=head2 preprocessOptions

ROCm-specific compiler argument fixup.  Applies two transformations to the
argument list before returning it:

=over 4

=item 1.

If the argument list contains a C<.F90> or C<.f90> source file, the file is
scanned for C<!amdrocm -ON> comment directives.  Any C<-O> flag already
present in the argument list is replaced by the level specified in the
directive.

=item 2.

C<.a> static-library arguments are deduplicated (only the first occurrence is
kept).  The flag C<-lflang_rt.hostdevice> is emitted at most once and is
always immediately followed by C<-lm>.

=back

=cut

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
