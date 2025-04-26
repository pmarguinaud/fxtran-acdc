package Style::IAL;

use base qw (Style);
use Fxtran;

use strict;

sub includeExtension
{
  return '.intfb.h';
}

sub noComputeRoutine
{
  shift;
  return 1 if ($_[0] =~ m/^(?:ABOR1|DR_HOOK)$/o);
}

sub handleMessages
{
  shift;
  my $d = shift;
  my %opts = @_;

  &Print::useABOR1_ACC ($d);
  &Print::changeWRITEintoPRINT ($d);
}

sub dim2ind
{
  shift;
  my $dim = shift;

  my %dim2ind = 
  (
    'KLON'   => 'JLON',
    'KLEV'   => 'JLEV',
  );

  return $dim2ind{$dim};
}

sub dim2bnd
{
  shift;
  my $dim = shift;

  my %dim2bnd =
  (
    'KLON'   => [qw (KIDIA KFDIA)],
  );

  return @{ $dim2bnd{$dim} || [] };
}

1;
