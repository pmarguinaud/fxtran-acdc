package Style::IAL;

use base qw (Style);
use Fxtran;
use Data::Dumper;

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

sub preProcessForOpenACC
{
  my $class = shift;
  my $d = shift;
  my %args = @_;

  my @s = qw (1:YDCPG_OPTS%KLON YDCPG_BNDS%KIDIA:YDCPG_BNDS%KFDIA 1:KLON KIDIA:KFIDA YLCPG_BNDS%KIDIA:YLCPG_BNDS%KFDIA);

  my $xpath = './/a-stmt//section-subscript[' . join (' or ', map { "string(.)=\"$_\"" } @s) . ']';

  my @ss = &F ($xpath, $d);

  for my $ss (@ss)
    {   
      $ss->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>:</n></N></named-E></lower-bound></section-subscript>"));
    }   

}

sub generateInterface
{
  my $class = shift;
  my ($F90, %opts) = @_;
  &Fxtran::intfb ($F90, $opts{dir}, $class->includeExtension ());
}

1;
