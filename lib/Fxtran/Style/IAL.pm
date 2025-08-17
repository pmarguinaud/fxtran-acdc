package Fxtran::Style::IAL;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Style);
use Data::Dumper;

use strict;

use Fxtran;
use Fxtran::Interface;

sub includeExtension
{
  return '.intfb.h';
}

sub noComputeRoutine
{
  shift;
  return 1 if ($_[0] =~ m/^(?:ABOR1|DR_HOOK|PCRC)$/o);
}

sub handleMessages
{
  shift;
  my $d = shift;
  my %opts = @_;

  &Fxtran::Print::useABOR1_ACC ($d);
  &Fxtran::Print::changeWRITEintoPRINT ($d);
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
  &Fxtran::Interface::intfb ($F90, $opts{dir}, $class->includeExtension ());
}

sub setOpenACCInterfaces
{
  shift;

  # Fxtran::Include *_openacc.intfb.h interfaces

  my ($d, %opts) = @_;

  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  my $suffix = $opts{suffix};

  my @called = &F ('.//call-stmt/procedure-designator', $ep, 1);
  
  my @include = &F ('./include', $dp);
  
  for my $include (@include)
    {
      my ($proc) = &F ('./filename', $include, 2);
      for ($proc)
        {
          s/\.intfb\.h$//o;
          s/\.h$//o;
          $_ = uc ($_);
        }

      $proc .= $suffix;
   
      if ((grep { $proc eq $_ } @called) && ! $opts{'merge-interfaces'})
        {
          next if (&F ('./include/filename[string(.)="?"]', lc ($proc) . '.intfb.h', $dp));
          my $include_openacc = &n ('<include>#include "<filename>' . lc ($proc) . '.intfb.h</filename>"</include>');
          $include->parentNode->insertAfter ($include_openacc, $include);
          $include->parentNode->insertAfter (&t ("\n"), $include);
        }
      
    }
  
}

1;
