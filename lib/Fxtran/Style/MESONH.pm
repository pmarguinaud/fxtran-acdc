package Fxtran::Style::MESONH;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use List::MoreUtils qw (uniq);
use Data::Dumper;

use strict;

use base qw (Fxtran::Style);

use Fxtran::Include;
use Fxtran;
use Fxtran::Print;
use Fxtran::Identifier;

sub nproma
{
  return qw (D%NIT D%NIJT);
}

sub kidia
{
  return 'D%NIB';
}

sub kfdia
{
  return 'D%NIE';
}

sub jlon
{
  return 'JI';
}

sub jlev
{
  return 'JK';
}

sub declareJlon
{
  return &s ("INTEGER :: JI");
}

sub includeExtension
{
  return '.h';
}

sub removeUnusedIncludes
{
  return 1;
}

sub noComputeRoutine
{
  shift;
  return 1 if ($_[0] =~ m/^(?:PRINT_MSG|DR_HOOK)$/o);
}

sub preProcessForOpenACC
{
  shift;
  my $d = shift;
  my %opts = @_;

  # JIJ -> JI

  &Fxtran::Identifier::rename ($d, 'JIJ' => 'JI');
}

sub handleMessages
{
  shift;
  my $d = shift;
  my %opts = @_;

  &Fxtran::Print::useABOR1_ACC ($d);
  &Fxtran::Print::changeWRITEintoPRINT ($d);
  &Fxtran::Print::changePRINT_MSGintoPRINT ($d);
}

sub dim2ind
{
  shift;
  my $dim = shift;

  my %dim2ind = 
  (
    'D%NIT'  => 'JI',
    'D%NIJT' => 'JI',
    'D%NKT'  => 'JK',
  );

  return $dim2ind{$dim};
}

sub dim2bnd
{
  shift;
  my $dim = shift;

  my %dim2bnd =
  (
    'D%NIT'  => [qw (D%NIB D%NIE)],
    'D%NIJT' => [qw (D%NIJB D%NIJE)],
  );

  return @{ $dim2bnd{$dim} || [] };
}

sub customIterator
{
  return 'D';
}

sub customIteratorCopy
{
  return 'DD';
}

sub updateCustomIterator
{
  shift;
  my $stmt = shift;

  my $p = $stmt->parentNode;

  $p->insertAfter (&s ("D%NIE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("D%NIB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

  $p->insertAfter (&s ("D%NIJE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("D%NIJB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

}

sub updateCustomIteratorCopy
{
  shift;
  my $stmt = shift;

  my $p = $stmt->parentNode;

  $p->insertAfter (&s ("DD%NIE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("DD%NIB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

  $p->insertAfter (&s ("DD%NIJE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("DD%NIJB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

  $p->insertAfter (&s ("DD = D"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
}

sub customIteratorDecl
{
  &s ('TYPE (DIMPHYEX_T) :: D');
}

sub customIteratorCopyDecl
{
  &s ('TYPE (DIMPHYEX_T) :: DD');
}

sub matchDocument
{
  shift;
  my $d = shift;

  my @pu;

  for my $pu (&F ('./object/file/program-unit', $d))
    {
      if ($pu->firstChild->nodeName eq 'module-stmt')
        {
          push @pu, 
           &F ('./program-unit', $pu),
           &F ('./specification-part/declaration-part/interface-construct/program-unit', $pu);
        }
      else
        {
          push @pu, $pu;
        }
    }

  for my $pu (@pu)
    {

      next unless (&F ('./subroutine-stmt/dummy-arg-LT/arg-N[string(.)="D"]', $pu));

      if (my ($decl) = &F ('./specification-part/declaration-part/T-decl-stmt'
                         . '[_T-spec_/derived-T-spec[string(T-N)="DIMPHYEX_T"]]'
                         . '[./EN-decl-LT/EN-decl[string(EN-N)="D"]]', $pu))
        {
          return 1;
        }

      return 1 unless (&F ('./execution-part//do-construct', $pu));

      next unless (&F ('./specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JI" or string(.)="JIJ"]', $pu));

      return 1;
    }

  return;
}

sub generateInterface
{
  my $class = shift;
  my ($F90, %opts) = @_;
  &Fxtran::Interface::modi ($F90, $opts{dir});
}

sub setOpenACCInterfaces
{
  shift;
  my ($d, %opts) = @_;

  my ($up) = &F ('./specification-part/use-part', $d);
  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  my @called = &F ('.//call-stmt/procedure-designator', $ep, 1);

  my $suffix = $opts{suffix};

  # Fxtran::Include MODI_* interfaces
  
  my @modi = &F ('./use-stmt[starts-with(string(module-N),"MODI_")]', $up);

  my @called_openacc = &uniq (grep { m/$suffix$/  } @called);

  for my $modi (@modi)
  {
    my ($proc) = &F ('./module-N', $modi, 1);
    $proc =~ s/^MODI_//o;
    if (grep { $proc eq $_ } @called)
      {
        $proc .= $suffix;
        next unless (grep { $_ eq $proc } @called_openacc);
        my ($use) = &s ("USE MODI_$proc");
        $modi->parentNode->insertAfter ($use, $modi);
        $modi->parentNode->insertAfter (&t ("\n"), $modi);
      }
  }
  
}

1;
