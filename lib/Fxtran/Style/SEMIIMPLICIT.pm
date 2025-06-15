package Fxtran::Style::SEMIIMPLICIT;

use Data::Dumper;

use base qw (Fxtran::Style::IAL);
use Fxtran;

use strict;

sub nproma
{
  return qw (KSPEC2V KSTA:KEND);
}

sub jlon
{
  return 'JSP';
}

sub kidia
{
  return 'KSTA';
}

sub kfdia
{
  return 'KEND';
}

sub matchDocument
{
  shift;
  my $d = shift;

  my ($dp) = &F ('./object/file/program-unit/specification-part/declaration-part', $d);

  if (&F ('./T-decl-stmt/EN-decl-LT/EN-decl/array-spec/shape-spec-LT[string(shape-spec[1])="KSPEC2V"]', $dp))
    {
      return 1;
    }
}

1;
