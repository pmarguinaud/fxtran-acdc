package Style::MFPHYSTOP;

use base qw (Style::MFPHYS);
use Fxtran;

use strict;

sub nproma
{
  return qw (YDGEOMETRY%YRDIM%NPROMA YDCPG_OPTS%KLON);
}

sub kidia
{
  return 'YDCPG_BNDS%KIDIA';
}

sub kfdia
{
  return 'YDCPG_BNDS%KFDIA';
}

sub matchDocument
{
  shift;
  my $d = shift;

  return 1 if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDMF_PHYS"]', $d));

  return 1 if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDMF_PHYS_OUT"]', $d));

  return unless (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="YDCPG_OPTS"]', $d));

  return 1 unless (&F ('./object/file/program-unit/execution-part//do-construct', $d));

  return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JLON"]', $d));

  return 1;
}

1;
