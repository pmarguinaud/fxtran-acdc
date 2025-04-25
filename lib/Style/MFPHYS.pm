package Style::MFPHYS;

use base qw (Style);
use Fxtran;

use strict;

sub nproma
{
  return qw (KLON YDGEOMETRY%YRDIM%NPROMA YDCPG_OPTS%KLON);
}

sub kidia
{
  return 'KIDIA';
}

sub kfdia
{
  return 'KFDIA';
}

sub jlon
{
  return 'JLON';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JLON");
}

sub includeExtension
{
  return '.intfb.h';
}

sub noComputeRoutine
{
  shift;
  return 1 if ($_[0] eq 'ABOR1');
}

1;
