package Style::ECPHYS;

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
  return 'JL';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JL");
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
