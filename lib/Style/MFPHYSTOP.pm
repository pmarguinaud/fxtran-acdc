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

1;
