package Style::ECPHYS;

use base qw (Style::IAL);
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

sub jlev
{
  return 'JK';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JL");
}

1;
