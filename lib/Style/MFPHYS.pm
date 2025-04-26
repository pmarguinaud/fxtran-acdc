package Style::MFPHYS;

use base qw (Style::IAL);
use Fxtran;

use strict;

sub nproma
{
  return qw (KLON YDGEOMETRY%YRDIM%NPROMA);
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

sub jlev
{
  return 'JLEV';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JLON");
}

1;
