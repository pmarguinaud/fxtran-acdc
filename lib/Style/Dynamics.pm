package Style::Dynamics;

use base qw (Style::IAL);
use Fxtran;

use strict;

sub nproma
{
  return qw (KPROMA YDGEOMETRY%YRDIM%NPROMA YDGEOMETRY%YRDIM%NPROMNH YDCPG_OPTS%KLON);
}

sub kidia
{
  return 'KST';
}

sub kfdia
{
  return 'KEND';
}

sub jlon
{
  return 'JROF';
}

sub jlev
{
  return 'JLEV';
}

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JROF");
}

1;
