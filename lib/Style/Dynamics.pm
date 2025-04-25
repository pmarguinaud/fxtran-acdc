package Style::Dynamics;

use base qw (Style);
use Fxtran;

use strict;

sub nproma
{
  return qw (KPROMA YDGEOMETRY%YRDIM%NPROMA YDCPG_OPTS%KLON);
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

sub declareJlon
{
  return &s ("INTEGER (KIND=JPIM) :: JROF");
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
