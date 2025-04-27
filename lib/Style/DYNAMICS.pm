package Style::DYNAMICS;

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

sub matchDocument
{
  shift;
  my $d = shift;


  if (&F ('./object/file/program-unit/subroutine-stmt/dummy-arg-LT/arg-N[string(.)="KST"]', $d))
    {
       return 1 unless (&F ('.//do-construct', $d));
       return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JROF"]', $d));
    }
  else
    {
       return unless (&F ('./object/file/program-unit/specification-part/declaration-part/T-decl-stmt//EN-N[string(.)="JROF"]', $d));
    }
}

1;
