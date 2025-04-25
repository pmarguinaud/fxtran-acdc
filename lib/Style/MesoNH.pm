package Style::MesoNH;

use strict;

use base qw (Style);

use Fxtran;

sub nproma
{
  return qw (D%NIT D%NIJT);
}

sub kidia
{
  return 'D%NIB';
}

sub kfdia
{
  return 'D%NIE';
}

sub jlon
{
  return 'JIJ';
}

sub declareJlon
{
  return &s ("INTEGER :: JI");
}

sub includeExtension
{
  return '.h';
}

sub removeUnusedIncludes
{
  return 1;
}

1;
