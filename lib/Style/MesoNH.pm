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

sub noComputeRoutine
{
  return 1 if ($_[0] eq 'PRINT_MSG');
}

1;
