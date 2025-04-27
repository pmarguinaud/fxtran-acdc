package Style::MESONH;

use strict;

use base qw (Style);

use Include;
use Fxtran;
use Print;
use Identifier;

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
  return 'JI';
}

sub jlev
{
  return 'JK';
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
  shift;
  return 1 if ($_[0] =~ m/^(?:PRINT_MSG|DR_HOOK)$/o);
}

sub preProcessForOpenACC
{
  shift;
  my $d = shift;
  my %opts = @_;

  &Include::loadContainedIncludes ($d, %opts);

  # JIJ -> JI

  &Identifier::rename ($d, 'JIJ' => 'JI');
}

sub handleMessages
{
  shift;
  my $d = shift;
  my %opts = @_;

  &Print::useABOR1_ACC ($d);
  &Print::changeWRITEintoPRINT ($d);
  &Print::changePRINT_MSGintoPRINT ($d);
}

sub dim2ind
{
  shift;
  my $dim = shift;

  my %dim2ind = 
  (
    'D%NIT'  => 'JI',
    'D%NIJT' => 'JI',
    'D%NKT'  => 'JK',
  );

  return $dim2ind{$dim};
}

sub dim2bnd
{
  shift;
  my $dim = shift;

  my %dim2bnd =
  (
    'D%NIT'  => [qw (D%NIB D%NIE)],
    'D%NIJT' => [qw (D%NIJB D%NIJE)],
  );

  return @{ $dim2bnd{$dim} || [] };
}

sub customIterator
{
  return 'D';
}

sub updateCustomIterator
{
  shift;
  my $stmt = shift;

  my $p = $stmt->parentNode;

  $p->insertAfter (&s ("D%NIE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("D%NIB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

  $p->insertAfter (&s ("D%NIJE = YLCPG_BNDS%KFDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);
  $p->insertAfter (&s ("D%NIJB = YLCPG_BNDS%KIDIA"), $stmt);
  $p->insertAfter (&t ("\n"), $stmt);

}

1;
