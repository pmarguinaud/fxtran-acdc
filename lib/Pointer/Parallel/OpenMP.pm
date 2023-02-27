package Pointer::Parallel::OpenMP;

use strict;
use Pointer::Parallel;
use Fxtran;

sub getDefaultWhere
{
  'host';
}

sub setOpenMPDirective
{
  my ($par, $t) = @_;

  my @firstprivate = ('YLCPG_BNDS');
  my %firstprivate = map { ($_, 1) } @firstprivate;

  my @priv = &Pointer::Parallel::getPrivateVariables ($par, $t);
  @priv = grep { ! $firstprivate{$_} } @priv;

  my ($do) = &F ('.//do-construct[./do-stmt[string(do-V)="JBLK"]]', $par);

  my $indent = &Fxtran::getIndent ($do);

  my $C = &n ('<C>!$OMP PARALLEL DO PRIVATE (' . join (', ', @priv)  . ') FIRSTPRIVATE (' . join (', ', @firstprivate) . ')</C>');
  
  $do->parentNode->insertBefore ($C, $do);
  $do->parentNode->insertBefore (&t ("\n" . (' ' x $indent)), $do);

}

sub makeParallel
{
  shift;
  my ($par, $t) = @_;

  my $init = &s ('CALL YLCPG_BNDS%INIT (YDCPG_OPTS)');
  my ($do) = &F ('./do-construct', $par);

  $par->insertBefore ($init, $do);
  $par->insertBefore (&t ("\n"), $do);

  &setOpenMPDirective ($par, $t);

  return $par;
}


1;
