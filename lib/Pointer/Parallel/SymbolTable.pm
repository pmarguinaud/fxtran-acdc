package Pointer::Parallel::SymbolTable;

use strict;
use Fxtran;

my @object = qw (YDMF_PHYS_BASE_STATE YDMF_PHYS_NEXT_STATE YDCPG_MISC YDCPG_PHY9
                 YDCPG_PHY0 YDMF_PHYS YDCPG_DYN9 YDCPG_DYN0 YDMF_PHYS_SURF YDVARS
                 YDCPG_SL1 YLMF_PHYS_NEXT_STATE YDCPG_GPAR);
my %object = map { ($_, 1) } @object;
my @skip = qw (PGFL PGFLT1 PGMVT1 PGPSDT2D);
my %skip = map { ($_, 1) } @skip;

my $conf0 = 
{
  NPROMA => 'YDCPG_OPTS%KLON'
};

sub getSymbolTable
{
  my ($doc, $conf) = @_;

  $conf ||= $conf0;

  my @args = &F ('.//subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $doc);
  my %args = map { ($_->textContent, $_) } @args;

  my @en_decl = &F ('.//EN-decl', $doc);

  my %t;

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('.//EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my ($ts) = &F ('./_T-spec_/*', $stmt);
      my ($as) = &F ('./array-spec', $en_decl);
      my @ss = $as ? &F ('./shape-spec-LT/shape-spec', $as) : ();
      my $nd = scalar (@ss);
      $t{$N} = {
                 object => $object{$N},
                 skip => $skip{$N},
                 nproma => $as && $ss[0]->textContent eq $conf->{NPROMA},
                 arg => $args{$N} || 0, 
                 ts => $ts->cloneNode (1), 
                 as => $as ? $as->cloneNode (1) : undef, 
                 nd => $nd,
                 en_decl => $en_decl,
               };
    }

  return \%t;
}

sub getFieldType
{
  my ($nd, $ts) = @_;

  $nd++;

  ($ts = $ts->textContent) =~ s/\s+//go;

  my %ts = ('INTEGER(KIND=JPIM)' => 'INT', 'REAL(KIND=JPRB)' => '', LOGICAL => 'LOG');

  return unless (defined ($ts{$ts}));

  return "FIELD_$ts{$ts}${nd}D";
}

1;
