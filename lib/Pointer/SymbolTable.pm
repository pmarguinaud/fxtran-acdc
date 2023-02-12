package Pointer::SymbolTable;

use strict;
use Fxtran;

sub getObjectList
{
  my ($doc, $dir) = @_;

  # Guess object list 

  my @object = ();

  my @decl = &F ('.//T-decl-stmt[_T-spec_/derived-T-spec', $doc);

  for my $decl (@decl)
    {
      my ($type) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
      if (-f "$dir/$type.pl")
        {
          my @N = &F ('.//EN-N', $decl, 1);
          push @object, @N;
        }
    }

  return @object;
}

sub getSymbolTable
{
  my ($doc, %opts) = @_;

  my %skip = map { ($_, 1) } @{ $opts{skip} || [] };

  my $nproma = $opts{nproma};

  my @object = &getObjectList ($doc, $opts{'types-dir'});
  my %object = map { ($_, 1) } @object;


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
                 nproma => $as && $ss[0]->textContent eq $nproma,
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
