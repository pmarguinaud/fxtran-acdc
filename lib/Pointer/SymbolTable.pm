package Pointer::SymbolTable;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Data::Dumper;

sub getFieldAPIList
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

sub getConstantList
{
  my ($doc, $dir) = @_;
 
  # Guess constant list 

  my @constant = ();

  my @decl = &F ('.//T-decl-stmt[_T-spec_/derived-T-spec', $doc);

  for my $decl (@decl)
    {
      my ($type) = &F ('./_T-spec_/derived-T-spec/T-N', $decl, 1);
      if (-f "$dir/$type.pl")
        {
          my @N = &F ('.//EN-N', $decl, 1);
          push @constant, @N;
        }
    }

  return @constant;
}

sub getSymbolTable
{
  my ($doc, %opts) = @_;

  my %skip = map { ($_, 1) } @{ $opts{skip} || [] };

  my %nproma = map { ($_, 1)} @{ $opts{nproma} };

  my @fieldapi = &getFieldAPIList ($doc, $opts{'types-fieldapi-dir'});
  my %fieldapi = map { ($_, 1) } @fieldapi;

  my @constant = &getConstantList ($doc, $opts{'types-constant-dir'});
  my %constant = map { ($_, 1) } @constant;

  my @args = &F ('.//subroutine-stmt/dummy-arg-LT/arg-N/N/n/text()', $doc);
  my %args = map { ($_->textContent, $_) } @args;

  my @en_decl = &F ('.//EN-decl', $doc);

  my %t;

  my @pointer;

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('.//EN-N', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);

      my ($ts) = &F ('./_T-spec_/*', $stmt);

      my $blocked = 1;

      if ($ts->nodeName eq 'derived-T-spec')
        {
          my ($tn) = &F ('./T-N', $ts, 1);
          if (grep { $tn eq $_ } @{ $opts{'types-fieldapi-non-blocked'} || [] })
            {
              $blocked = 0;
            }
        }

      my ($as) = &F ('./array-spec', $en_decl);
      my @ss = $as ? &F ('./shape-spec-LT/shape-spec', $as) : ();
      my $nd = scalar (@ss);

      $t{$N} = {
                 object => $fieldapi{$N},
                 constant => $constant{$N},
                 skip => $skip{$N},
                 isFieldAPI => $as && $nproma{$ss[0]->textContent},
                 arg => $args{$N} || 0, 
                 ts => $ts->cloneNode (1), 
                 as => $as ? $as->cloneNode (1) : undef, 
                 nd => $nd,
                 en_decl => $en_decl,
                 blocked => $blocked,
               };

      my ($pointer) = &F ('.//attribute-N[string(.)="POINTER"]', $stmt);
      if ($pointer)
        {
          push @pointer, $en_decl;
          $t{$N}{pointer} = 1;
        }
    }

  # Second pass : examine pointers who points to NPROMA data, they will get the nproma attribute

  for my $en_decl (@en_decl)
    {
      my ($N) = &F ('.//EN-N', $en_decl, 1);
      my @ta = &F ('.//pointer-a-stmt[./E-1/named-E[string(.)="?"]]/E-2/named-E/N', $N, $doc, 1);
      for my $ta (@ta)
        {
          $t{$N}{isFieldAPI} = $t{$ta}{isFieldAPI};
          $t{$N}{nd}         = $t{$ta}{nd};
          $t{$N}{as}         = $t{$ta}{as}->cloneNode (1);
          last;
        }
    }


  return \%t;
}

sub getFieldType
{
  my ($nd, $ts) = @_;

  ($ts = $ts->textContent) =~ s/\s+//go;

  my %ts = ('INTEGER(KIND=JPIM)' => 'IM', 'REAL(KIND=JPRB)' => 'RB', LOGICAL => 'LM');

  return unless (defined ($ts{$ts}));

  return "FIELD_${nd}$ts{$ts}";
}

1;
