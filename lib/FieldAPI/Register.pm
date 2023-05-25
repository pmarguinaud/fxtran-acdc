package FieldAPI::Register;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use FileHandle;
use Data::Dumper;

use Fxtran;

sub registerFieldAPI1
{
  my ($tc, $opts, $class) = @_;

  my %h;

  my ($type) = &F ('.//T-stmt/T-N/N/n/text()', $tc, 1);

  my ($abstract) = &F ('./T-stmt/attribute[string(attribute-N)="ABSTRACT"]', $tc);
  my ($extends) = &F ('./T-stmt/attribute[string(attribute-N)="EXTENDS"]/N/n/text()', $tc);

  my @en_decl = &F ('.//EN-decl', $tc);
  my %en_decl;

  for my $en_decl (@en_decl)
    {
      my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
      $en_decl{$name} = $en_decl;
    }

  for my $en_decl (@en_decl)
    {
      my ($name) = &F ('.//EN-N/N/n/text()', $en_decl, 1);
      my ($stmt) = &Fxtran::stmt ($en_decl);
      my %attr = map { ($_, 1) } &F ('.//attribute/attribute-N/text()', $stmt);

      my ($tspec) = &F ('./_T-spec_', $stmt);
      if (my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec, 1))
        {
          $h{$name} = \$tname unless ($tname =~ m/^FIELD_/o);
        }
      elsif ($class && (my $fam = $class->getFieldAPIMember ($type, $name, \%attr, \%en_decl)))
        {
          my @ss = &F ('./array-spec/shape-spec-LT/shape-spec', $en_decl);
          $h{$name} = [$fam, scalar (@ss), $tspec->textContent];
        }
    }


  my $update_view = 0;
  if (&F ('./procedure-stmt/procedure-N-LT/rename[string(use-N)="UPDATE_VIEW"]', $tc))
    {
      $update_view = 1;
    }

  return {comp => \%h, name => $type, super => ($extends && $extends->textContent), update_view => $update_view};
}

sub registerFieldAPI
{
  my ($d, $opts) = @_;

  my $class;

  if ($class = $opts->{'field-api-class'})
    {
      eval "use $class";
      my $c = $@;
      $c && die ($c);
    }

  my @tc = &F ('.//T-construct', $d);
  
  for my $tc (@tc)
    {
      my ($type) = &F ('.//T-stmt/T-N/N/n/text()', $tc, 1);
      next if ($opts->{'skip-types'}->($type));
      my $h = &registerFieldAPI1 ($tc, $opts, $class);
      local $Data::Dumper::Sortkeys = 1;
      'FileHandle'->new (">$opts->{'types-fieldapi-dir'}/$type.pl")->print (&Dumper ($h));
  }

}

1;
