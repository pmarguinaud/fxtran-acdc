package Fxtran::IO::spectral;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use Data::Dumper;

sub skip
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return 1 if ($attr->{POINTER} && ! ($comp =~ m/^F_/o));

  return 1 if ($comp =~ m/^SP\dD$/o);
}

sub getFieldAPIMember
{
  my $class = shift;
  my ($type, $comp, $attr, $en_decl_hash) = @_;
  
  return unless ($attr->{POINTER});

  if (my $en_decl = $en_decl_hash->{"F_$comp"})
    {
      my $stmt = &Fxtran::stmt ($en_decl);
      my ($tspec) = &Fxtran::F ('./_T-spec_', $stmt);  
      my ($tname) = &F ('./derived-T-spec/T-N/N/n/text()', $tspec);
      return "F_$comp" if ($tname =~ m/^FIELD_/o);
    }
}

1;
