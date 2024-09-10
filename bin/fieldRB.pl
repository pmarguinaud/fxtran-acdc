#!/usr/bin/perl -w

use strict;
use FileHandle;
use Data::Dumper;
  
for my $f (<types-fieldapi/FIELD_*RD_ARRAY.pl>)
  {
    (my $g = $f) =~ s/RD_/RB_/go;
    my $text = do { my $fh = "FileHandle"->new ("<$f"); local $/ = undef; <$fh> };
    for ($text)
      {
        s/JPRD/JPRB/go;
        s/RD_/RB_/go;
      }
    "FileHandle"->new (">$g")->print ($text);
  }


