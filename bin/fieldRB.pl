#!/usr/bin/perl -w

use strict;
use FileHandle;
use Data::Dumper;
use Getopt::Long;

my %opts = ('types-fieldapi-dir' => 'types-fieldapi');
my @opts_f = qw (help);
my @opts_s = qw (types-fieldapi-dir);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

my $dir = $opts{'types-fieldapi-dir'};

for my $f (<$dir/FIELD_*RD_ARRAY.pl>)
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


