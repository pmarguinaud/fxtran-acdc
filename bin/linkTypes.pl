#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use FileHandle;
use Data::Dumper;
use File::Basename;
use Storable;
use Getopt::Long;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

my %opts = ('types-fieldapi-dir' => 'types-fieldapi');
my @opts_f = qw (help);
my @opts_s = qw (types-fieldapi-dir);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

if ($opts{help})
  {
    print
     "Usage: " . &basename ($0) . "\n" .
      join ('', map { "  --$_\n" } @opts_f) .
      join ('', map { "  --$_=...\n" } @opts_f) .
     "\n";
    exit (0);
  }

my $dir = $opts{'types-fieldapi-dir'};

my %T;

for my $f (<$dir/*.pl>)
  {
    my $T = &basename ($f, qw (.pl));
    $T{$T} = do ("./$f");
  }


for my $T (keys (%T))
  {
    if (my $super = $T{$T}{super})
      {
        $T{$T}{super} = $T{$super};
      }
    for my $v (values (%{ $T{$T}{comp} }))
      {
        next unless (my $ref = ref ($v));
        if ($ref eq 'SCALAR')
          {
            my $t = $$v;
            $v = $T{$t};
          }
      }
  }

my %TT;

while (my ($t, $h) = each (%T))
  {
    my %h;
    for (my $hh = $h; $hh; $hh = $hh->{super})
      {
        while (my ($k, $v) = each (%{ $hh->{comp} }))
          {
            next unless (my $ref = ref ($v));
            if ($ref eq 'HASH')
              {
                $TT{$v->{name}} ||= {};
                $h{$k} = $TT{$v->{name}};
              }
            elsif ($ref eq 'ARRAY')
              {
                $h{$k} = $v;
              }
          }
      }
    $TT{$t} ||= {};
    %{ $TT{$t} } = %h;
  }

#print &Dumper (\%TT);

my %UU;

for my $T (keys (%T))
  {
    $UU{$T} = $T{$T}{update_view};
  }

&Storable::nstore (\%TT, "$dir/types.dat");
&Storable::nstore (\%UU, "$dir/update_view.dat");

sub walk
{
  my ($p, $h, $r) = @_;

  if (ref ($h) eq 'ARRAY')
    {
      my ($k) = ($p =~ m/%(\w+)$/o);
      $r->{$p} = $h->[2] . ' :: ' . $k . '(' . join (',', (':') x $h->[1]) . ')';
    }
  elsif (ref ($h) eq 'HASH')
    {
      for my $k (sort keys (%$h))
        {
          &walk ($p . '%' . $k, $h->{$k}, $r);
        }
    }

}

my %RR;

for my $t (sort keys (%TT))
  {
    &walk ($t, $TT{$t}, \%RR);
  }

&Storable::nstore (\%RR, "$dir/decls.dat");


