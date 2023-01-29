#!/usr/bin/perl -w

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Basename;
use File::Spec;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::IO;
use FieldAPI::Register;
use Common;
use Fxtran;

my %opts = (dir => '.', 'types-dir' => 'types');
my @opts_f = qw (size save load copy wipe field-api help);
my @opts_s = qw (skip-components skip-types only-components only-types dir out no-allocate module-map field-api-class tmp types-dir);

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

( -d $opts{dir}) or &mkpath ($opts{dir});
( -d $opts{'types-dir'}) or &mkpath ($opts{'types-dir'});

if (! $opts{'no-allocate'})
  {
    $opts{'no-allocate'} = [];
  }
else
  {
    $opts{'no-allocate'} = [split (m/,/o, $opts{'no-allocate'})];
  }

if (! $opts{'module-map'})
  {
    $opts{'module-map'} = {};
  }
else
  {
    $opts{'module-map'} = {split (m/,/o, $opts{'module-map'})};
  }

sub parseListOrCodeRef
{
  my ($opts, $kw) = @_;

  if (-f "$Bin/$opts->{$kw}.pm")
    {
      my $class = $opts->{$kw};
      eval "use $class;";
      my $c = $@;
      $c && die ($c);
      $opts->{$kw} = sub { $class->skip (@_) };
    }
  elsif ($opts->{$kw} =~ m/^sub /o)
    {
      $opts->{$kw} = eval ($opts->{$kw});
      my $c = $@;
      die $c if ($c);
    }
  elsif ($kw =~ m/-components$/o)
    {
      my @comp = split (m/,/o, $opts->{$kw});
      if ($kw =~ m/^skip-/o)
        {
          $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
        }
      else
        {
          $opts->{$kw} = sub { my ($type, $comp) = @_; grep { $_ eq "$type$comp" } @comp };
        }
    }
  elsif ($kw =~ m/-types$/o)
    {
      my @type = split (m/,/o, $opts->{$kw});
      if ($kw =~ m/^skip-/o)
        {
          $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
        }
      else
        {
          $opts->{$kw} = sub { my ($type) = @_; grep { $_ eq "$type" } @type };
        }
    }
}

sub parseSkipOnly
{
  my ($opts, $skip, $only) = @_;
  if ($opts->{$skip})
    {
      &parseListOrCodeRef ($opts, $skip);
    }
  elsif ($opts->{$only})
    {
      &parseListOrCodeRef ($opts, $only);
      $opts->{$skip} = sub { ! $opts->{$only}->(@_) };
    }
  else
    {
      $opts->{$skip} = sub { 0 };
    }
}

&parseSkipOnly (\%opts, 'skip-components', 'only-components');
&parseSkipOnly (\%opts, 'skip-types', 'only-types');

my $F90 = shift;

my $doc = &Fxtran::parse (location => $F90, fopts => [qw (-construct-tag -no-include -line-length 800)], dir => $opts{tmp});

if ($opts{load} || $opts{save} || $opts{size} || $opts{copy})
  {
    &Fxtran::IO::processTypes ($doc, \%opts);
  }

if ($opts{'field-api'})
  {
    &FieldAPI::Register::registerFieldAPI ($doc, \%opts);
  }


