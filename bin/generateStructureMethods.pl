#!/usr/bin/perl -w

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Fxtran::IO;
use Common;
use Fxtran;

sub registerFieldAPI
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
          $h{$name} = [$fam, scalar (@ss)];
        }
    }


  my $update_view = 0;
  if (&F ('./procedure-stmt/procedure-N-LT/rename[string(use-N)="UPDATE_VIEW"]', $tc))
    {
      $update_view = 1;
    }

  return {comp => \%h, name => $type, super => ($extends && $extends->textContent), update_view => $update_view};
}

my %opts = qw (dir .);

&GetOptions
(
  'skip-components=s' => \$opts{'skip-components'}, 'skip-types=s' => \$opts{'skip-types'},
  'only-components=s' => \$opts{'only-components'}, 'only-types=s' => \$opts{'only-types'},
  'dir=s' => \$opts{dir}, 'out=s' => \$opts{out},
  size => \$opts{size}, save => \$opts{save}, load => \$opts{load}, copy => \$opts{copy}, wipe => \$opts{wipe},
  'no-allocate=s' => \$opts{'no-allocate'}, 'module-map=s' => \$opts{'module-map'},
  'field-api' => \$opts{'field-api'}, 'field-api-class=s' => \$opts{'field-api-class'},
  'tmp=s' => \$opts{tmp},
);

( -d $opts{dir}) or &mkpath ($opts{dir});

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
    &Fxtran::IO::process_types ($doc, \%opts);
  }

if ($opts{'field-api'})
  {
    my $class;

    if ($class = $opts{'field-api-class'})
      {
        eval "use $class";
        my $c = $@;
        $c && die ($c);
      }

    my @tc = &F ('.//T-construct', $doc);
    
    for my $tc (@tc)
      {
        my ($type) = &F ('.//T-stmt/T-N/N/n/text()', $tc, 1);
        next if ($opts{'skip-types'}->($type));
        my $h = &registerFieldAPI ($tc, \%opts, $class);
        local $Data::Dumper::Sortkeys = 1;
        'FileHandle'->new (">types/$type.pl")->print (&Dumper ($h));
    }
  }
