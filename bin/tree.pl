#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


package scan;

use strict;
use Data::Dumper;
use FileHandle;
use File::Basename;
use File::Find;

my %data;

sub slurp
{
  my ($file, $discard) = @_;

  unless (exists $data{$file})
    {
      my $fh = 'FileHandle'->new ("<$file");
      local $/ = undef;
      $data{$file} = <$fh>;
    }

  return delete ($data{$file}) if ($discard);

  return $data{$file};
}

sub scan
{
  shift;

  my %f2f;
  my %seen;
  
  my $wanted = sub
  {
    my $f = shift;
    return unless ($f =~ m/\.F90$/o);
    return if ($seen{&basename ($f)}++);
    my $code = &slurp ($f, 1);
    my ($s) = ($code =~ m/(?:MODULE|FUNCTION|PROGRAM|SUBROUTINE)[ ]+(\w+)/goms);
    return unless ($s);
    $s = uc ($s);
    $f2f{$s} = $f;
  };
  
  for my $F90 (<compile.gpu_nvhpc/*.F90>)
    {
      $wanted->($F90);
    }
  
  return \%f2f;
}


my %call;

sub call
{
  shift;

  my $file = shift;

  unless (exists $call{$file})
    {
      my $code = &slurp ($file);
      my @code = grep { !/^\s*!/o } split (m/\n/o, $code);
      $code = join (";", @code);
      my @call = ($code =~ m/\bCALL\s+([\w%]+)/goms);

      for (@call)
        {
          s/^.+%//o;
        }

      $call{$file} = \@call;
    }


  return $call{$file};
}

my %size;

sub size
{
  shift;

  my $file = shift;
  
  die $file unless (-f $file);

  unless (exists $size{$file})
    {
      my @code = do { my $fh = 'FileHandle'->new ("<$file"); <$fh> };
      $size{$file} = scalar (@code);
    }

  return $size{$file};
}

package main;

use strict;
use Data::Dumper;
use FileHandle;
use File::Basename;
use GraphViz2;

$ENV{PATH} = "/home/gmap/mrpm/marguina/install/graphviz-2.38.0/bin:$ENV{PATH}";

my $f2f = 'scan'->scan ();

my %g; # Graph
my %L; # Size

my @q = @ARGV; # Routines to process

my %skip1 = map { ($_, 1) } qw (DR_HOOK ABOR1 ABORT INIT FINAL UPDATE_VIEW SYNC_HOST_RDONLY SYNC_HOST_RDWR SAVE COPY WIPE
                                DIFF INITOPTIONS GETOPTION LOAD CHECKOPTIONS UPDATE SYNC_DEVICE_RDONLY SYNC_DEVICE_RDWR);

sub add
{

# Add a routine to the graph

  my $name = shift;
  return if (exists $g{$name});
  $g{$name} ||= [];
  my $file = $f2f->{$name};

# Count line numbers

  unless ($file)
    {
      $L{$name} = '';
      return;
    }
  $L{$name} = 'scan'->size ($file);
}

for my $q (@q)
  {
    &add ($q);
  }

my %seen;

while (my $name = shift (@q))
  {
    my $file = $f2f->{$name};

    next unless ($file);

    my @call = @{ 'scan'->call ($file) };
    
    @call = 
       grep { ! m/^(?:GSTAT|MPL_|JFH_)/o } 
       grep { ! $skip1{$_} } @call;

    for my $call (@call)
      {
        &add ($call);
        push @{ $g{$name} }, $call;
        push @q, $call unless ($seen{$call}++);
      }
    
  }


# Single link, even for multiple calls

while (my ($k, $v) = each (%g))
  {
    my %seen;
    @$v = grep { ! ($seen{$_}++) } grep { $g{$_} } @$v;
  }

my @root = keys (%g); # Never called by any other routine belonging to the graph

while (my ($k, $v) = each (%g))
  {
    my %v = map { ($_, 1) } @$v;
    @root = grep { ! $v{$_} } @root;
  }

sub color
{
  my $name = shift;
  return ();
}

my $root = join ('-', sort @root);

my $g = 'GraphViz2'->new (graph => {rankdir => 'LR', ordering => 'out'}, global => {rank => 'source'});
#my $g = 'GraphViz2'->new (graph => {rankdir => 'TB', ordering => 'out'}, global => {rank => 'source'});

while (my ($k, $v) = each (%g))
  {
    next if ($k =~ m/%/o); # Do not report method call
    $g->add_node (name => $k, label => "$k\n$L{$k}", shape => 'box', &color ($k));
    for my $v (@$v)
      {   
        next if ($v =~ m/%/o); # Do not report method call
        $g->add_edge (from => $k, to => $v);
      }   
  }

$g->run (format => 'svg', output_file => "$root.svg");


