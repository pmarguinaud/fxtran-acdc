#!/usr/bin/perl -w

use strict;

use threads;
use Thread::Queue;

use Cwd;
use FileHandle;
use Data::Dumper;
use File::Path;
use File::Copy;
use File::Spec;
use File::Basename;
use FindBin qw ($Bin);
use Getopt::Long;

my $FXTRAN_F90 = &Cwd::realpath ("$Bin/../../bin/fxtran-f90");
my $FXTRAN_GEN = &Cwd::realpath ("$Bin/../../bin/fxtran-gen");

sub runCommand
{
  my @cmd = @_;
  system (@cmd)
    and die ("Command `@cmd' failed");
}

sub getLines
{
  my $f = shift;
  my @lst = do { my $fh = 'FileHandle'->new ("<$f"); <$fh> }; 
  chomp for (@lst);
  return @lst;
}

sub processFile
{
  my %args = @_;
  my ($file, $log, $config) = @args{qw (file log config)};

  my $cwd = &cwd ();

  my ($dir, $view, $F90) = ($file =~ m{^(\w+)/(\w+)/(.*)$}o);
  
  $dir = join ('/', $dir, 'local', &dirname ($F90));
  
  &mkpath ($dir) unless (-d $dir);

  my @cmd = ($FXTRAN_F90, '--config', $config, '--dir', $dir, '--dryrun', '--', 'f90', '-c', $file);

  print "@cmd\n";

  $log->print ("@cmd\n");
  
  if (system (@cmd))
    {
      $log->print ("Command `@cmd' failed");
      die ("Command `@cmd' failed");
    }
}

sub processIntf
{
  my %args = @_;
  my ($file, $log, $config) = @args{qw (file log config)};

  my $cwd = &cwd ();

  my ($dir, $view, $F90) = ($file =~ m{^(\w+)/(\w+)/(.*)$}o);
  
  $dir = join ('/', $dir, 'local', '.intfb');
  
  &mkpath ($dir) unless (-d $dir);

  my @cmd = ($FXTRAN_GEN, 'interface', '--dir', $dir, $file);

  print "@cmd\n";

  $log->print ("@cmd\n");
  
  if (system (@cmd))
    {
      $log->print ("Command `@cmd' failed");
      die ("Command `@cmd' failed");
    }
}

sub processList
{
  my $opts = shift;

  my @lst = @_;

  if ($opts->{threads} > 1)
    {
       my @t;
      
       my $q = 'Thread::Queue'->new ();
      
       for my $tid (1 .. $opts->{threads})
         {
           push @t, 'threads'->create (sub 
           { 
             my $fh = 'FileHandle'->new (sprintf ('>run.%4.4d.log', $tid));

             eval 
               {
                 while (my $f = $q->dequeue ()) 
                   { 
                     $opts->{proc}->(tid => $tid, file => $f, log => $fh, config => $opts->{config}); 
                   } 
               };
             my $c = $@;

             $fh->close ();

             $c && return 0;

             return 1;
           });
         }
      
       for my $f (@lst, (0) x $opts->{threads})
         {
           $q->enqueue ($f);
         }
      
       for (@t)
         {
           $_->join () or die;
         }
    }
  else
    { 
      my $tid = 0;
      my $fh = 'FileHandle'->new (sprintf ('>run.%4.4d.log', $tid));
      for my $f (@lst)
        {
          &processFile (file => $f, tid => $tid, log => $fh);
        }
    }
}

unlink ($_) for (<run.*.log>);


my $cwd = &cwd ();

my %opts = (threads => 4);
my @opts_f = qw (help);
my @opts_s = qw (threads);

&GetOptions
(
  (map { ($_, \$opts{$_}) } @opts_f),
  (map { ("$_=s", \$opts{$_}) } @opts_s),
);

$ENV{TMPDIR}      = "$cwd/tmp";
$ENV{TARGET_PACK} = $cwd;

my @mod = &getLines ("list.mod");
my @src = &getLines ("list.src");
my @int = &getLines ("list.int");

for my $type (qw (typebound util))
  {
    my $config = "fxtran-$type.conf";

    for my $d (qw (types-fieldapi types-constant src/local hub/local))
      {
        &rmtree ($d) if (-d $d);
      }

    &processList ({%opts, config => $config, proc => \&processIntf},  @int);
    &processList ({%opts, config => $config, proc => \&processFile},  @mod);
    &processList ({%opts, config => $config, proc => \&processFile},  @src);

    &rmtree ("run/$type");
    &mkpath ("run/$type") or die;

    for my $dir (qw (hub src))
      {
        &mkpath ("run/$type/$dir") or die;
        if (-d "$dir/local")
          {
            &move ("$dir/local", "run/$type/$dir/local") 
              or die ("Cannot move `$dir/local' into `run/$type/$dir/local'");
          }

        &runCommand (qw (diff -B -w -x *.F90.xml -r), "ref/$type/$dir/local", "run/$type/$dir/local");
      }

  }

