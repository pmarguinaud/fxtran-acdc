#!/usr/bin/perl -w

use strict;

use threads;
use Thread::Queue;

use Cwd;
use FileHandle;
use Data::Dumper;
use File::Path;
use File::Basename;
use FindBin qw ($Bin);
use Getopt::Long;

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
  my $f = $args{file};
  my $log = $args{log};

  my $cwd = &cwd ();

  my ($dir, $view, $F90) = ($f =~ m{^(\w+)/(\w+)/(.*)$}o);
  
  $dir = join ('/', $dir, 'local', &dirname ($F90));
  
  &mkpath ($dir) unless (-d $dir);
  
  my @cmd = ("$Bin/../../bin/fxtran-f90", '--config', 'fxtran-util.conf', '--dir', $dir, '--dryrun', '--', 'f90', '-c', $f);

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

  unlink ($_) for (<run.*.log>);

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
                     &processFile (tid => $tid, file => $f, log => $fh); 
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

for my $d (qw (types-fieldapi types-constant src/local hub/local))
  {
    &rmtree ($d) if (-d $d);
  }


&processList (\%opts, @mod);
&processList (\%opts, @src);

