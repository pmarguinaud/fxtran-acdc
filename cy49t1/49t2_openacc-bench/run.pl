#!/usr/bin/perl -w

use strict;

use Cwd;
use FileHandle;
use Data::Dumper;
use File::Path;
use File::Basename;
use FindBin qw ($Bin);

sub getLines
{
  my $f = shift;
  my @lst = do { my $fh = 'FileHandle'->new ("<$f"); <$fh> }; 
  chomp for (@lst);
  return @lst;
}


sub processList
{
  my @lst = @_;

  my $cwd = &cwd ();

  for my $f (@lst)
    {
      my ($dir, $view, $F90) = ($f =~ m{^(\w+)/(\w+)/(.*)$}o);
      
      $dir = join ('/', $dir, 'local', &dirname ($F90));
  
      &mkpath ($dir) unless (-d $dir);
  
      my @cmd = ("$Bin/../../bin/fxtran-f90", 
                 '--types-constant-dir', "$cwd/types-constant", 
                 '--types-fieldapi-dir', "$cwd/types-fieldapi",
                 '--cycle', '49', '--create-interface', '--dir', $dir, 
                 '--dryrun', '--', 'f90', '-c', $f);

      print "@cmd\n";
  
      system (@cmd) 
        and die ("Command `@cmd' failed");
    }

}

my $cwd = &cwd ();

$ENV{TMPDIR}      = "$cwd/tmp";
$ENV{TARGET_PACK} = $cwd;

my @mod = &getLines ("list.mod");
my @src = &getLines ("list.src");

&processList (@mod);
&processList (@src);

