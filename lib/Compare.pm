package Compare;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Fxtran;
use List::MoreUtils qw (all);
use File::Spec;
use File::Basename;
use File::Temp;
use File::Copy;

sub compareFiles
{
  my ($f1, $f2, %opts) = @_;

  my ($d1, $d2) = map { &Fxtran::parse (location => $_, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]) } ($f1, $f2);

  my $fh1 = 'File::Temp'->new (UNLINK =>0, SUFFIX => '.F90'); $fh1->print ($d1->textContent); $fh1->flush ();
  my $fh2 = 'File::Temp'->new (UNLINK =>0, SUFFIX => '.F90'); $fh2->print ($d2->textContent); $fh2->flush ();
 
  
  print "==> ", &basename ($f1), " <==\n";
  my @cmd = ('diff', $fh1->filename, $fh2->filename);

  if (system (@cmd))
    {
      if ($opts{'compare-prompt'})
        {
          print "Update $f1 (y/n) ?\n";
          chomp (my $ans = <>);
          if (lc ($ans) eq 'y')
            {
              &copy ($f2, $f1);
            }
        }
      else
       {
         die;
       }
    }
}

sub compareDirectories
{
  my ($dir1, $dir2, %opts) = @_;

  my @f = map { &basename ($_) } glob ("$dir1/*.F90");

  for my $f (@f)
    {
      &compareFiles ("$dir1/$f", "$dir2/$f", %opts);
    }
}

sub compare
{
  if (all { -d } @_[0,1])
    {
      &compareDirectories (@_);
    }
  elsif (all { -d } @_[0,1])
    {
      &compareFiles (@_);
    }
  else
    {
      die;
    }
}

1;
