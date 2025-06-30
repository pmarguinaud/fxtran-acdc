package Fxtran::Util;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use FileHandle;
use File::Path;
use File::Basename;
use Data::Dumper;

use strict;

use Fxtran;


sub updateFile
{
  my ($file, $code) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$file"); $fh ? <$fh> : undef };
  
  if ((! defined ($c)) || ($c ne $code))
    {
      unlink ($file);
      &mkpath (&dirname ($file));
      my $fh = 'FileHandle'->new (">$file"); 
      $fh or die ("Cannot write to $file");
      $fh->print ($code); 
      $fh->close ();
    }
}

sub addVersion
{
  my $d = shift;
  my $version = &Fxtran::getVersion ();
  my ($file) = &F ('./object/file', $d);
  $file->appendChild (&n ("<C>! $version</C>"));
  $file->appendChild (&t ("\n"));
}

{
my $count = 0;

sub runCommand
{
  my %args = @_;
  my @cmd = @{ $args{cmd} };

  $count++;

  if ($args{debug}) 
    {
      my $bash = sprintf ('cmd.%3.3d.sh', $count);

      'FileHandle'->new (">$bash")->print (<< "EOF");
#!/bin/bash

set -x

@{ $args{cmd} }

EOF

      chmod (0755, $bash);
    }

  system (@cmd)
    and die ("Command `@cmd' failed");
}

}

sub loadModule
{
  my $module = shift;
  eval "use $module";
  my $c = $@;
  die ($c) if ($c);
}

sub slurp
{
  my $f = shift;
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
} 

1;
