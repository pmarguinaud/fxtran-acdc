package Util;

use FileHandle;
use File::Path;
use File::Basename;

use strict;

use Fxtran;


sub updateFile
{
  my ($F90, $code) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$F90"); $fh ? <$fh> : undef };
  
  if ((! defined ($c)) || ($c ne $code))
    {
      unlink ($F90);
      &mkpath (&dirname ($F90));
      my $fh = 'FileHandle'->new (">$F90"); 
      $fh or die ("Cannot write to $F90");
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

1;
