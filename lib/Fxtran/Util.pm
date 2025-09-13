package Fxtran::Util;

=head1 NAME

Fxtran::Util

=head1 DESCRIPTION

This module provides various utilities.

=head1 FUNCTIONS

=cut

use FileHandle;
use File::Path;
use File::Spec;
use File::Basename;
use Data::Dumper;

use strict;

use Fxtran;


sub updateFile
{

=head2 updateFile

Update a file with new contents. Do do write to the file if its 
content did not change.

=cut

  my ($file, $code, %opts) = @_;

  my $c = do { local $/ = undef; my $fh = 'FileHandle'->new ("<$file"); $fh ? <$fh> : undef };

  if ($opts{time})
    {
      $c =~ s/\n! time = \d+\n$/\n/goms;
    }

  my $eq =  defined ($c) && ($c eq $code);

  unless ($eq)
    {
      unlink ($file);
      &mkpath (&dirname ($file));
      my $fh = 'FileHandle'->new (">$file"); 
      $fh or die ("Cannot write to $file");
      $fh->print ($code); 

      unless (substr ($code, -1 , 1) eq "\n")
        {
          $fh->print ("\n");
        }

      if ($opts{version})
        {
          my $version = &Fxtran::getVersion ();
          $fh->print ("! version = $version\n");
        }

      if (my $from = $opts{from})
        {
          $fh->print ("! from = $from\n");
        }

      my $time = time ();

      if ($opts{time})
        {
          $fh->print ("! time = $time\n");
        }

      $fh->close ();

      if ($opts{time})
        {
          utime ($time, $time, $file);
        }
    }
}

{
my $count = 0;

sub runCommand
{

=head2 runCommand

Run an external command, save the command to a file
if debug mode is enabled.

=cut

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

=head2 loadModule

Load a Perl module dynamically. The purpose 
of this function is to avoid loading all libraries
each time the software is run.

=cut

  my $module = shift;

  eval "use $module";
  my $c = $@;
  die ($c) if ($c);
}

sub slurp
{

=head2 slurp

Read the contents of a file.

=cut

  my $f = shift;
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
} 

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
