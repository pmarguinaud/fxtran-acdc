package Fxtran::Generate::Interface;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Temp;
use File::Basename;
use Cwd;

use strict;

use click;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Util;
use Fxtran::Interface;

sub interface
{
  my ($doc, $text, $opts, $intfb, $method) = @_;

  $$intfb = '';

  my ($directive) = map { m/^!\$ACDC\s+($method.*)/ ? ($1) : ()  } @$text;

  return unless ($directive && $opts->{'merge-interfaces'});

  my @directive = split (m/\s+/o, $directive);

  my $tmpdir = 'File::Temp'->newdir ();

  my $F90 = "$tmpdir/file.F90";
  my $fh = 'FileHandle'->new (">$F90");

  $fh->print ("!\$ACDC @directive\n", $doc->textContent);

  $fh->close ();

  my @opts = 'click'->hashToCommandLine (method => $method, package => 'Fxtran::Generate', opts => $opts);

  my @cmd = ('fxtran-f90', @opts, '--dryrun', '--dir', $tmpdir, '--tmp', '.', '--', 'f90', '-c', $F90);

  &Fxtran::Util::runCommand (cmd => \@cmd);

  my $suffix = lc ($opts->{"suffix-$method"});
  (my $F90_directive = $F90) =~ s/\.F90$/$suffix.F90/;

  $doc = &Fxtran::parse (location => $F90_directive, fopts => ['-construct-tag', '-no-include', '-line-length' => 500], dir => $opts->{tmp});

  &Fxtran::Canonic::makeCanonic ($doc, %$opts);

  &Fxtran::Interface::intfbBody ($doc);

  $_->unbindNode () for (&F ('.//a-stmt', $doc));

  $$intfb = $doc->textContent ();
  $$intfb =~ s/^\s*\n$//goms;
}

1;
