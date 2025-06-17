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

use strict;

use click;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Util;
use Fxtran::Interface;

sub interface
{
  my ($doc, $text, $opts, $intfb, $method) = @_;

  $intfb->{$method} = '';

  my ($directive) = map { m/^!\$ACDC\s+($method.*)/ ? ($1) : ()  } @$text;

  if ($directive && $opts->{'merge-interfaces'})
    {
      use File::Temp;

      my @directive = split (m/\s+/o, $directive);

      my $tmp = 'File::Temp'->new (SUFFIX => '.F90', TEMPLATE => 'fxtranXXXXX', CLEANUP => 0);

      $tmp->print ("!\$ACDC @directive\n", $doc->textContent);

      $tmp->close ();

      my @opts = 'click'->hashToCommandLine (method => $method, package => 'Fxtran::Generate', opts => $opts);

      my @cmd = ('fxtran-f90', @opts, '--dryrun', '--dir', '.', '--tmp', '.', '--', 'f90', '-c', $tmp);

      &Fxtran::Util::runCommand (cmd => \@cmd);

      my $suffix = lc ($opts->{"suffix-$method"});
      (my $tmp_directive = $tmp) =~ s/\.F90$/$suffix.F90/;

      my $doc = &Fxtran::parse (location => $tmp_directive, fopts => ['-construct-tag', '-no-include', '-line-length' => 500], dir => $opts->{tmp});

      &Fxtran::Canonic::makeCanonic ($doc, %$opts);

      &Fxtran::Interface::intfbBody ($doc);

      $_->unbindNode () for (&F ('.//a-stmt', $doc));

      $intfb->{$method} = $doc->textContent ();
      $intfb->{$method} =~ s/^\s*\n$//goms;

      unlink ($_) for ($tmp_directive, "$tmp_directive.xml");
    }
}

1;
