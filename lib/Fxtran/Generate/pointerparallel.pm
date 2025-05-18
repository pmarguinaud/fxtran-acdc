package Fxtran::Generate::pointerparallel;

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
  my ($doc, $text, $opts, $intfb) = @_;

  $intfb->{pointerparallel} = '';

  my ($pointerparallel) = map { m/^!\$ACDC\s+(pointerparallel.*)/o ? ($1) : ()  } @$text;

  if ($pointerparallel && $opts->{'merge-interfaces'})
    {
      use File::Temp;

      my @pointerparallel = split (m/\s+/o, $pointerparallel);

      my $tmp = 'File::Temp'->new (SUFFIX => '.F90', TEMPLATE => 'fxtranXXXXX');

      $tmp->print ("!\$ACDC @pointerparallel\n", $doc->textContent);

      my @opts = 'click'->hashToCommandLine (method => 'pointerparallel', package => __PACKAGE__, opts => $opts);

      &Fxtran::Util::runCommand (cmd => ['fxtran-f90', @opts, '--dryrun', '--dir', '.', '--', 'f90', '-c', $tmp]);

      my $suffix = lc ($opts->{'suffix-pointerparallel'});
      (my $tmp_pointerparallel = $tmp) =~ s/\.F90$/$suffix.F90/;

      my $doc = &Fxtran::parse (location => $tmp_pointerparallel, fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);

      &Fxtran::Canonic::makeCanonic ($doc, %$opts);

      &Fxtran::Interface::intfbBody ($doc);

      $_->unbindNode () for (&F ('.//a-stmt', $doc));

      $intfb->{pointerparallel} = $doc->textContent ();
      $intfb->{pointerparallel} =~ s/^\s*\n$//goms;

      unlink ($_) for ($tmp_pointerparallel, "$tmp_pointerparallel.xml");
    }
}

1;
