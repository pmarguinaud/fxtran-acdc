package Fxtran::Generate::singlecolumn;

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

  $intfb->{singlecolumn} = '';

  my ($singlecolumn) = map { m/^!\$ACDC\s+(singlecolumn.*)/o ? ($1) : ()  } @$text;

  if ($singlecolumn && $opts->{'merge-interfaces'})
    {
      use File::Temp;

      my @singlecolumn = split (m/\s+/o, $singlecolumn);

      my $tmp = 'File::Temp'->new (SUFFIX => '.F90', TEMPLATE => 'fxtranXXXXX');

      $tmp->print ("!\$ACDC @singlecolumn\n", $doc->textContent);

      my @opts = 'click'->hashToCommandLine (method => 'singlecolumn', package => __PACKAGE__, opts => $opts);

      &Fxtran::Util::runCommand (cmd => ['fxtran-f90', @opts, '--dryrun', '--dir', '.', '--', 'f90', '-c', $tmp]);

      my $suffix = lc ($opts->{'suffix-singlecolumn'});
      (my $tmp_singlecolumn = $tmp) =~ s/\.F90$/$suffix.F90/;

      my $doc = &Fxtran::parse (location => $tmp_singlecolumn, fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);

      &Fxtran::Canonic::makeCanonic ($doc, %$opts);

      &Fxtran::Interface::intfbBody ($doc);

      $_->unbindNode () for (&F ('.//a-stmt', $doc));

      $intfb->{singlecolumn} = $doc->textContent ();
      $intfb->{singlecolumn} =~ s/^\s*\n$//goms;

      unlink ($_) for ($tmp_singlecolumn, "$tmp_singlecolumn.xml");
    }
}

1;
