#!/usr/bin/perl -w

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use FieldAPI::Parallel;

my ($f) = @ARGV;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 500)]);

&FieldAPI::Parallel::makeParallelUpdateView ($d);

print $d->textContent;

'FileHandle'->new (">$f.new")->print ($d->textContent ());
'FileHandle'->new (">$f.new.xml")->print ($d->toString ());


