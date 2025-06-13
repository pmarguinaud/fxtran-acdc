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

use Fxtran::Common;

use Fxtran;
use Fxtran::Outline;
use Fxtran::Decl;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-construct-tag -no-include -line-length 500 -directive ACDC -canonic)]);

&Fxtran::Decl::forceSingleDecl ($d);
my @o = &Fxtran::Outline::outline ($d);

print $d->textContent, "\n";

print &Dumper ([map { ($_->[0]->textContent, $_->[1]->textContent) } @o]);

