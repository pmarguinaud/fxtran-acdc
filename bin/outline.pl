#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Outline;
use Decl;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-line-length 500)]);

&Decl::forceSingleDecl ($d);
my @o = &Outline::outline ($d);

print $d->textContent, "\n";

print &Dumper ([map { ($_->[0]->textContent, $_->[1]->textContent) } @o]);

