#!/usr/bin/perl -w

use strict;

use FileHandle;
use Data::Dumper;
use File::Basename;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib "$Bin/../lib";

use Common;

use Fxtran;
use Outline1;
use Canonic;
use Directive;
use Intrinsic;

my $f = shift;

my $d = &Fxtran::parse (location => $f, fopts => [qw (-construct-tag -no-include -line-length 500 -directive ACDC -canonic)]);
&Canonic::makeCanonic ($d);

&Directive::parseDirectives ($d, name => 'ACDC');



my ($pu) = &F ('./object/file/program-unit', $d);


my $DEP = &Outline1::getVariables ($pu);

my @par = &F ('.//parallel-section', $d);


for my $i (0 .. $#par)
  {
    my $par = $par[$i];

#   my $parName = $par->getAttribute ('name') || sprintf ('PARALLEL.%3.3d', $i);
    my $parName = sprintf ('PARALLEL_%3.3d', $i);

    &Outline1::outline ($pu, $par, $parName, $DEP);
  }

'FileHandle'->new ('>' . &basename ($f))->print ($d->textContent);

__END__

for my $n (keys (%$DEP))
  {
    print &Dumper ([$n, $DEP->{$n}]) if (exists ($DEP->{$n}{count}) && ($DEP->{$n}{count} == 0));
  }
