#!/usr/bin/env perl

# Unit tests for Fxtran::Loop; run with: prove -I lib t/
#
# Each t/loop/<case>.F90 is a subroutine whose NPROMA loops and array syntax are
# removed by Fxtran::Loop::removeNpromaLoops, as for the singlecolumn transformation
# (this includes the SUM, COUNT and ANY idioms); the result is compared with
# t/loop/<case>.ref.F90, ignoring whitespace and blank lines.
#
# The style is ECPHYS (JL, KIDIA, KFDIA), unless t/loop/<case>.style holds another style name.
#
# Set LOOP_TEST_OUTPUT=<dir> to write the actual results in <dir>.

use strict;
use warnings;

use Test::More;
use FindBin qw ($Bin);
use FileHandle;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Loop;
use Fxtran::Style;

# Cases which the current code gets wrong (none at the moment)
my %TODO = map { ($_, 1) } qw ();

sub slurp
{
  my $f = shift;
  my $fh = 'FileHandle'->new ("<$f") or die ("Cannot open $f");
  local $/ = undef;
  return <$fh>;
}

# Like diff -B -w
sub normalize
{
  my $text = shift;
  return join ("\n", grep { length ($_) } map { (my $s = $_) =~ s/\s+//go; $s } split (m/\n/o, $text));
}

sub removeNpromaLoops
{
  my ($F90, $style) = @_;

  my $d = &Fxtran::parse (string => &slurp ($F90), fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);
  &Fxtran::Canonic::makeCanonic ($d);

  $style = 'Fxtran::Style'->new (style => $style, document => $d);

  for my $pu (&F ('.//program-unit', $d))
    {
      &Fxtran::Loop::removeNpromaLoops ($pu, style => $style);
    }

  return &Fxtran::Canonic::indent ($d);
}

my @case = map { m{/(\w+)\.F90$}o ? ($1) : () } sort grep { ! m/\.ref\.F90$/o } <$Bin/loop/*.F90>;

for my $case (@case)
  {
    local $TODO = $TODO{$case} ? 'not implemented yet' : undef;

    my $style = 'ECPHYS';
    chomp ($style = &slurp ("$Bin/loop/$case.style")) if (-f "$Bin/loop/$case.style");

    my $out = eval { &removeNpromaLoops ("$Bin/loop/$case.F90", $style) };
    my $err = $@;

    if (my $dir = $ENV{LOOP_TEST_OUTPUT})
      {
        my $fh = 'FileHandle'->new (">$dir/$case.F90");
        $fh->print (defined ($out) ? $out : "ERROR: $err");
      }

    if ($err)
      {
        fail ("$case: removeNpromaLoops");
        diag ($err);
      }
    elsif (! -f "$Bin/loop/$case.ref.F90")
      {
        fail ("$case: missing reference $case.ref.F90");
      }
    else
      {
        is (&normalize ($out), &normalize (&slurp ("$Bin/loop/$case.ref.F90")), "$case: result");
      }
  }

done_testing ();
