#!/usr/bin/env perl

# Unit tests for Fxtran::Construct; run with: prove -I lib t/
#
# Each t/construct/<case>.F90 is a subroutine; Fxtran::Construct::apply is called on it,
# with the variables LT and LF forced to .TRUE. and .FALSE., and the result is compared
# with t/construct/<case>.ref.F90, ignoring whitespace and blank lines.
#
# Set CONSTRUCT_TEST_OUTPUT=<dir> to write the actual results in <dir>.

use strict;
use warnings;

use Test::More;
use FindBin qw ($Bin);
use FileHandle;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Construct;

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

sub apply
{
  my $F90 = shift;

  my $d = &Fxtran::parse (string => &slurp ($F90), fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);

  for my $pu (&F ('.//program-unit', $d))
    {
      &Fxtran::Construct::apply
      (
        $pu,
        './/named-E[string(N)="LT"]' => &e ('.TRUE.'),
        './/named-E[string(N)="LF"]' => &e ('.FALSE.'),
      );
    }

  return &Fxtran::Canonic::indent ($d);
}

my @case = map { m{/(\w+)\.F90$}o ? ($1) : () } sort grep { ! m/\.ref\.F90$/o } <$Bin/construct/*.F90>;

for my $case (@case)
  {
    local $TODO = $TODO{$case} ? 'not implemented yet' : undef;

    my $out = eval { &apply ("$Bin/construct/$case.F90") };
    my $err = $@;

    if (my $dir = $ENV{CONSTRUCT_TEST_OUTPUT})
      {
        my $fh = 'FileHandle'->new (">$dir/$case.F90");
        $fh->print (defined ($out) ? $out : "ERROR: $err");
      }

    if ($err)
      {
        fail ("$case: apply");
        diag ($err);
      }
    elsif (! -f "$Bin/construct/$case.ref.F90")
      {
        fail ("$case: missing reference $case.ref.F90");
      }
    else
      {
        is (&normalize ($out), &normalize (&slurp ("$Bin/construct/$case.ref.F90")), "$case: result");
      }
  }

done_testing ();
