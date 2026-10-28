#!/usr/bin/env perl

# Unit tests for Fxtran::Inline; run with: prove -I lib t/
#
# Each t/inline/<case>.F90 is a host subroutine with CONTAINed routines;
# they are inlined as with fxtran-f90 --inline-contained, and the result is
# compared with t/inline/<case>.ref.F90, ignoring whitespace and blank lines.
# If t/inline/<case>.die exists, inlining must fail instead, with a message
# matching the regular expression in this file.
#
# Set INLINE_TEST_OUTPUT=<dir> to write the actual results in <dir>.

use strict;
use warnings;

use Test::More;
use FindBin qw ($Bin);
use FileHandle;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Inline;
use Fxtran::Style;

# Cases which the current code gets wrong; renaming of local variables (r12) is not
# implemented yet
my %TODO = map { ($_, 1) } qw (r12_clash);

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

sub inline
{
  my $F90 = shift;

  my $d = &Fxtran::parse (string => &slurp ($F90), fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);
  &Fxtran::Canonic::makeCanonic ($d);

  my $style = 'Fxtran::Style'->new (style => 'NONE', document => $d);

  for my $pu (&F ('.//program-unit', $d))
    {
      next unless ($pu->firstChild->nodeName eq 'subroutine-stmt');
      &Fxtran::Inline::inlineContainedSubroutines ($pu, inlineDeclarations => 1, style => $style);
    }

  return &Fxtran::Canonic::indent ($d);
}

my @case = map { m{/(\w+)\.F90$}o ? ($1) : () } sort grep { ! m/\.ref\.F90$/o } <$Bin/inline/*.F90>;

for my $case (@case)
  {
    local $TODO = $TODO{$case} ? 'not implemented yet' : undef;

    my $out = eval { &inline ("$Bin/inline/$case.F90") };
    my $err = $@;

    if (my $dir = $ENV{INLINE_TEST_OUTPUT})
      {
        my $fh = 'FileHandle'->new (">$dir/$case.F90");
        $fh->print (defined ($out) ? $out : "ERROR: $err");
      }

    if (-f "$Bin/inline/$case.die")
      {
        chomp (my $re = &slurp ("$Bin/inline/$case.die"));
        like ($err, qr/$re/, "$case: dies");
      }
    elsif ($err)
      {
        fail ("$case: inlining");
        diag ($err);
      }
    elsif (! -f "$Bin/inline/$case.ref.F90")
      {
        fail ("$case: missing reference $case.ref.F90");
      }
    else
      {
        is (&normalize ($out), &normalize (&slurp ("$Bin/inline/$case.ref.F90")), "$case: result");
      }
  }

done_testing ();
