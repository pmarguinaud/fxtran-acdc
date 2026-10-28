#!/usr/bin/env perl

# Unit tests for Fxtran::Canonic; run with: prove -I lib t/

use strict;
use warnings;

use Test::More;

use Fxtran;
use Fxtran::Canonic;

sub canonic
{
  my $text = shift;
  my $d = &Fxtran::parse (string => $text, fopts => [qw (-construct-tag -line-length 512 -canonic -no-include)]);
  &Fxtran::Canonic::makeCanonic ($d);
  my ($pu) = &F ('./object/file/program-unit', $d);
  return $pu;
}

# Names of the element children of a program unit
sub layout
{
  my $pu = shift;
  return join (' ', map { $_->nodeName } grep { $_->nodeName ne '#text' } $pu->childNodes ());
}

# Executable statements of the execution part, one per line, without whitespace
sub execution
{
  my $pu = shift;
  my ($ep) = &F ('./execution-part', $pu);
  return join ("\n", map { (my $s = $_->textContent) =~ s/\s+//go; $s } &F ('./ANY-stmt|./ANY-construct', $ep));
}

my $layout = 'subroutine-stmt specification-part execution-part contains-stmt program-unit end-subroutine-stmt';

# Contained subroutine, with DR_HOOK
{
  my $pu = &canonic (<< 'EOF');
SUBROUTINE HOOK (A)
REAL, INTENT (INOUT) :: A
IF (LHOOK) CALL DR_HOOK ('HOOK', 0, ZHOOK_HANDLE)
CALL S (A)
IF (LHOOK) CALL DR_HOOK ('HOOK', 1, ZHOOK_HANDLE)
CONTAINS
SUBROUTINE S (A)
REAL, INTENT (INOUT) :: A
A = 2. * A
END SUBROUTINE
END SUBROUTINE
EOF
  is (&layout ($pu), $layout, 'DR_HOOK: CONTAINS outside of the execution part');
}

# Contained subroutine, no DR_HOOK
{
  my $pu = &canonic (<< 'EOF');
SUBROUTINE NOHOOK (A)
REAL, INTENT (INOUT) :: A
CALL S (A)
A = A + 1.
CONTAINS
SUBROUTINE S (A)
REAL, INTENT (INOUT) :: A
A = 2. * A
END SUBROUTINE
END SUBROUTINE
EOF
  is (&layout ($pu), $layout, 'no DR_HOOK: CONTAINS outside of the execution part');
  is (&execution ($pu), "CALLS(A)\nA=A+1.", 'no DR_HOOK: execution part');
  my ($s) = &F ('./program-unit', $pu);
  is (&execution ($s), 'A=2.*A', 'no DR_HOOK: execution part of the contained subroutine');
}

# Contained subroutine, no DR_HOOK, no executable statement in the host
{
  my $pu = &canonic (<< 'EOF');
SUBROUTINE NOEXEC (A)
REAL, INTENT (INOUT) :: A
CONTAINS
SUBROUTINE S (A)
REAL, INTENT (INOUT) :: A
A = 2. * A
END SUBROUTINE
END SUBROUTINE
EOF
  is (&layout ($pu), $layout, 'no executable statement: CONTAINS outside of the execution part');
  is (&execution ($pu), '', 'no executable statement: empty execution part');
}

done_testing ();
