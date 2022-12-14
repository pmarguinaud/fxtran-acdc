package Intrinsic;

use strict;

my @INTRINSIC = qw (SIGN MAX MIN MOD REAL EXP ASIN FOLH SQRT PRESENT 
                    ABS TINY SUM ATAN2 COS SIN);
my %INTRINSIC = map { ($_, 1) } @INTRINSIC;

sub isIntrinsic
{
  my $s = shift;
  return $INTRINSIC{$s};
}

1;
