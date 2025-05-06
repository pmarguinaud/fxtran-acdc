package Intrinsic;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;

my @INTRINSIC = qw (SIGN MAX MIN MOD REAL EXP ASIN FOLH SQRT PRESENT INT NULL
                    ABS TINY SUM ATAN2 COS SIN NINT MERGE SIZE TRIM COUNT FLOAT);
my %INTRINSIC = map { ($_, 1) } @INTRINSIC;

sub isIntrinsic
{
  my $s = shift;
  return $INTRINSIC{$s};
}

1;
