package PATH;

use strict;
use File::Basename;

my $TOP = $INC{__PACKAGE__ . '.pm'};

for (1 .. 2)
  {
    $TOP = &dirname ($TOP);
  }

$ENV{PATH} = "$TOP/bin:$ENV{PATH}";



1;
