package Finder;

use strict;
use Finder::Pack;
use Finder::Include;

sub new
{
  my $class = shift;
  if (-f '.gmkview')
    { 
      return 'Finder::Pack'->new (@_);
    }
  else
    {
      return 'Finder::Include'->new (@_);
    }
}

1;
