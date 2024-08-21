package Finder;

use strict;
use Finder::Pack;
use Finder::Include;
use Finder::Files;

sub new
{
  my $class = shift;
  my %args = @_;

  if (-f '.gmkview')
    { 
      return 'Finder::Pack'->new (@_);
    }
  elsif ($args{files})
    {
      return 'Finder::Files'->new (@_);
    }
  else
    {
      return 'Finder::Include'->new (@_);
    }
}

1;
