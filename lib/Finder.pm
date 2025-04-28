package Finder;

use strict;
use Finder::Pack;
use Finder::Pack::Build;
use Finder::Include;
use Finder::Files;

sub new
{
  my $class = shift;

  my %args;

  if ((scalar (@_) % 2) == 0)
    {
      %args = @_;
    }

  if ($ENV{TARGET_PACK})
    {
      return 'Finder::Pack::Build'->new (@_);
    }
  elsif (-f '.gmkview')
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
