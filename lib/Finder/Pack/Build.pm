package Finder::Pack::Build;

use strict;
use base qw (Finder::Basic);

use Finder::Pack;
use Finder::Include;

sub new
{
  my $class = shift;

  my $self = $class->SUPER::new (@_);

  $self->{pack} = $ENV{TARGET_PACK};
  $self->{pack} = 'File::Spec'->rel2abs ($self->{pack});

  $self->{finderpack} = 'Finder::Pack'->new (pack => $self->{pack});

  $self->{includepack} = 'Finder::Include'->new (@_);

  return $self;
}

sub resolve
{
  my $self = shift;

  my $r;

  if ($r = $self->{includepack}->resolve (@_))
    {
      return $r;
    }
  elsif ($r = $self->{finderpack}->resolve (@_))
    {
      return $r;
    }
}

1;
