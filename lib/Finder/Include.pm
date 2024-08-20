package Finder::Include;

use strict;

sub new
{
  my $class = shift;
  my %args = @_;
  my $self = bless \%args, $class;
  for (@{ $self->{I} })
    {
      s/^-I//o;
    }
  return $self;
}

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  for my $I (@{ $self->{I} })
    {
      return "$I/$file" if (-f "$I/$file");
    }
}

1;
