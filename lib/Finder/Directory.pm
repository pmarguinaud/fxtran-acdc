package Finder::Directory;

use strict;
use base qw (Finder::Basic);

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{directory} . '/' . $file;
}


1;
