package queue::serial;

use strict;
use base qw (queue);

use Data::Dumper;

sub new
{
  my $class = shift;
  my $self = bless {@_}, $class;
  $self->{res} = [];
  return $self;
}

sub push : method
{
  my $self = shift;
  for (@_)
    {
      $_->run ();
      push @{ $self->{res} }, $_;
    }
}

sub shift : method
{
  my $self = shift;
  return shift (@{ $self->{res} });
}

sub finish
{

}


1;
