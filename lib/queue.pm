package queue;

use strict;
use Data::Dumper;

use queue::serial;
use queue::threaded;

sub new
{
  my $class = shift; 
  my %args = @_;

  my $self = $args{nthread} > 1 
           ? 'queue::threaded'->new (%args)
           : 'queue::serial'->new (%args);

  return $self;
}

sub tail
{
  my $self = shift;
  my %args = @_;

  my $cb = $args{callback} || sub {};

  my @failed;

  while (my $o = $self->shift ()) 
    {
      if ($o->{code})
        {
          push @failed, $o;
          next;
        }
      $cb->($o);
    }

  if (@failed)
    {
      die &Dumper (\@failed);
    }

}

1;
