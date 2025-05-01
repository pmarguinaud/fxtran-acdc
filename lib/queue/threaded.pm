package queue::threaded;

use strict;
use base qw (queue);

use threads;
use Thread::Queue;
use Storable;
use Data::Dumper;

sub run
{
  my ($qi, $qo) = @_;
  
  while (my $t = $qi->dequeue ()) 
    { 
      if ($t eq 'STOP')
        {
          $qo->enqueue ('STOP');
          return;
        }
  
      $t = &Storable::thaw ($t);
  
      $t->run ();
  
      $qo->enqueue (&Storable::freeze ($t)); 
    }
}

sub new
{
  my $class = shift;

  my $self = bless {@_}, $class;

  $self->{nthread} ||= 4;

  return $self;
}

sub create
{
  my $self = shift;

  my $qi = 'Thread::Queue'->new ();
  my $qo = 'Thread::Queue'->new ();

  @{$self}{qw (qi qo)} = ($qi, $qo);

  for my $i (1 .. $self->{nthread})
    {
      push @{ $self->{threads} }, 
       'threads'->create (sub { &run ($qi, $qo) }); 
    }

  return $self;
}

sub push : method
{
  my $self = shift;
  $self->create () unless ($self->{qi});
  $self->{count}++;
  $self->{qi}->enqueue (map { &Storable::freeze ($_) } @_);
}

sub shift : method
{
  my $self = shift;
  return unless ($self->{count});
  $self->{count}--;

  my $qo = $self->{qo};
  my $o = $qo->dequeue ();
  return &Storable::thaw ($o);
}

sub finish
{
  my $self = shift;

  if ((my $qi = delete $self->{qi}) && (my $qo = delete $self->{qo}))
    {
     
      for (@{ $self->{threads} })
        {
          $qi->enqueue ('STOP');
        }
     
      for my $t (@{ $self->{threads} })
        {
          $t->join ();
        }
     
      my $n = $self->{nthread};
     
      while (defined (my $o = $qo->dequeue ()))
        { 
          if ($o eq 'STOP')
            {
              $n--;
              last unless ($n);
              next;
            }
        }
      
    }

  %$self = ();
}

1;
