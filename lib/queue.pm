package queue;

use threads;
use Thread::Queue;

use strict;

sub new
{
  my $class = shift;
  my %args = @_;

  my $self = bless \%args, $class;

  $self->{threads} ||= 1;

  if ($self->{threads} > 1)
    {
       my @t;

       my $q = 'Thread::Queue'->new ();

       for my $tid (1 .. $self->{threads})
         {
           push @t, 'threads'->create (sub 
           {
             eval
               {
                 while (my $cmd = $q->dequeue ())
                   {
                     print "@$cmd\n";
                     system (@$cmd)
                       and die ("Command `@$cmd' failed");
                   }
               };

             if (my $c = $@)
               {
                 print "$c\n";
                 $q->insert (0, 0) for (1 .. $self->{threads});
                 return 0;
               }

             return 1;
           });
         }

      $self->{queue} = $q;
      $self->{tasks} = \@t;

    }

  return $self;
}

sub append
{
  my $self = shift;
  if ($self->{queue})
    {
      $self->{queue}->enqueue (@_);
    }
  else
    {
      for my $cmd (@_)
        {
          print "@$cmd\n";
          system (@$cmd)
            and die ("Command `@$cmd' failed\n");
        }
    }
}

sub flush
{
  my $self = shift;

  if ($self->{tasks})
    {

     for my $x ((0) x $self->{threads})
       {
         $self->{queue}->enqueue ($x);
       }


      my $c = 1;
      for my $t (@{ $self->{tasks} })
        {
          $c = $t->join () && $c;
        }

      $c or die;
    }
}

1;
