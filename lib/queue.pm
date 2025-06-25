package queue;

use threads;
use Thread::Queue;
use Data::Dumper;

use strict;

sub runCommand
{
  my %opts = @_;

  my $cmd = $opts{command};

  if ((scalar (@$cmd) == 3) && (! ref ($cmd->[0])) && (! ref ($cmd->[1])) && (ref ($cmd->[2]) eq 'ARRAY'))
    {
      my ($class, $method, $args) = @$cmd;
      print "Call $class->$method (" . join (', ', @$args) . ")\n" if ($opts{verbose});
      $class->$method (@$args);
    }
  else
    {
      print "@$cmd\n" if ($opts{verbose});
      system (@$cmd)
        and die ("Command `@$cmd' failed");
    }

}

sub init
{
  my $self = shift;

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
                     &runCommand (command => $cmd, verbose => $self->{verbose});
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
}

sub new
{
  my $class = shift;
  my %args = @_;

  my $self = bless \%args, $class;

  $self->{threads} ||= 1;

  return $self;
}

sub append
{
  my $self = shift;

  if ($self->{threads} > 1)
    {
      $self->{queue} or $self->init ();
      $self->{queue}->enqueue (@_);
    }
  else
    {
      for my $cmd (@_)
        {
          &runCommand (command => $cmd, verbose => $self->{verbose});
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

      delete $self->{queue};
      delete $self->{tasks};
    }
}

1;
