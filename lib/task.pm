package task;

use strict;
use FileHandle;

my $count = 0;

sub new
{
  my $class = shift;
  my %args = @_;

  my $self = bless {id => $count++, @_}, $class;

  if ($args{debug}) 
    {
      my $bash = sprintf ('cmd.%3.3d.sh', $self->{id});

      'FileHandle'->new (">$bash")->print (<< "EOF");
#!/bin/bash

set -x

@{ $args{cmd} }

EOF

      chmod (0755, $bash);
    }

  return $self;
}

sub run
{
  my $self = shift;
  my %args = @_;

  my @cmd = @{ $self->{cmd} };

  $self->{mess} = `@cmd 2>&1`;
  $self->{code} = $?;

  if ($args{final}) 
    {
      $self->final ();
    }
}

sub final
{
  my $self = shift;

  print $self->{mess};

  if ($self->{code})
    {
      die ("Command `@{ $self->{cmd} }' failed");
    }
}

1;
