package Fxtran::Finder::CMake;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use File::Find;
use File::Basename;
use File::Spec;
use FileHandle;
use Data::Dumper;

use strict;

use base qw (Fxtran::Finder::Basic);

use Fxtran::Finder::Include;

sub new
{
  my $class = shift;

  my $self = $class->SUPER::new (@_);

  $self->{cmake_build_directory} = $ENV{CMAKE_BUILD_DIRECTORY};
  $self->{cmake_build_directory} = 'File::Spec'->rel2abs ($self->{cmake_build_directory});

  $self->{finderinclude} = 'Fxtran::Finder::Include'->new (@_);


  return $self;
}

sub resolve
{
  my $self = shift;

  my $r;

  if ($r = $self->{finderinclude}->resolve (@_))
    {
      return $r;
    }
  else
    {
      return $self->resolveInCMakeHomeDirectory (@_);
    }
}

sub resolveInCMakeHomeDirectory
{
  my $self = shift;
  my %args = @_;

  my $file = $args{file};

  unless ($self->{cmake_home_directory})
    {
      my $CMakeCache = "$self->{cmake_build_directory}/CMakeCache.txt";
      (my $fh = 'FileHandle'->new ("<$CMakeCache")) 
        or die ("$CMakeCache was not found");

      my @text = <$fh>;
      ($self->{cmake_home_directory}) = map { m/^CMAKE_HOME_DIRECTORY:INTERNAL=(\S+)$/o ? ($1) : () } @text;
    }

  unless ($self->{cmake_home_directory_index})
    {
      my %index;
      my $dir = $self->{cmake_home_directory};

      &find ({wanted => sub 
      { 
        my $f = $File::Find::name; 
        return unless (-f $f); 
        $f = 'File::Spec'->rel2abs ($f, $dir); 
        push @{ $index{ &basename ($f) } }, $f;
      }, no_chdir => 1}, $dir);
      $self->{cmake_home_directory_index} = \%index;
    }

  my $r = $self->{cmake_home_directory_index}{$file};

  return unless ($r);

  if (scalar (@$r) > 1)
    {
      die (sprintf ("File `%s' was found in %s locations:\n - ", 
           $file, scalar (@$r)) . join ("\n - ", @$r) . "\n");
    }
  elsif (scalar (@$r) == 1)
    {
      return $r->[0];
    }

}

1;
