package Fxtran::Finder::Pack;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use base qw (Fxtran::Finder::Basic);

use File::Spec;
use File::Find;
use Cwd;
use Data::Dumper;
use File::Basename;
use FileHandle;

sub scanView
{
  my $self = shift;

  my $pack = $self->{pack};

  my ($scan, $view) = @_;

  my $wanted = sub
    {
      my $f = $File::Find::name;
      return unless (-f $f);
      return unless (($f =~ m/\.F90$/o) || (($f =~ m/\.h/o)));
      $f =~ s,\.\/,,o;
      $scan->{&basename ($f)} = $f;
    };

  if (-d "$pack/src/$view/")
    {
      &find ({no_chdir => 1, wanted => $wanted}, "$pack/src/$view/");
    }
}

# Index files in pack

sub scanpack
{
  my $self = shift;

  my $pack = $self->{pack};

  my @view = do { my $fh = 'FileHandle'->new ("<$pack/.gmkview"); <$fh> };
  chomp for (@view);

  my $local = shift (@view);

  @view = reverse (@view);

  my $scan = {};

  if (-f "$pack/.scan.pl")
    {
      # Read back existing scan
      $scan = do ("$pack/.scan.pl");
    }
  else
   {
     for my $view (@view)
       {
         $self->scanView ($scan, $view);
       }
      local $Data::Dumper::Terse = 1;
      local $Data::Dumper::Sortkeys = 1;
      'FileHandle'->new (">$pack/.scan.pl")->print (&Dumper ($scan));
    }

  $self->{scan} = $scan;

  $self->scanView ($scan, $local);
}

sub new
{
  my $class = shift;
  my $self = $class->SUPER::new (@_);

  $self->{pack} ||= '.';
  $self->{pack} = 'File::Spec'->rel2abs ($self->{pack});


  return $self;
}

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};

  $self->scanpack () unless ($self->{scan});

  return $self->{scan}{$file};
}


1;
