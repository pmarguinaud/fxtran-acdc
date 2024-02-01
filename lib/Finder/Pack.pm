package Finder::Pack;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use base qw (Finder::Basic);

use File::Spec;
use File::Find;
use Cwd;
use Data::Dumper;
use File::Basename;
use FileHandle;

# Index files in pack

sub scanpack
{
  my $self = shift;

  my $pack = $self->{pack};

  my @view = do { my $fh = 'FileHandle'->new ("<$pack/.gmkview"); <$fh> };
  chomp for (@view);
  @view = reverse (@view);

  my $scan;

  if (-f "$pack/.scan.pl")
    {
      # Read back existing scan
      $scan = do ("$pack/.scan.pl");

      # Rescan only local
      @view = ($view[-1]);

      # Delete files from local before scanning local again
      for my $f (keys (%$scan))
        { 
          if (index ($scan->{$f}, "$pack/src/$view[0]") == 0)
            {
              delete $scan->{$f};
            }
        }
    }

  $scan ||= {};

  for my $view (@view)
    {
      my $cwd = &cwd ();

      eval 
        {
          chdir ("$pack/src/$view");
          my $wanted = sub
            {
              my $f = $File::Find::name;
              return unless (-f $f);
              return unless (($f =~ m/\.F90$/o) || (($f =~ m/\.h/o)));
              $f =~ s,\.\/,,o;
              $scan->{&basename ($f)} = "$pack/src/$view/$f";
            };
          &find ({no_chdir => 1, wanted => $wanted}, '.');
        };
      my $c = $@;

      chdir ($cwd);
      $c && die ($c);
    }

  # Make it fast, cache results
  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Sortkeys = 1;
    'FileHandle'->new (">$pack/.scan.pl")->print (&Dumper ($scan));
  }


  $self->{scan} = $scan;

}

sub new
{
  my $class = shift;
  my $self = $class->SUPER::new (@_);

  $self->{pack} ||= '.';
  $self->{pack} = 'File::Spec'->rel2abs ($self->{pack});

  $self->scanpack ();

  return $self;
}

sub resolve
{
  my $self = shift;
  my %args = @_;
  my $file = $args{file};
  return $self->{scan}{$file};
}


1;
