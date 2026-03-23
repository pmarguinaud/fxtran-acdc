package Fxtran::Finder::Pack;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::Pack

=head1 DESCRIPTION

Finder implementation for gmkpack-style source packs. A pack is a directory
tree whose layout is described by a C<.gmkview> file listing the views in
priority order. All C<.F90> and C<.h> files across every view are indexed by
basename. The index is cached in a C<.scan.pl> file inside the pack directory
so that subsequent runs do not need to re-scan the tree. The local (first)
view is always scanned afresh to pick up any recent edits.

=head1 FUNCTIONS

=cut

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

=head2 scanView

Populates (or updates) a scan hash-ref with the C<.F90> and C<.h> files found
under C<pack/src/view/>. Each entry maps the file's basename to its absolute
path. Called internally by C<scanpack> for each view in the pack.

=cut

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

=head2 scanpack

Builds the full basename-to-path index for the gmkpack pack. Reads the view
list from C<.gmkview>, treats the first entry as the local (mutable) view, and
reverses the remaining views so that higher-priority views overwrite
lower-priority ones in the index. Non-local views are loaded from a
C<.scan.pl> cache file if it exists; otherwise they are scanned and the cache
is written. The local view is always rescanned to capture recent edits. Stores
the result in C<$self-E<gt>{scan}>.

=cut

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

=head2 new

Constructor. Calls the parent constructor, then resolves the C<pack> attribute
to an absolute path (defaulting to the current directory C<.> if not
provided).

=cut

  my $class = shift;
  my $self = $class->SUPER::new (@_);

  $self->{pack} ||= '.';
  $self->{pack} = 'File::Spec'->rel2abs ($self->{pack});


  return $self;
}

sub resolve
{

=head2 resolve

Resolves a filename (C<file> named argument) to its absolute path within the
pack. Triggers C<scanpack> on the first call to populate the index. Returns
the path if found in the index, or undef otherwise.

=cut

  my $self = shift;
  my %args = @_;
  my $file = $args{file};

  $self->scanpack () unless ($self->{scan});

  return $self->{scan}{$file};
}


1;
