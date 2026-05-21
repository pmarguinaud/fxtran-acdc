package Fxtran::Finder::CMake;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Finder::CMake

=head1 DESCRIPTION

Finder implementation for CMake-based projects. Resolves source files by first
searching the include paths (via L<Fxtran::Finder::Include>) and then falling
back to a full recursive scan of the CMake home directory read from
C<CMakeCache.txt>. The environment variable C<CMAKE_BUILD_DIRECTORY> must point
to the CMake build directory. Directories containing a C<.fxtran_acdc_cmake_ignore>
file are excluded from the scan.

=head1 FUNCTIONS

=cut

use File::Find;
use File::Basename;
use File::Spec;
use FileHandle;
use Data::Dumper;
use FindBin qw ($Bin);

use strict;

use base qw (Fxtran::Finder::Basic);

use Fxtran::Finder::Include;

sub new
{

=head2 new

Constructor. Calls the parent constructor, then reads the
C<CMAKE_BUILD_DIRECTORY> environment variable (converting it to an absolute
path) and instantiates an L<Fxtran::Finder::Include> helper that will be tried
first on every C<resolve> call.

=cut

  my $class = shift;

  my $self = $class->SUPER::new (@_);

  $self->{cmake_build_directory} = $ENV{CMAKE_BUILD_DIRECTORY};
  $self->{cmake_build_directory} = 'File::Spec'->rel2abs ($self->{cmake_build_directory});

  $self->{finderinclude} = 'Fxtran::Finder::Include'->new (@_);


  return $self;
}

sub resolve
{

=head2 resolve

Resolves a filename (passed as the C<file> named argument) to an absolute
path. First delegates to L<Fxtran::Finder::Include> (which searches the
C<-I> include paths). If that fails, falls back to
C<resolveInCMakeHomeDirectory>. Returns the absolute path on success, or undef
if the file cannot be found.

=cut

  my $self = shift;

  my $r;

  if ($r = $self->{finderinclude}->resolve (@_))
    {
      return $r;
    }
  elsif ($r = $self->resolveInCMakeHomeDirectory (@_))
    {
      return $r;
    }
  elsif ($r = $self->resolveInCMakeBuildDirectory (@_))
    {
      return $r;
    }
}

sub resolveInCMakeHomeDirectory
{

=head2 resolveInCMakeHomeDirectory

Resolves a filename against the CMake home directory. On the first call,
parses C<CMakeCache.txt> to discover C<CMAKE_HOME_DIRECTORY> and then builds a
basename-to-path index by recursively scanning that directory (directories
containing a C<.fxtran_acdc_cmake_ignore> file are skipped). Subsequent calls
reuse the cached index. Dies if the file is found in more than one location.
Returns the absolute path, or undef if the file is not present in the tree.

=cut

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
      my $cmake_home_directory = $self->{cmake_home_directory};

      my @dir = ($cmake_home_directory); # main source directory

      my %seen; # seen symlinks (pointees)

      # but we may have some symlinks inside source directory...

      for my $d (<$cmake_home_directory/*>)
        {  
          next unless ((-l $d) && (-d "$d/")); # keep only symlinks

          my $ll = readlink ($d);   # pointee
          next if ($ll =~ m/^\w/o); # points inside source/
          next if ($seen{$ll}++);   # already registered

          push @dir, "$d/";
        }

      my %index;

      for my $dir (@dir)
        {
          &find ({
          wanted => sub 
          { 
            my $f = $File::Find::name; 
            return unless (-f $f); 
            $f = 'File::Spec'->rel2abs ($f, $dir); 
            push @{ $index{ &basename ($f) } }, $f;
          }, 
          preprocess => sub 
          {
            my $dir = $File::Find::dir;
            return -f "$File::Find::dir/.fxtran_acdc_cmake_ignore" ? () : @_;
          },
          no_chdir => 1,
          }, $dir);
        }

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

sub resolveInCMakeBuildDirectory
{
  my $self = shift;
  my %args = @_;

  my $file = $args{file};

  my $cmake_build_directory = $self->{cmake_build_directory};

  my %index;

  &find ({
  wanted => sub 
  { 
    my $f = $File::Find::name; 
    return unless (-f $f); 
    $f = 'File::Spec'->rel2abs ($f, $cmake_build_directory); 
    push @{ $index{ &basename ($f) } }, $f;
  }, 
  preprocess => sub 
  {
    my $dir = $File::Find::dir;
    return -f "$File::Find::dir/.fxtran_acdc_cmake_ignore" ? () : @_;
  },
  no_chdir => 1,
  }, $cmake_build_directory);

  my $r = $index{$file};

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

=head1 SEE ALSO

L<Fxtran::Finder>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
