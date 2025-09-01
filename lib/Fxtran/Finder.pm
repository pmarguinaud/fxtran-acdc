package Fxtran::Finder;

=head1 NAME

Fxtran::Finder

=head1 DESCRIPTION

This is a factory class for all finder objects; the C<new> method
picks the right finder class from the context.

A finder object is responsible for finding files in the project
being compiled. It provides the following methods:

=over 4

=item resolve

Return a full path name given a filename.

=item getInterface

Return the full path of an interface from a subroutine name.

=back

We currently provide concrete classes for the following environments:

=over 4

=item 

cmake

=item 

gmkpack

=item 

A simple directory containing all source files.

=back

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran::Finder::Pack;
use Fxtran::Finder::Pack::Build;
use Fxtran::Finder::Include;
use Fxtran::Finder::Files;
use Fxtran::Finder::CMake;

sub new
{
  my $class = shift;

  my %args;

  if ((scalar (@_) % 2) == 0)
    {
      %args = @_;
    }

  if ($ENV{CMAKE_BUILD_DIRECTORY})
    {
      return 'Fxtran::Finder::CMake'->new (@_);
    }
  elsif ($ENV{TARGET_PACK})
    {
      return 'Fxtran::Finder::Pack::Build'->new (@_);
    }
  elsif (-f '.gmkview')
    { 
      return 'Fxtran::Finder::Pack'->new (@_);
    }
  elsif ($args{files} && @{ $args{files} })
    {
      return 'Fxtran::Finder::Files'->new (@_);
    }
  else
    {
      return 'Fxtran::Finder::Include'->new (@_);
    }
}

1;
