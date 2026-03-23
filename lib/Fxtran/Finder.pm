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

=head2 new

Factory constructor.  Inspect the current environment and return the most
appropriate concrete finder object:

=over 4

=item *

C<Fxtran::Finder::CMake> when the C<CMAKE_BUILD_DIRECTORY> environment
variable is set.

=item *

C<Fxtran::Finder::Pack::Build> when the C<TARGET_PACK> environment
variable is set.

=item *

C<Fxtran::Finder::Pack> when a C<.gmkview> file exists in the current
directory (gmkpack layout).

=item *

C<Fxtran::Finder::Files> when an explicit C<files> list is provided.

=item *

C<Fxtran::Finder::Include> as the default fall-back.

=back

=cut

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


=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;
