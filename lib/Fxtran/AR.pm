package Fxtran::AR;

=head1 NAME

Fxtran::AR

=head1 DESCRIPTION

This module provides the C<expandObjects> function, which processes
a list of arguments.

Arguments with the C<.o> extension may actually be archives (if the C<.o>
file was created by C<fxtran-f90> with the C<--object-merge-method> option
set to C<archive>).

In this case, the archive is expanded into single objects and these objects
replace the item in the argument list.
=cut

use Data::Dumper;
use File::Spec;
use File::Type;
use File::Temp;
use Cwd;

use strict;

my $AR = '/usr/bin/ar';

sub sysAR
{

=head2 sysAR

Run the C<ar> archiver with the given arguments. If the first argument is C<t>,
returns the list of members in the archive. Otherwise, runs the command and
dies on failure.

=cut

  my @cmd = ($AR, @_);
  if ($cmd[1] eq 't')
    {
      my @obj = split (m/\n/o, `@cmd`);
      goto ERROR if ($?);
      return @obj;
    }
  else
    {
      system (@cmd)
        and goto ERROR;
    }

  return;

ERROR:
  die ("Command `@cmd' failed");
}

sub expandObjects
{

=head2 expandObjects

Expand C<.o> arguments that are actually archives (C<application/x-ar>) into
their individual object files. Non-archive C<.o> files and non-object arguments
are passed through unchanged. The C<$args> array ref is modified in place.

=cut

  my $args = shift;

  my $ft = 'File::Type'->new ();
  
  my @argv;
  
  my $dir = 'File::Temp'->newdir (CLEANUP => 1);
  
  my $cwd = &cwd ();
  
  for my $argv (@$args)
    {
  
      unless ($argv =~ m/\.o$/o)
        {
          push @argv, $argv;
          next;
        }
      
      my $obj = $argv;
  
      my $type = $ft->checktype_filename ($obj);
  
      if ($type eq 'application/x-executable-file')
        {
          push @argv, $obj;
        }
      elsif ($type eq 'application/x-ar')
        {
          $obj = 'File::Spec'->rel2abs ($obj);
  
          chdir ($dir);
  
          for (&sysAR ('t', $obj))
            {
              push @argv, 'Fxtran::AR::Object::Temp'->new (dir => $dir, file => $_);
            }
  
          &sysAR ('x', $obj);
  
          chdir ($cwd);
        }
      else   
        {
          die ("Unexpected type `$type' for object `$obj'");
        }
  
    }

  @$args = @argv;
}

package Fxtran::AR::Object::Temp;

use strict;

use overload '""' => \&asString;

sub new
{

=head2 new

Constructor for C<Fxtran::AR::Object::Temp>. Accepts C<dir> and C<file>
keyword arguments and returns a blessed hash reference.

=cut

  my $class = shift;
  return bless {@_}, $class;
}

sub asString
{

=head2 asString

Returns the full path of the temporary object file by joining C<dir> and
C<file>. Used as the string overload for C<Fxtran::AR::Object::Temp>.

=cut

  my $self = shift;
  return "$self->{dir}/$self->{file}";
}



=head1 SEE ALSO

L<fxtran-f90>, L<fxtran-cxx>, L<Fxtran::F90Compiler>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut
1;
