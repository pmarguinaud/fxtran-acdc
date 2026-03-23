package Fxtran::Generate::Interface;

=head1 NAME

Fxtran::Generate::Interface

=head1 DESCRIPTION

Generate interface block for different transformation methods.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 SEE ALSO

L<Fxtran::Generate>, L<Fxtran::Interface>

=head1 COPYRIGHT

Meteo-France 2025

=cut

use FileHandle;
use Data::Dumper;
use Getopt::Long;
use File::stat;
use File::Path;
use File::Copy;
use File::Temp;
use File::Basename;
use Cwd;

use strict;

use click;

use Fxtran;
use Fxtran::Canonic;
use Fxtran::Util;
use Fxtran::Interface;

sub interface
{

=head2 interface

Generate the interface block for a given transformation method.  The
function writes the source document to a temporary F90 file prefixed with
the appropriate C<!$ACDC> directive, invokes C<fxtran-f90> in dry-run mode
to perform the transformation, then parses the resulting file, makes it
canonic, strips all assignment statements, and returns the text of the
interface block body.

=cut

  my ($doc, $opts, $method, @args) = @_;

  my $tmpdir = 'File::Temp'->newdir ();

  my $F90 = "$tmpdir/file.F90";
  my $fh = 'FileHandle'->new (">$F90");

  $fh->print ("!\$ACDC $method @args\n", $doc->textContent);

  $fh->close ();

  my @opts = 'click'->hashToCommandLine (method => $method, package => 'Fxtran::Generate', opts => $opts);

  my @cmd = ('fxtran-f90', @opts, '--dryrun', '--dir', $tmpdir, '--tmp', '.', '--', 'f90', '-c', $F90);

  &Fxtran::Util::runCommand (cmd => \@cmd);

  my ($F90_directive) = grep { $_ ne $F90 } <$tmpdir/*.F90>;

  $doc = &Fxtran::parse (location => $F90_directive, fopts => ['-construct-tag', '-no-include', '-line-length' => 500], dir => $opts->{tmp});

  &Fxtran::Canonic::makeCanonic ($doc, %$opts);

  &Fxtran::Interface::intfbBody ($doc);

  $_->unbindNode () for (&F ('.//a-stmt', $doc));

  (my $intfb = $doc->textContent ()) =~ s/^\s*\n$//goms;

  return $intfb;
}

1;
