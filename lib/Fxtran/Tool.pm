package Fxtran::Tool;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Tool

=head1 DESCRIPTION

General-purpose helper utilities for the fxtran-acdc toolchain.  Provides a
simple logging function (C<ll>) that writes caller location and message to a
log file, C<runCommand> to execute a shell command and die on failure (with an
optional interactive debug mode that opens an xterm), and C<which> to locate
an executable in C<$PATH>.

=head1 FUNCTIONS

=cut

use Data::Dumper;
use FileHandle;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

use strict;

use Fxtran::Formatter;
use Fxtran::MergeTool;

my $log;

sub ll
{

=head2 ll

Append the current file/line location and a message to C<fxtran-tool.txt>,
opening the log file on first use.

=cut

  $log ||= 'FileHandle'->new (">>fxtran-tool.txt");

  my @call = caller (0);
  $log->print ("$call[1]:$call[2]\n");
  $log->print (@_);
  $log->print ("\n" x 2);
}

sub debugCommand
{

=head2 debugCommand

Open an interactive xterm with the failing command pre-aliased, letting the
developer re-run or edit the offending file in an interactive shell session.

=cut

  use File::Temp;
  my %args = @_;

  my @cmd = @{ $args{cmd} };
  my $file = $args{file} || '';

  my $bashrc = 'File::Temp'->new (UNLINK => 1, SUFFIX => '.sh');

  $bashrc->print (<< "EOF");

alias r="echo \\"@cmd\\";  @cmd"

alias e="vi $file"

echo "Command \'@cmd\' fails:"
echo
@cmd
echo
echo "Type r for running command '@cmd'"
echo
if [ "x$file" != "x" ]
then
echo "Type e for editing '$file'"
fi
echo

export PS1='debug> '

EOF

  $bashrc->flush ();

  system (qw (xterm -e bash --init-file), $bashrc, '-i');
}

sub runCommand
{

=head2 runCommand

Execute a shell command and die with a descriptive message on failure.  When
C<debug> is set, opens a C<debugCommand> xterm before retrying once.

=cut

  my %args = @_;

  my @cmd = @{ $args{cmd} };

  if (system (@cmd))
    {
      goto FAILED unless ($args{debug});

      &debugCommand (%args);

      system (@cmd)
        and goto FAILED;
    }

  return;
FAILED:
  die ("Command `@cmd' failed");
}

sub which
{

=head2 which

Search C<$PATH> for C<$prog> and return the first executable path found, or
C<undef> if not found.

=cut

  my $prog = shift;
  for my $path (split (m/:/o, $ENV{PATH}))
    {
      my $p = "$path/$prog";
      return $p if ((-f $p) && (-x $p));
    }
}

1;
