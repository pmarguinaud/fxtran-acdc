package Fxtran::Tool;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

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
  $log ||= 'FileHandle'->new (">>fxtran-tool.txt");

  my @call = caller (0);
  $log->print ("$call[1]:$call[2]\n");
  $log->print (@_);
  $log->print ("\n" x 2);
}

sub debugCommand
{
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
  my $prog = shift;
  for my $path (split (m/:/o, $ENV{PATH}))
    {
      my $p = "$path/$prog";
      return $p if ((-f $p) && (-x $p));
    }
}

1;
