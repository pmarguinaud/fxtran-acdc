package Fxtran::Bt;

=head1 NAME

Fxtran::Bt

=head1 DESCRIPTION

This module provides the C<bt> function and uses it to handle C<__WARN__> and
C<__DIE__> Perl signal. The C<bt> function will then print a stack trace 
and abort when an error or a warnings is encountered.

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2022

=cut

use Data::Dumper;
use FileHandle;
use Text::Wrap;

use strict;

my %CONTEXT;

sub showFrame
{
  my ($filename, $line, $subroutine, $args) = @_;

  return unless (defined ($filename) && defined ($line));

  my $width = 80;

  print '-' x $width, "\n";

  if ($subroutine && $args)
    {
      my @args = @$args;
      for (@args)
        {
          if (! defined ($_))
            {
              $_ = 'undef';
            }
          elsif (s/=SCALAR\(\w+\)$//o) { }
          elsif (s/=HASH\(\w+\)$//o) { }
          elsif (s/^HASH\(\w+\)$/{...}/o) { }
          elsif (s/^ARRAY\(\w+\)$/[...]/o) { }
          else
            {
              $_ = '"' . $_ . '"';
            }
        }
      my $text = "$subroutine (" . join (', ', @args) . ")";

      local $Text::Wrap::columns = 80;
      print "\n" . &wrap ('  ', '  ', $text), "\n\n";
    }

  print "$filename:$line\n";

  my $size = 2;

  unless (exists $CONTEXT{$filename})
    {
      my $fh = 'FileHandle'->new ("<$filename");
      $CONTEXT{$filename} = $fh ? [<$fh>] : undef;
    }


  $line = $line - 1;

  if (my $lines = $CONTEXT{$filename})
    { 
      my $imin = $line - $size > 0         ? $line - $size : 0;
      my $imax = $line + $size < $#{$lines}? $line + $size : $#{$lines};

      for my $i ($imin .. $imax)
        {
          printf ("%6d | %s", $i+1, $lines->[$i]);
        }
    }

  print "\n";
}

sub bt
{ 
  for (my $i = 0; ; $i++)
    {   
      last unless (my @call = caller ($i));
      return if ($call[3] eq '(eval)');
    }   

  print @_; 

  print "\n";

  eval "use Devel::StackTrace";

  my $c = $@; $c = 1;

  if ($c)
    {
      for (my $i = 0; ; $i++)
        {   
          last unless (my @call = caller ($i));
          print " $i ", $call[1], ':', $call[2], "\n";
        }   
    }
  else
    {
      my $trace = 'Devel::StackTrace'->new ();
      $trace->next_frame ();
      while (my $frame = $trace->next_frame ()) 
        {
          &showFrame (@{$frame}{qw (filename line subroutine args)});
        }
    }

  $DB::single = 1;

  die "\n";
}

$SIG{__WARN__} = \&bt;
$SIG{__DIE__} = \&bt;

1;
