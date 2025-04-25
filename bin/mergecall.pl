#!/usr/bin/perl -w

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FileHandle;

use FindBin qw ($Bin);
use lib "$Bin/../lib";
use local::lib;

use fxtran;
use fxtran::xpath;

sub w
{
  my ($file, $code) = @_;

  $code =~ s/^\s*//o;
  $code =~ s/\s*$//o;

  my $stmt = &parse (statement => $code, fopts => [qw (-line-length 10000 -canonic)]);

  my ($proc) = &F ('.//procedure-designator', $stmt);

  my @arg = &F ('.//arg', $stmt);

  my $fh = 'FileHandle'->new (">$file");

  $fh->print ("CALL " . $proc->textContent . " (\n");
  
  for my $i (0 .. $#arg)
    {
      my $arg = $arg[$i];
      $fh->print ($arg->textContent);
      $fh->print (",") unless ($i == $#arg);
      $fh->print ("\n");
    }

  $fh->print (")\n");

  $fh->close ();
}

sub slurp
{
  my $f = shift;
  die unless (-f $f);
  return do { local $/ = undef; my $fh = 'FileHandle'->new ("<$f"); <$fh> };
}

sub indent
{
  my ($code, $indent) = @_;

  for ($code)
    {
      s/\n//goms;
      s/^\s*//o;
      s/\s*$//o;
    }

  my $len = -1;
  my $max = 120;
  my $ind = $indent;
  my $pp = sub
  {
    if ($len < 0)
      {
        print $ind;
        $len = 0;
      }
    if ($len + length ($_[0]) > $max)
      {
        print "&\n$ind  & ";
        $len = 0;
      }

    print $_[0];
    $len += length ($_[0]);
  };

  my $stmt = &parse (statement => $code, fopts => [qw (-line-length 10000 -canonic)]);

  my ($proc) = &F ('.//procedure-designator', $stmt);
  my @arg = &F ('.//arg', $stmt);

  $pp->("CALL " . $proc->textContent . " (");
 
  for my $i (0 .. $#arg)
    {
      my $arg = $arg[$i]->textContent;
      $arg = "$arg, " unless ($i == $#arg);
      $pp->($arg);
    }

  $pp->(")");
  print "\n";

}


unlink ('call.txt');
system ('vim', '-c', 'startinsert', 'call.txt');

my @line = do { my $fh = 'FileHandle'->new ("<call.txt"); <$fh> };

my (@line0, @line1, @line2);

my $where = 0b111;

for my $line (@line)
  {
    chomp ($line);
    if (index ($line, '<<<<<<<') == 0)
      {
        $where = 0b010; next;
      }
    elsif (index ($line, '|||||||') == 0)
      {
        $where = 0b001; next;
      }
    elsif (index ($line, '=======') == 0)
      {
        $where = 0b100; next;
      }
    elsif (index ($line, '>>>>>>>') == 0)
      {
        $where = 0b111; next;
      }
    push @line0, "$line\n" if ($where & 0b001);
    push @line1, "$line\n" if ($where & 0b010);
    push @line2, "$line\n" if ($where & 0b100);
  }

my $code0 = join ('', @line0);
my $code1 = join ('', @line1);
my $code2 = join ('', @line2);

my ($indent) = ($code1 =~ m/^(\s*)/o);
$indent =~ s/^\s*\n//o;

&w ('call0.txt', $code0);
&w ('call1.txt', $code1);
&w ('call2.txt', $code2);

system ('vim', '-d', 'call0.txt', 'call1.txt', 'call2.txt');

my $code = &slurp ('call0.txt');

&indent ($code, $indent);

