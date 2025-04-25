#!/usr/bin/perl -w

use strict;

use Data::Dumper;
use Getopt::Long;
use File::Path;
use File::Spec;
use FileHandle;
use List::MoreUtils qw (uniq);

use FindBin qw ($Bin);
use lib "$Bin/../lib";
use local::lib;

use fxtran;
use fxtran::xpath;

sub max { $_[0] < $_[1] ? $_[1] : $_[0] }

sub align
{
  my $doc = shift;

  my @decl = &F ('.//T-decl-stmt', $doc);
  
  my %len;
  my %att;
  
  for my $decl (@decl)
    {   
      my ($tspec) = &F ('./_T-spec_', $decl, 1); $tspec =~ s/\s+//go;
  
      $len{type} = &max ($len{type} || 0, length ($tspec));
  
      my @attr = &F ('.//attribute', $decl);
      for my $attr (@attr)
        {
          my ($N) = &F ('./attribute-N', $attr, 1); 
          $attr = $attr->textContent; $attr =~ s/\s+//go;
          $att{$N} = 1;
          $len{$N} = &max (($len{$N} || 0), length ($attr));
        }
    }   
  
  
  my @att = sort keys (%att);
  
  for (values (%len))
    {   
      $_++;
    }   
  
  for my $decl (@decl)
    {   
      my ($tspec) = &F ('./_T-spec_', $decl, 1); $tspec =~ s/\s+//go;
      my ($endlt) = &F ('./EN-decl-LT', $decl, 1); 
  
      my @attr = &F ('.//attribute', $decl);
      my %attr = map { my ($N) = &F ('./attribute-N', $_, 1); ($N, $_->textContent) } @attr;
  
      for (values (%attr))
        {
          s/\s+//go;
        }
  
      my $code = sprintf ("%-$len{type}s", $tspec);
  
      for my $att (@att)
        {
          if ($attr{$att})
            {
              $code .= sprintf (",%-$len{$att}s", $attr{$att});
            }
          else
            {
              $code .= ' ' x ($len{$att} + 1); 
            }
        }
  
      $code .= ' :: ' . $endlt;
  
      my $stmt = &fxtran::parse (statement => $code, fopts => [qw (-line-length 10000)]);
  
      $decl->replaceNode ($stmt);
  
    }   
}

sub w
{
  my ($file, $code) = @_;

  $code =~ s/^\s*//o;
  $code =~ s/\s*$//o;

  my $doc = &fxtran::parse (string => "$code\nEND", fopts => [qw (-line-length 10000 -canonic)]);

  &align ($doc);

  $code = $doc->textContent;

  $code =~ s/END\n$//o;

  my $fh = 'FileHandle'->new (">$file");
  $fh->print ($code);
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


unlink ('decl.txt');
system ('vim', '-c', 'startinsert', 'decl.txt');

my @line = do { my $fh = 'FileHandle'->new ("<decl.txt"); <$fh> };

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

&w ('decl0.txt', $code0);
&w ('decl1.txt', $code1);
&w ('decl2.txt', $code2);

system ('vim', '-d', 'decl0.txt', 'decl1.txt', 'decl2.txt');

my $code = &slurp ('decl0.txt');

print $code;
