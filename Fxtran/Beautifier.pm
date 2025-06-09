package Fxtran::Beautifier;

use Data::Dumper;
use FileHandle;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

use strict;

my $log;

sub ll
{
  $log ||= 'FileHandle'->new (">>fxtran-mergetool.txt");

  my @call = caller (0);
  $log->print ("$call[1]:$call[2]\n");
  $log->print (@_);
  $log->print ("\n" x 2);
}

sub debugCommand
{
  my @cmd = @_;

  'FileHandle'->new ('>cmd.sh')->print (<< "EOF");
#!/bin/bash

set -x

exec @cmd

EOF
  chmod (0755, 'cmd.sh');
  system ('xterm');
}

sub runCommand
{
  my %args = @_;

  my @cmd = @{ $args{cmd} };

  if (system (@cmd))
    {
      die unless ($args{debug});

      &debugCommand (@cmd);

      system (@cmd)
        and die;
    }
}

sub getIndent
{
  my $stmt = shift;

  $stmt or die;

  my $n = $stmt->previousSibling;

  unless ($n) 
    {    
      if ($stmt->parentNode)
        {
          return &getIndent ($stmt->parentNode);
        }
      return 0;
    }    

  if (($n->nodeName eq '#text') && ($n->data =~ m/\n/o))
    {    
      (my $t = $n->data) =~ s/^.*\n//gsmo;
      return length ($t);
    }    

  if (my $m = $n->lastChild)
    {
      if (($m->nodeName eq '#text') && ($m->data =~ m/\n/o))
        {    
          (my $t = $m->data) =~ s/^.*\n//gsmo;
          return length ($t);
        }    
      return &getIndent ($m);
    }
  elsif (($n->nodeName eq '#text') && ($n->data =~ m/^\s*$/o) && $n->parentNode)
    {
      return length ($n->data) + &getIndent ($n->parentNode);
    }

  return 0;
}

sub getDocument
{
  my $f = shift;

  my @fopts = qw (-line-length 1000 -no-cpp -no-include);

  &runCommand (cmd => ['fxtran', @fopts, $f], debug => 1);

  return 'XML::LibXML'->load_xml (location => "$f.xml");
}

my %class;

sub class
{
  my $stmt = shift;

  (my $class = $stmt->nodeName) =~ s/-stmt$//o; 

  $class = join ('::', 'Fxtran', 'Beautifier', map { ucfirst ($_) } split (m/-/o, $class));

  unless (exists $class{$class})
    {
      (my $pm = $class) =~ s,::,/,go; $pm .= '.pm';
      eval "use $class;";

      &ll (&Dumper ([$class, $pm]));

      if (my $c = $@)
        {
          if (index ($c, "Can't locate $pm ") == 0)
            {
              $class{$class} = undef;
            }
          else
            {
              die $c;
            }
        }
      else
        {
          $class{$class} = $class;
        }
    }

  return $class{$class};
}

sub expandFile
{
  my ($f) = @_;

  my $d = &getDocument ($f);

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      next unless (my $class = &class ($stmt));
    
      my $indent = ' ' x &getIndent ($stmt);

      my $expandStmt = $class->expand ($stmt, $indent);

      $stmt->replaceNode ($expandStmt);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);
}

sub repackFile
{
  my ($f) = @_;

  my $d = &getDocument ($f);

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      next unless (my $class = &class ($stmt));
    
      my $indent = ' ' x &getIndent ($stmt);

      my $canonicStmt = $class->canonic ($stmt);
      my $repackStmt = $class->repack ($canonicStmt, $indent);

      $stmt->replaceNode ($repackStmt);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);
}

sub repackCallLikeStatement
{
  my $class = shift;

  my $prefix = shift (@_);
  my $indent = pop (@_);
  my $suffix = pop (@_);
  my @arg = @_;

  my $len = 0; 
  my $max = 120;
  my $ind = $indent;
  my $str = '';

  my $pp = sub 
  {
    if ($len + length ($_[0]) > $max)
      {   
        $str .= "&\n$ind  & ";
        $len = 0;
      }   

    $str .= $_[0];
    $len += length ($_[0]);
  };  

  $pp->($prefix);
 
  for my $i (0 .. $#arg)
    {   
      my $arg = $arg[$i];
      $arg = "$arg, " unless ($i == $#arg);
      $pp->($arg);
    }   

  $pp->($suffix);

  return $class->reparse ($str);
}

1;
