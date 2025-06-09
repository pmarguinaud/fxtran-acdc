package Fxtran::Beautifier;

use Data::Dumper;
use FileHandle;

use fxtran;
use fxtran::parser;
use fxtran::xpath;

use strict;

use Fxtran::Beautifier::Call;
use Fxtran::Beautifier::Associate;

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

sub expand
{
  my ($f) = @_;

  my $d = &getDocument ($f);

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      my $indent = ' ' x &getIndent ($stmt);

      my $expandStmt;

      if ($stmt->nodeName eq 'call-stmt')
        {
          $expandStmt = &Fxtran::Beautifier::Call::expand ($stmt, $indent);
        }
      elsif ($stmt->nodeName eq 'associate-stmt')
        {
          $expandStmt = &Fxtran::Beautifier::Associate::expand ($stmt, $indent);
        }
      else
        {
          next;
        }

      $stmt->replaceNode ($expandStmt);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);
}

sub repackCallStatement
{
  my ($code, $indent) = @_; 

  for ($code)
    {   
      s/\n//goms;
      s/^\s*//o;
      s/\s*$//o;
    }   

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

  return $str;
}

sub repackCallStatements
{
  my $f = shift;

  my $d = &getDocument ($f);

  eval 
    {
      for my $call (&F ('.//call-stmt', $d))
        {
          my $indent = ' ' x &getIndent ($call);
     
          my $code = &repackCallStatement ($call->textContent, $indent);

          my $stmt = &s ($code);
     
          $call->replaceNode ($stmt);
        }
    };

  if (my $c = $@)
    {
      &ll ($c);
      die ($c);
    }

  'FileHandle'->new (">$f")->print ($d->textContent);

}


1;
