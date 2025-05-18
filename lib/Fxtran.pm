package Fxtran;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use XML::LibXML;
use Data::Dumper;
use List::Util qw (max);
use FileHandle;
use Storable;
use File::Basename;
use Storable;
use Carp qw (croak);

use fxtran;
use fxtran::xpath;

use strict;

use base qw (Exporter);
our @EXPORT = qw (s e F f n t TRUE FALSE);

{
my $version;
sub getVersion
{
  use FindBin qw ($Bin);
  use Cwd;

=pod

commit b9206730bbe52494d44b39b5d3a701868f334be0
Author: Philippe Marguinaud <philippe.marguinaud@meteo.fr>
Date:   Sun Feb 5 16:09:41 2023 +0000

    Add OpenMPSingleColumn transform

=cut

  unless ($version)
    {
      my $cwd = &cwd ();
     
      chdir ($Bin);
      my @log = split (m/\n/o, `git log -n1`);
      chdir ($cwd);
     
      ($version) = ($log[0] =~ m/commit\s+(\w+)/o);
    }

  return $version;
}

}

sub removeListElement
{

# Remove element from list, take care of removing comma before or after the element

  my $x = shift;

  my $nn = $x->nodeName;

  my ($p) = $x->parentNode;
  
  my @cf = &F ('following-sibling::text()[contains(.,",")]', $x);   
  my @cp = &F ('preceding-sibling::text()[contains(.,",")]', $x);   
  
  if (@cf)
    {   
      $cf[+0]->unbindNode (); 
    }   
  elsif (@cp)
    {   
      $cp[-1]->unbindNode (); 
    }   
  
  $x->parentNode->appendChild (&t (' '));
  my $l = $x->parentNode->lastChild;
  
  $x->unbindNode (); 
  
  while ($l)
    {   
      last if (($l->nodeName ne '#text') && ($l->nodeName ne 'cnt'));
      $l = $l->previousSibling;
      last unless ($l);
      $l->nextSibling->unbindNode;
    }   

  return &F ("./$nn", $p) ? 0 : 1;
}



sub getIndent
{
  my $stmt = shift;

  $stmt or croak;

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

sub reIndent
{
  my ($node, $ns) = @_;

  my $sp = ' ' x $ns; 

  my @cr = &f ('.//text ()[contains (.,"' . "\n" . '")]', $node);

  for my $cr (@cr)
    {    
      (my $t = $cr->data) =~ s/\n/\n$sp/g;
      $cr->setData ($t);
    }
}

sub xpath_by_type
{
  my $type = shift;
  my $size = length ($type);
  return '*[substring(name(),string-length(name())-'.$size.')="-'.$type.'"]';
}

sub _offset
{
  my ($node, $pfound) = @_;

  my $offset = 0;
  for (my $c = $node->previousSibling; $c; $c = $c->previousSibling)
    {
      last if ($c->nodeName eq 'xml-stylesheet');
      my $text = $c->textContent;
      my @text = reverse split (m/(\n)/o, $text);
      for (@text)
        {
          if (m/\n/o)
            {
              $pfound = 1;
              goto FOUND;
            }

          $offset += length ($_);
        }
    }

  if ((! $$pfound) && $node->parentNode)
    {
      $offset += &offset ($node->parentNode, $pfound);
    }

FOUND:

  return $offset;
}

sub offset
{
  my $node = shift;
  return &_offset ($node, \0);
}

# Fold statement workhorse

sub _fold
{
  my ($node, $plen, $indent, $cnt, $len) = @_;
  
  my $shift = 0;

  my @n = &f ('.//text ()', $node);
  if ((scalar (@n) == 1) || ($node->nodeName eq '#text'))
    {
      
      my $lenc = $$plen;

      $$plen += length ($node->textContent);

      my ($lit) = &f ('./ancestor::f:literal-E', $node);
      my ($nam) = &f ('./ancestor::f:named-E', $node);
      my ($ass) = &f ('./ancestor::f:associate', $node);
      my ($arg) = &f ('./ancestor::f:arg', $node);

      if (($$plen > 100) && (! $lit) && (! $nam) && (! $ass) && (! $arg))
        {
          if ($node->textContent =~ m/^\s*,\s*$/o)
            { 
              $lenc = $$plen;
              $node = $node->nextSibling;
              $shift = 1;
            }

          my $c = &n ("<cnt>&amp;</cnt>");

          $node->parentNode->insertBefore ($c, $node);
          $node->parentNode->insertBefore (&t ("\n" . (' ' x $indent)), $node);
          $node->parentNode->insertBefore (&n ("<cnt>&amp;</cnt>"), $node);
          $node->parentNode->insertBefore (&t (" "), $node);
          $$plen = $indent + 2 + length ($node->textContent);

          $lenc++;
          push @$len, $lenc;
          push @$cnt, $c;
        }
    }
  else
    {
      my @c = $node->childNodes;
      while (my $c = shift (@c))
        {
          &_fold ($c, $plen, $indent, $cnt, $len) && shift (@c);
        }
    }

  return $shift;
}


sub expand
{
  my $stmt = shift;

  for (&F ('.//cnt', $stmt), &F ('.//C', $stmt))
    {
      $_->unbindNode ();
    }
  for (&f ('.//text ()', $stmt))
    {
      my $data = $_->data;
      if ($data =~ m/\n/o)
        {
          $data =~ s/\s+/ /go;
          $_->setData ($data);
        }
    }

  $stmt->normalize ();
}

# Fold a statement

sub fold
{
  my $stmt = shift;

  &expand ($stmt);

  my $indent = &offset ($stmt);

  my $len = $indent;
  my @len;
  my @cnt;
  &_fold ($stmt, \$len, $indent, \@cnt, \@len);

  my ($lenmax) = sort { $b <=> $a } @len;

  for my $i (0 .. $#cnt)
    {
      $cnt[$i]->parentNode->insertBefore (&t (' ' x ($lenmax - $len[$i])), $cnt[$i]);
    }

  $stmt->normalize ();
}



# Guess type using the doctor norm

sub doctor_type
{
  my $v = shift;
  if ($v =~ m/^[NIJKM]/o)
    {   
      return 'INTEGER(KIND=JPIM)';
    }   
  elsif ($v =~ m/^[L]/o)
    {   
      return 'LOGICAL';
    }   
  elsif ($v =~ m/^[GUSEAVZPRT]/o)
    {   
      return 'REAL(KIND=JPRB)';
    }   
}

# Returns the statement the element belongs to

sub stmt
{
  my $e = shift;
  my @anc = reverse &F ('./ancestor::*', $e);
  my ($stmt) = grep { $_->nodeName =~ m/-stmt$/o } @anc;
  return $stmt;
}

sub expr
{
  my $e = shift;
  my @anc = reverse &f ('./ancestor::*', $e);
  my ($expr) = grep { $_->nodeName =~ m/-E$/o } @anc;
  return $expr;
}

sub is_INTEGER
{
  my $e = shift;
  return (($e->nodeName eq 'literal-E') && ($e->textContent =~ m/^\d+$/o));
}

sub val_INTEGER
{
  my $e = shift;
  die unless (&is_INTEGER ($e));
  return $e->textContent;
}

sub is_TRUE
{
  my $e = shift;
  if ($e->nodeName eq 'literal-E')
    {
      return $e->textContent eq '.TRUE.' ? 1 : 0;
    }
}

sub is_FALSE
{
  my $e = shift;
  if ($e->nodeName eq 'literal-E')
    {
      return $e->textContent eq '.FALSE.' ? 1 : 0;
    }
}

sub TRUE
{
  &n ('<literal-E>.TRUE.</literal-E>');
}

sub FALSE
{
  &n ('<literal-E>.FALSE.</literal-E>');
}

# Try to reduce logical expr

sub simplify_logical_expr
{
  my ($e) = @_;
  
  if ($e->nodeName () eq 'op-E')
    {
      my @o = grep { $_->isa ('XML::LibXML::Element') } $e->childNodes ();
      if (@o == 2)
        {
          if ($o[0]->textContent eq '.NOT.')
            {
              &simplify_logical_expr ($o[1]);
              @o = grep { $_->isa ('XML::LibXML::Element') } $e->childNodes ();
              if (&is_TRUE ($o[1]))
                {
                  $e->replaceNode (&FALSE ()); return;
                }
              elsif (&is_FALSE ($o[1]))
                {
                  $e->replaceNode (&TRUE ()); return;
                }
              else
                {
                  return;
                }
            }
          else
            {
              die;
            }
        }
      elsif (@o == 3)
        {
          &simplify_logical_expr ($o[0]);
          &simplify_logical_expr ($o[2]);
          @o = grep { $_->isa ('XML::LibXML::Element') } $e->childNodes ();
          if ($o[1]->textContent eq '.AND.')
            {
              if (&is_TRUE ($o[0]))
                {
                  $e->replaceNode ($o[2]); return;
                }
              elsif (&is_TRUE ($o[2]))
                {
                  $e->replaceNode ($o[0]); return;
                }
              elsif (&is_FALSE ($o[0]) || &is_FALSE ($o[2]))
                {
                  $e->replaceNode (&FALSE ()); return;
                }
              else
                {
                  return;
                }
            }
          elsif ($o[1]->textContent eq '.OR.')
            {
              if (&is_TRUE ($o[0]) || &is_TRUE ($o[2]))
                {
                  $e->replaceNode (&TRUE ()); return;
                }
              elsif (&is_FALSE ($o[0]))
                {
                  $e->replaceNode ($o[2]); return;
                }
              elsif (&is_FALSE ($o[2]))
                {
                  $e->replaceNode ($o[0]); return;
                }
              else
                {
                  return;
                }
            }
          elsif ($o[1]->textContent =~ m/^(?:==|\.EQ\.)$/io)
            {
              return unless (&is_INTEGER ($o[0]) && &is_INTEGER ($o[2]));
              if (&val_INTEGER ($o[0]) == &val_INTEGER ($o[2]))
                {
                  $e->replaceNode (&TRUE ()); return;
                }
              else
                {
                  $e->replaceNode (&FALSE ()); return;
                }
            }
          elsif ($o[1]->textContent =~ m/^(?:>|\.GT\.)$/io)
            {
              return unless (&is_INTEGER ($o[0]) && &is_INTEGER ($o[2]));
              if (&val_INTEGER ($o[0]) > &val_INTEGER ($o[2]))
                {
                  $e->replaceNode (&TRUE ()); return;
                }
              else
                {
                  $e->replaceNode (&FALSE ()); return;
                }
            }
          else
            {
              die $o[1]->textContent;
            }
        }
      else
        {
          die &Dumper (\@o);
        }
    }
  elsif ($e->nodeName eq 'parens-E')
    {
      my @c = $e->childNodes ();
      &simplify_logical_expr ($c[1]);
      if (&is_TRUE ($c[1]) || &is_FALSE ($c[1]))
        {
          $e->replaceNode ($c[1]);
        }
      return;
    }
  elsif ($e->nodeName eq 'literal-E')
    {
      return;
    }
  elsif ($e->nodeName eq 'named-E')
    {
      return;
    }
  die $e->toString ();
}

# Change ELSEIF statement into IFTHEN

sub mute_if_stmt
{
  my $stmt = shift;
  my $ELSEIF = $stmt->firstChild->textContent;
  $ELSEIF =~ s/^ELSE//o;
  $stmt->firstChild->replaceNode (&t ($ELSEIF));
  $stmt->setNodeName ('if-then-stmt');
}

# Change statement into ELSE statement

sub mute_else_stmt
{
  my $stmt = shift;
  $stmt->replaceNode (&n ("<else-stmt>ELSE</else-stmt>"));
}

# Remove IF/ENDIF, keep inner statements

sub mute_unblock
{
  my $block = shift;

  my $indent = &offset ($block);

  my @stmt = $block->childNodes ();
  shift (@stmt); pop (@stmt);
  my $p = $block->parentNode;
  for (reverse @stmt)
    {
      $p->insertAfter ($_, $block);
    }
  $block->unbindNode ();

  for (@stmt)
    {
      my $ind = $indent - &offset ($_);
      &indent ($_, $ind);
    }

}

# Test if element is something else than code

sub non_code
{
  my $e = shift;
  my $nn = $e->nodeName;
  my %nn = map { ($_, 1) } ('#text', 'cnt', 'C');
  return $nn{$nn};
}

sub level
{
  my $stmt = shift;

  my @construct = grep { $_->nodeName =~ m/-construct$/o } &f ('.//ancestor::f:*', $stmt);
  my $level = scalar (@construct);

  if (($stmt->parentNode->nodeName =~ m/-(block|construct)$/o) && 
      ((! $stmt->nextSibling) || (! $stmt->previousSibling)))
    {
      $level--;
    }

  return $level;
}
    
# Indent node using the number of spaces provided as the second argument

sub prev_space
{
  my $node = shift;

  return unless ($node);

  my $sp = $node->previousSibling; 

  if ($sp && ($sp->nodeName eq '#text'))
    {
      return $sp;
    } 
  elsif ($sp && ($sp->nodeName ne '#text'))
    {
      &prev_space ($sp->previousSibling);
    }
  else 
    {
      &prev_space ($node->parentNode);
    }
}

sub indent
{
  my ($node, $indent) = @_;
  my @cr = &f ('.//text ()[contains (., "' . "\n" . '")]', $node);

  my $sp = &prev_space ($node);
  if ($sp)
    {
      unshift (@cr, $sp);
    }

  for my $cr (@cr)
    {
      if ($cr->textContent =~ m/^(\s*)\n(\s*?)$/o)
        {
          my ($speol, $spbeol) = ($1, $2);
          my $ind = $indent =~ m/^[+-]/o ? length ($spbeol) + $indent : $indent;
          $cr->replaceNode (&t ($speol . "\n" . (' ' x $ind)));
        }
    }
  
}

sub decl_last
{
  my $doc = shift;

  my @arg = &f ('.//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $doc, 1);
  my @decl = &f ('//f:T-decl-stmt', $doc);  

  my $decl_last;
  for my $d (@decl)
    {   
      my @n = &f ('.//f:EN-decl//f:EN-N//f:n/text ()', $d, 1); 
      my %n = map { ($_, 1) } @n; 
      for (@arg)
        {
          if ($n{$_})
            {
              $decl_last = $d;
            }
        }
    }   

  return $decl_last;
}

sub stmt_is_executable
{
  my $stmt = shift;
  &croak ("Undefined stmt\n") unless ($stmt);

  my @noexec = ('subroutine-stmt', 'use-stmt', 'T-decl-stmt', 'end-subroutine-stmt', 'data-stmt', 'save-stmt',
                'implicit-none-stmt', 'T-stmt', 'component-decl-stmt', 'end-T-stmt');
  my %noexec = map {($_, 1)} @noexec;

  if ($noexec{$stmt->nodeName})
    {
      return 0;
    }
  return 1;
}


1;
