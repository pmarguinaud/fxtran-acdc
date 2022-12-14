package Fxtran;

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

# Type declaration statement

sub decl_stmt_list
{
  my ($decl, @name) = @_;

  my @tpina;
  for my $name (@name)
    {
      my $d = $decl->{$name};
      my $as = join (', ', map { defined ($_->[0]) ? $_->[0] . ':' . $_->[1] : $_->[1]  } @{ $d->{as} || [] });
      push @tpina, [
                      $d->{t},
                      ($d->{pointer} ? ', POINTER' : ''),
                      ($d->{i} ? sprintf (', INTENT (%5s)', $d->{i}) : ''),
                      $name,
                      $as
                   ];
    }


  my $tmax = &max (map { length ($_->[0] || '') } @tpina);
  my $pmax = &max (map { length ($_->[1] || '') } @tpina);
  my $imax = &max (map { length ($_->[2] || '') } @tpina);
  my $nmax = &max (map { length ($_->[3] || '') } @tpina);
  my $amax = &max (map { length ($_->[4] || '') } @tpina);

  my $code = '';
  for (@tpina)
    {
      $code .= sprintf ("%-${tmax}s%-${pmax}s%-${imax}s :: %-${nmax}s %s\n", $_->[0], $_->[1], $_->[2], $_->[3], $_->[4] ? "($_->[4])" : "");
    }

  return $code;
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

# Index files in pack

sub scanpack
{
  my $level = shift; $level ||= 0;
  my $intfb = shift; $intfb ||= 0;

  use File::Find;
  use Cwd;

  my @view = do { my $fh = 'FileHandle'->new ("<.gmkview"); <$fh> };
  chomp for (@view);
  shift (@view) for (1 .. $level);

  my %scan;
  for my $view (@view)
    {
      my $cwd = &cwd ();
      chdir ("src/$view");
      my $wanted = sub
        {
          my $f = $File::Find::name;
          return unless (($f =~ m/\.F90$/o) || (($f =~ m/\.intfb\.h/o) && $intfb));
          return if (($f =~ m/\.intfb/o) && (! $intfb));
          return if ($scan{&basename ($f)});
          $f =~ s,\.\/,,o;
          $scan{&basename ($f)} = $scan{$f} = "src/$view/$f";
        };
      &find ({no_chdir => 1, wanted => $wanted}, '.');
      chdir ($cwd);
    }

  return \%scan;
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

sub remove_unused_if_blocks
{
  my $doc = shift;

  my @expr = &f ('.//f:condition-E', $doc);

  for my $expr (@expr)
    {
      my $stmt = $expr->parentNode;
      if (&is_FALSE ($expr->firstChild) && $stmt->nodeName eq 'if-stmt')
        {
          $stmt->unbindNode ();
        }
      elsif (&is_TRUE ($expr->firstChild) && (($stmt->nodeName eq 'if-then-stmt') || ($stmt->nodeName eq 'else-if-stmt')))
        {
          my $if_block = $stmt->parentNode;
          for (my $block = $if_block->nextSibling; $block; $block = $block->nextSibling)
            {
              $block->unbindNode ();
            }
          if ($stmt->nodeName eq 'else-if-stmt')
            {
              &mute_else_stmt ($stmt);
            }
          elsif ($stmt->nodeName eq 'if-then-stmt')
            {
              &mute_unblock ($if_block);
            }
        }
      elsif (&is_FALSE ($expr->firstChild) && (($stmt->nodeName eq 'if-then-stmt') || ($stmt->nodeName eq 'else-if-stmt')))
        {
          my $if_block = $stmt->parentNode;
          my $else = $if_block->nextSibling;
          $if_block->unbindNode ();
          if ($if_block = $else)
            {
              $stmt = $if_block->firstChild;
              if ($stmt->nodeName eq 'else-stmt') 
                {
                  &mute_unblock ($if_block);
                }
              elsif ($stmt->nodeName eq 'else-if-stmt')
                {
                  &mute_if_stmt ($stmt);
                }
            }
        }
        
    }

}

# Remove unused interface blocks

sub remove_unused_intfb
{
  my $doc = shift;

  my @call = &f ('.//f:call-stmt//f:procedure-designator//f:n/text()', $doc, 1);
  my %incl = map { (lc ($_) . '.intfb.h', 1) } @call;

  for my $incl (&f ('.//f:include', $doc))
    {
      my ($file) = &f ('.//f:filename/text ()', $incl, 2);
      next unless ($file =~ m/\.intfb\.h$/o);
      unless ($incl{$file})
        {
          $incl->unbindNode ();
        }
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

# Remove element from list

sub remove_element_from_list
{
  my $element = shift;

  my $list = $element->parentNode ();

  if ($element->nextSibling)
    {
      for (my $x = $element->nextSibling; $x; )
        {
          my $y = $x->nextSibling;
          last unless (&non_code ($x));
          $x->unbindNode ();
          $x = $y;
        }
    }
  elsif ($element->previousSibling)
    {
      for (my $x = $element->previousSibling; $x; )
        {
          my $y = $x->previousSibling;
          last unless (&non_code ($x));
          $x->unbindNode ();
          $x = $y;
        }
    }
  $element->unbindNode ();


}

# Remove a module variable not used in the code

sub remove_used_var
{
  my $n = shift;
  my @anc = reverse &f ('./ancestor::*', $n);
  my ($rename) = grep { $_->nodeName eq 'rename' } @anc;
  my $rename_list = $rename->parentNode;
  

  &remove_element_from_list ($rename);

  unless ($rename_list->childNodes)
    {
      my $use_stmt = $rename_list->parentNode ();
      $use_stmt->unbindNode ();
    }

}

# Remove unused module variables

sub remove_unused_module_vars
{
  my $doc = shift;
  my %used_vars;

  my ($fcttrm) = &f ('.//f:include/f:filename[text ()="fcttrm.func.h"]', $doc);  
  my @fcttrm = qw (RV RCPV RETV RCW RCS RLVTT RLSTT RTT RALPW RBETW 
                   RGAMW RALPS RBETS RGAMS RALPD RBETD RGAMD);
  @fcttrm = () unless ($fcttrm);

  my @n = &f ('.//f:n/text ()', $doc);

  for my $n (@n)
    {
      my @anc = reverse &f ('./ancestor::*', $n);
      my ($stmt) = grep { $_->nodeName =~ m/-stmt$/o } @anc;
      if ($stmt->nodeName ne 'use-stmt')
        {
          $used_vars{uc ($n->textContent)}++;
        }
    }

  for (@fcttrm)
    {
      $used_vars{$_}++;
    }

  for my $n (&f ('.//f:use-stmt//f:use-N/f:N/f:n/text ()', $doc))
    {
      next if ($used_vars{uc ($n->textContent)});
      &remove_used_var ($n);
    }

}

sub remove_unused_associate
{
  my $assoc = shift;

  my @anc = reverse &f ('./ancestor::*', $assoc);
  my ($list) = grep { $_->nodeName eq 'associate-LT' } @anc;
  my ($stmt) = grep { $_->nodeName eq 'associate-stmt' } @anc;

  &remove_element_from_list ($assoc);

  unless ($list->childNodes)
    {
      my $block = $stmt->parentNode ();
      &mute_unblock ($block);
    }
  

}

# Remove unused associate variables

sub remove_unused_associates
{
  my $doc = shift;

  my @n = &f ('.//f:named-E/f:N/f:n/text ()', $doc, 1);
  my %n;
  for (@n)
    {
      $n{$_}++;
    }

  for my $assoc (&f ('.//f:associate', $doc))
    {
      my ($name) = &f ('.//f:associate-N/f:n/text ()', $assoc, 1);

      unless ($n{$name})
        {
          &remove_unused_associate ($assoc);
        }

    }


}


sub remove_entity_decl
{
  my $en_decl = shift;
  my $stmt = &stmt ($en_decl);
  my $list = $en_decl->parentNode ();
  &remove_element_from_list ($en_decl);
  unless ($list->childNodes ())
    {
      $stmt->unbindNode ();
    }
}

sub remove_unused_arguments
{
  my ($doc, $args_cb) = @_;
  
  my @n = &f ('.//f:named-E/f:N/f:n/text ()', $doc, 1);
  my %n;
  for (@n)
    {
      $n{$_}++;
    }

  my @arg = &f ('.//f:dummy-arg-LT/f:arg-N', $doc);

  my @i;
  for my $i (0 .. $#arg)
    {
      my $arg = $arg[$i];
      my $name = uc ($arg->textContent);
      unless ($n{$name})
        {
          &remove_element_from_list ($arg);
          my @en_decl = &f ('.//f:EN-decl[./f:EN-N/f:N/f:n[text ()="' . $arg->textContent . '"]]', $doc);
          for my $en_decl (@en_decl)
            {
              &remove_entity_decl ($en_decl);
            }
          push @i, $i;
        }
    }


  push @{ $args_cb }, sub 
  { 
    my $argspec = shift; 
    for my $i (reverse (@i)) 
      { 
        my @arg = &f ('.//f:arg', $argspec);
        &remove_element_from_list ($arg[$i]);
      } 
  };

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

sub indent_doc
{
  my $doc = shift;

  for my $stmt (grep { $_->nodeName =~ m/-stmt$/o } &f ('.//f:*', $doc))
    {
      my $level = &Fxtran::level ($stmt);
      &Fxtran::indent ($stmt, 2 * $level);
      &Fxtran::fold ($stmt);
    }

}

# Set logical options (replace by values), and try to simplify the code

sub apply_options
{
  my ($doc, $opt, $args_cb) = @_;

  while (my ($n, $v) = each (%$opt))
    {
      for my $expr (&f ('.//f:condition-E[.//f:named-E/f:N/f:n[./text()="' . $n . '"]]', $doc))
        {
          my @e = &f ('.//f:named-E[./f:N/f:n/text()="' . $n . '"]', $expr); 
          for (@e)
            {
              $_->replaceNode ($v->cloneNode (1));
            }
          &simplify_logical_expr ($expr->firstChild);
        }
    }
  
  &remove_unused_if_blocks ($doc);
  &remove_unused_intfb ($doc);
  &remove_unused_arguments ($doc, $args_cb);
  &remove_unused_associates ($doc);
  &remove_unused_module_vars ($doc);
}


# Build a table of type, intent and dimensions for all variables

sub decl_table
{
  my $doc = shift;

  my @decl = &f ('//f:T-decl-stmt', $doc);

  my @arg = &f ('.//f:dummy-arg-LT/f:arg-N/f:N/f:n/text ()', $doc, 1);
  my %arg = map {($_, 1)} @arg;

  my %decl;

  for my $decl (@decl)
    {
      my @en_decl = &f ('.//f:EN-decl', $decl);
      my ($t_spec) = &f ('.//f:_T-spec_', $decl, 1);
      my ($i_spec) = &f ('.//f:intent-spec/text ()', $decl, 1);

      for my $en_decl (@en_decl)
        {
          my @as;
          my @asx;

          my ($n) = &f ('.//f:n/text ()', $en_decl, 1);
          for (&f ('.//f:array-spec//f:shape-spec', $en_decl))
            {
              my ($lb) = &f ('.//f:lower-bound', $_);
              my ($ub) = &f ('.//f:upper-bound', $_);
              push @as, [$lb && $lb->textContent, $ub && $ub->textContent];
              push @asx, [$lb && $lb->toString, $ub && $ub->toString];
            }

          $decl{$n} = {(@as ? (as => \@as) : ()), 
                       (@asx ? (asx => \@asx) : ()),
                        n => $n, i => $i_spec, 
                        t => $t_spec, arg => $arg{$n}};

        }
    }

  return \%decl;
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

sub pass_module_vars_as_arguments
{
  my ($doc, $decl, $args_cb, $umod, $assoc_pair) = @_;

# List of imported items using modules (except structures)

  my @use_stmt = &f ('//f:use-stmt', $doc);

  my @u;

  for my $use_stmt (@use_stmt)
    {
      my ($mod) = &f ('.//f:module-N', $use_stmt, 1);
      next if ($mod =~ m/^(?:PARKIND1|YOMHOOK)$/o);
      my @n = &f ('.//f:use-N', $use_stmt, 1);
      push @u, grep { !/^Y/o } @n;
      for (@n)
        {
          $umod->{$_} = $mod;
        }
    }

  for my $u (@u)
    {
      $decl->{$u} = {n => $u, i => 'IN', t => &doctor_type ($u)};
    }

# Add associated symbols to the table of symbols

  my @associate = &f ('//f:associate', $doc);

  my @a;
  for my $associate (@associate)
    {
      my ($n) = &f ('.//f:associate-N', $associate, 1);
      my ($s) = &f ('.//f:selector', $associate, 1);
      push @$assoc_pair, [$n, $s];
      push @a, $n;
      $decl->{$n} = {n => $n, i => 'IN', t => &doctor_type ($n)};
    }


# Remove ASSOCIATEs

  for (&f ('//f:associate-stmt', $doc), &f ('//f:end-associate-stmt', $doc))
    {
      $_->unbindNode ();
    }

# Remove USEs

  for (&f ('//f:use-stmt', $doc))
    {
      my ($n) = &f ('.//f:module-N', $_, 1);
      next if ($n =~ m/^(?:PARKIND1)/o);
      $_->unbindNode ();
    }

# Add extra arguments declaration

  my $decl_last = &decl_last ($doc);
  $decl_last->parentNode->insertAfter (&t ("\n" . &decl_stmt_list ($decl, @u, @a)), $decl_last);


  my ($subroutine) = &f ('.//f:subroutine-stmt', $doc);
  my ($arglist) = &f ('.//f:dummy-arg-LT', $doc);

  for my $name (@u, @a)
    {
      $arglist->appendChild ($_)
        for (&t (", "), &n ("<arg-N><N><n>$name</n></N></arg-N>)"));
    }
  &fold ($subroutine);


  push @{ $args_cb }, sub 
  { 
    my $argspec = shift; 
    for my $name (@u, @a)
      {
        $argspec->appendChild (&t (", "));
        $argspec->appendChild (&n ("<arg><named-E><N><n>$name</n></N></named-E></arg>"));
      }
  };
}

sub mute_subroutine_to_call
{
  my $subroutine = shift;

  my $call = $subroutine->cloneNode (1);

  # <call-stmt>CALL <procedure-designator><named-E><N><n>FL2HL</n></N></named-E></procedure-designator> ( <arg-spec><arg><named-E><N><n>KIDIA</n></N></named-E></arg>, <arg>
  # <subroutine-stmt>SUBROUTINE <subroutine-N><N><n>ACTKE</n></N></subroutine-N> ( <dummy-arg-LT><arg-N><N><n>KIDIA</n></N><
  
  $call->setNodeName ('call-stmt');
  $call->firstChild->replaceNode (&t ("CALL "));

  my ($subroutine_name) = &f ('.//f:subroutine-N', $call);
  my $procedure_designator = &n ('<procedure-designator/>');
  $subroutine_name->setNodeName ('named-E');
  $subroutine_name->replaceNode ($procedure_designator);
  $procedure_designator->appendChild ($subroutine_name);

  my ($dummy_arg_LT) = &f ('.//f:dummy-arg-LT', $call);
  $dummy_arg_LT->setNodeName ('arg-spec');

  for my $arg (&f ('.//f:arg-N', $call))
    {
      $arg->setNodeName ('arg');
      my $N = $arg->firstChild;
      $N->unbindNode ();
      my $e = &n ('<named-E/>');
      $e->appendChild ($N);
      $arg->appendChild ($e);
    }


  return $call;

}

sub save_to_file
{
  my ($file, $data) = @_;

  if (-f $file)
    {
      my $data0 = do { my $fh = 'FileHandle'->new ("<$file"); local $/ = undef; <$fh> };
      return if ($data0 eq $data);
    }
  use File::Path;
  use File::Basename;

  &mkpath (&dirname ($file));

  'FileHandle'->new (">$file")->print ($data);
} 


sub add_used_vars
{
  my ($doc, $mod, @vars) = @_;
  my ($use_stmt) = &f ('.//f:use-stmt[./f:module-N/f:N/f:n/text ()="' . $mod . '"]', $doc);

  my %rename = map { ($_, 1) } &f ('.//f:rename//f:n/text ()', $use_stmt, 1);
  my ($rename_list) = &f ('.//f:rename-LT', $use_stmt);
  
  for my $var (@vars)
    {

      unless ($rename{$var})
        {
          my $rename = &n ("<rename><use-N><N><n>$var</n></N></use-N></rename>");
          $rename_list->appendChild (&t (", "));
          $rename_list->appendChild ($rename);
        }

    }

  &fold ($use_stmt);

  return $use_stmt;
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

sub add_associates
{
  my ($doc, @var_expr) = @_;
  return unless (@var_expr);

  my ($associate) = &f ('.//f:associate-stmt', $doc);

  unless ($associate)
    {
      my @exec_stmt = grep { &stmt_is_executable ($_) } grep { $_->nodeName =~ m/-stmt$/o } &f ('.//f:*', $doc);
      $associate = &n ('<associate-stmt>ASSOCIATE(<associate-LT/>)</associate-stmt>');
      my $end = &n ('<end-associate-stmt>END ASSOCIATE</end-associate-stmt>');
      $exec_stmt[0]->parentNode->insertBefore ($_, $exec_stmt[0]) for ($associate, &t ("\n"));
      $exec_stmt[-1]->parentNode->insertAfter ($_, $exec_stmt[-1]) for ($end, &t ("\n"));
    }

  my ($associate_list) = &f ('.//f:associate-LT', $associate);

  my %seen;
  while (my ($var, $expr) = splice (@var_expr, 0, 2))
    {
      next if ($seen{$var}++);
      $associate_list->appendChild (&t (', ')) if (&f ('.//f:*', $associate_list));
      $associate_list->appendChild (&n ("<associate><associate-N><n>$var</n></associate-N> =&gt; <selector><named-E>$expr</named-E></selector></associate>"));
    }

  &fold ($associate);

}

sub intfb_body
{
  my $doc = shift;
  
  my @pu = &F ('./object/file/program-unit', $doc);
  
  for my $pu (@pu)
    {

      for (&F ('.//program-unit', $pu))
        {
          $_->unbindNode ();
        }

      my $stmt = $pu->firstChild;
  
      (my $kind = $stmt->nodeName ()) =~ s/-stmt$//o;
  
      my ($name) = &F ('./' . $kind . '-N/N/n/text()', $stmt, 1);
      my @args = &F ('.//dummy-arg-LT//arg-N/N/n/text()', $stmt, 1);
      
      my %stmt;
      
      # Keep first & last statements
      
      $stmt{$pu->firstChild} = $pu->firstChild;
      $stmt{$pu->lastChild}  = $pu->lastChild;
      
      # Keep declaration statements referencing arguments

      for my $arg (@args)
        {
          my @en = &F ('.//EN-decl[./EN-N[string(N)="?"]]', $arg, $pu);
          for my $en (@en)
            {
              my $stmt = &Fxtran::stmt ($en);
              $stmt{$stmt} = $stmt;
            }
        }
  
      # Strip blocks (these may contain use statements)
      
      for (&F ('.//ANY-construct', $doc))
        {
          $_->unbindNode ();
        }
  
      # Keep use statements
      
      for (&F ('.//use-stmt', $pu))
        {
          $stmt{$_} = $_;
        }
      
      my @stmt = &F ('.//ANY-stmt', $pu);
      
      for my $stmt (@stmt)
        {
          $stmt->unbindNode () unless ($stmt{$stmt});
        }
  
    }
  

  # Strip labels
  for (&F ('.//label', $doc))
    {
      $_->unbindNode ();
    }
  
  # Strip comments
  
  for (&F ('.//C', $doc))
    {
      next if ($_->textContent =~ m/^!\$acc\s+routine/o);
      $_->unbindNode ();
    }
  
  # Strip includes
  
  for (&F ('.//include', $doc))
    {
      $_->unbindNode ();
    }

  # Strip defines

  for (&F ('.//cpp[starts-with (text(),"#define ")]', $doc))
    {
      $_->unbindNode ();
    }

  $doc->documentElement->normalize ();

  my @text = &F ('.//text()[translate(.," ?","")=""]', "\n", $doc);

  for my $text (@text)
    {
      if ($text->data =~ m/\n/goms)
        {
          $text->setData ("\n");
        }
    }

}

sub intfb
{
  my ($F90, $dir) = @_;
  
  $dir ||= '.';

  my $doc = &Fxtran::parse (location => $F90, fopts => ['-construct-tag', '-no-include', '-line-length' => 500]);
  
  &intfb_body ($doc);

  # Strip empty lines
  
  my $text = $doc->textContent ();
  
  $text =~ s/^\s*\n$//goms;
  
  my $sub = &basename ($F90, qw (.F90));
  
  
  &Fxtran::save_to_file ("$dir/$sub.intfb.h", << "EOF");
INTERFACE
$text
END INTERFACE
EOF

  return "$dir/$sub.intfb.h";
}

1;
