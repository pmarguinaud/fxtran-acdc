package Construct;

use strict;
use Fxtran;
use Data::Dumper;
use FileHandle;
use File::Basename;
use Fxtran;


sub changeIfStatementsInIfConstructs
{
  my $d = shift;

  # Change if statements into if constructs

  my @if_stmt = &F ('.//if-stmt', $d);

  for my $if_stmt (@if_stmt)
    {
      my $indent = &Fxtran::getIndent ($if_stmt);

      my ($action) = &F ('./action-stmt', $if_stmt);
      my ($stmt) = &F ('./ANY-stmt', $action);

      die $if_stmt->textContent unless ($stmt);

      next if ($stmt->nodeName eq 'call-stmt' && $stmt->textContent =~ m/DR_HOOK/o);

      $action->unbindNode ();

      $stmt = $stmt->textContent;
      my $if = $if_stmt->textContent;

      my ($if_construct) = &Fxtran::parse (fragment => << "EOF");
$if THEN
  $stmt
ENDIF
EOF

     &Fxtran::reIndent ($if_construct, $indent);

     $if_stmt->replaceNode ($if_construct);
      
    }

}


sub simplify
{
  my $e1 = shift;
  my $e = $e1->parentNode;
  return unless ($e->nodeName =~ m/-E$/o);
  
  my $nn;

  if (my ($op) = &F ('./op', $e, 1))
    {
      my ($e2) = grep { $_->unique_key != $e1->unique_key } &F ('./ANY-E', $e);
      if ($e1->textContent eq '.TRUE.')
        {
          if ($op eq '.AND.')
            {
              $nn = $e2;
            }
          elsif ($op eq '.OR.')
            {
              $nn = $e1;
            }
          elsif ($op eq '.NOT.')
            {
              $nn = &n ('<literal-E>.FALSE.</literal-E>');
            }
        }
      if ($e1->textContent eq '.FALSE.')
        {
          if ($op eq '.AND.')
            {
              $nn = $e1;
            }
          elsif ($op eq '.OR.')
            {
              $nn = $e2;
            }
          elsif ($op eq '.NOT.')
            {
              $nn = &n ('<literal-E>.TRUE.</literal-E>');
            }
        }
    }
  elsif ($e->nodeName eq 'parens-E')
    {
      $nn = $e1;
    }

  unless ($nn)
     {
#      print $e->textContent, "\n";
       return;
     }

  $e->replaceNode ($nn); 

  if ($nn->textContent =~ m/^\.(?:TRUE|FALSE)\.$/o)
    {
      &simplify ($nn);
    }

}

sub apply
{
  my ($d, %c) = @_;

  while (my ($k, $v) = each (%c))
    {
      my @expr = &F ($k, $d);
      for my $expr (@expr)
        {
          $expr->replaceNode ($v->cloneNode (1));
        } 
    }

  my @tr = &F ('.//named-E[string(N)="TRIM"][./R-LT/parens-R/element-LT/element/string-E]', $d);
  
  for my $tr (@tr)
    {
      my ($s) = &F ('./R-LT/parens-R/element-LT/element/string-E', $tr);
      $tr->replaceNode ($s);
    }
  
  my @sc = &F ('.//op-E[string(op)="=="][count(./string-E)=2]', $d);

  for my $sc (@sc)
    {
      my ($e1, $e2) = &F ('./string-E/S', $sc, 2);
      for ($e1, $e2)
        {
          s/^["']//o;
          s/["']$//o;
        }
      if ($e1 eq $e2)
        {
          $sc->replaceNode (&n ("<literal-E>.TRUE.</literal-E>"));
        }
      else
        {
          $sc->replaceNode (&n ("<literal-E>.FALSE.</literal-E>"));
        }
    }



  my @expr = &F ('.//literal-E[string(.)=".FALSE." or string(.)=".TRUE."]', $d);

  for my $expr (@expr)
    {
      next unless (&Fxtran::stmt ($expr));
      &simplify ($expr);
    }

  for my $ce (&F ('.//if-stmt/condition-E[string(.)=".TRUE." or string(.)=".FALSE."]', $d))
    {
      my $stmt = $ce->parentNode;
      if ($stmt->nodeName eq 'if-stmt')
        {
          if ($ce->textContent eq '.TRUE.')
            {
              my ($stmt1) = &F ('./action-stmt/ANY-stmt', $stmt);
              $stmt->replaceNode ($stmt1);
            }
          else
            {
              $stmt->unbindNode ();
            }
        }
    }

  &simplifyIfConstructs ($d);
}

sub simplifyIfConstructs
{
  my $d = shift;

  my @if_construct = &F ('.//if-construct[./if-block/ANY-stmt/condition-E[string(.)=".TRUE." or string(.)=".FALSE."]]', $d);

  for my $i (0 .. $#if_construct)
    {
      my $if_construct = $if_construct[$i];
  
#     my ($file) = &F ('ancestor::file', $if_construct);
#     next unless ($file);
  
      my $str = $if_construct->textContent;
  
      if (my ($if_block) = &F ('./if-block[./ANY-stmt/condition-E[string(.)=".TRUE."]]', $if_construct))
        {
          if (my @if_block = &F ('following-sibling::if-block', $if_block))
            {
              for (@if_block)
                {
                  $_->unbindNode ();
                }
              $if_block->appendChild (&n ('<end-if-stmt>ENDIF</end-if-stmt>'));
            }
        }
  

      if (my @if_block = &F ('./if-block[./ANY-stmt/condition-E[string(.)=".FALSE."]]', $if_construct))
        {
          for my $if_block (@if_block)
            {
              if (my $prev = $if_block->previousSibling)
                {
                  unless ($if_block->nextSibling)
                    {
                      $prev->appendChild (&n ('<end-if-stmt>ENDIF</end-if-stmt>'));
                    }
                  $if_block->unbindNode ();
                }
              elsif (my $next = $if_block->nextSibling)
                {
                  my ($stmt) = &F ('./ANY-stmt', $next);
                  if ($stmt->nodeName eq 'else-stmt')
                    {
                      $if_block->unbindNode ();
                      $stmt->replaceNode (&n ("<if-then-stmt>IF (<condition-E><literal-E>.TRUE.</literal-E></condition-E>) THEN</if-then-stmt>"));
                    }
                  elsif ($stmt->nodeName eq 'else-if-stmt')
                    {
                      $if_block->unbindNode ();
                      $stmt->setNodeName ('if-then-stmt');
                      my $tt = $stmt->firstChild;
                      $tt->replaceNode (&t ("IF ("));
                    }
                }
            }
        }
  
      my @if_block = &F ('./if-block', $if_construct);
  
      if (scalar (@if_block) == 1)
        {
          my ($if_block) = @if_block;
          if (&F ('./ANY-stmt/condition-E[string(.)=".TRUE."]', $if_block))
            {
              $if_block->firstChild->unbindNode ();
              $if_block->lastChild->unbindNode ();
 
              for my $node (&F ('./if-block/node()', $if_construct))
                {
                  $if_construct->parentNode->insertBefore ($node, $if_construct);
                }
            }

          $if_construct->unbindNode ();
        }
  
  
    }


}

sub removeEmptyDoConstructs
{
  my $d = shift;

  for my $do_construct (reverse (&F ('.//do-construct', $d)))
    {
      &removeEmptyIfConstructs ($do_construct);
      my @stmt = &F ('.//ANY-stmt', $do_construct);
      next unless (scalar (@stmt) == 2);
      $do_construct->unbindNode ();
    }
}

sub removeEmptyIfConstructs
{
  my $d = shift;

  for my $if_construct (reverse (&F ('.//if-construct', $d)))
    {
#     &removeEmptyDoConstructs ($if_construct);
      my @if_block = &F ('./if-block', $if_construct);
      my @stmt = &F ('.//ANY-stmt', $if_construct);

      # Non final blocks have a single statement, final block has two statements
      next unless (scalar (@stmt) == (scalar (@if_block) + 1));
      $if_construct->unbindNode ();
    }
}

sub removeEmptyConstructs
{
  my $d = shift;
  &removeEmptyIfConstructs ($d);
  &removeEmptyDoConstructs ($d);
}

1;
