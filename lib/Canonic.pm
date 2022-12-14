package Canonic;

use strict;
use Fxtran;
use Data::Dumper;
use Intrinsic;
use Ref;

sub makeCanonicReferences
{
  my $d = shift;

  for my $p (&F ('.//parens-R', $d))
    {
      my $expr = &Fxtran::expr ($p);

      if ($p->previousSibling)
        {
          &Ref::parensToArrayRef ($p);
          goto DONE;
        }

      my ($n) = &F ('./N', $expr, 1);
      if (&Intrinsic::isIntrinsic ($n))
        {
          $p->setNodeName ('function-R');
          goto DONE;
        }
      
      if (substr ($n, 0, 1) eq 'F')
        {
          $p->setNodeName ('function-R');
          goto DONE;
        }
 
      &Ref::parensToArrayRef ($p);

DONE:

    }

}

sub makeCanonic
{
  my $d = shift;

  &makeCanonicReferences ($d);

  return;

  my ($pu) = &F ('./object/file/program-unit', $d);

  my ($ex1, $ex2);
  if (my @drhook = &F ('./if-stmt[./action-stmt/call-stmt[string(procedure-designator)="DR_HOOK"]]', $pu))
    {
      ($ex1, $ex2) = ($drhook[0], $drhook[-1]);
    }
  else
    {
      die;
      my @node = &F ('./node()', $pu);
      for my $node (@node)
        {
          my $name = $node->nodeName;
          next unless ($name =~ m/-stmt$/o);
          print "$name\n";
        }
    }


  my $body = &n ('<body/>');
  $ex1->parentNode->insertBefore ($body, $ex1);

  for my $node ($ex1, &F ('following-sibling::node()', $ex1))
    {
      $body->appendChild ($node);
      last if ($ex2->unique_key eq $node->unique_key);
    }

  

}


sub indentCr
{
  my $e = shift;
  my @cr = &F ('.//text()[contains(.,"?")]', "\n", $e); pop (@cr);
  for my $cr (@cr)
    {
      (my $tt = $cr->data) =~ s/\n/\n  /goms;
      $cr->setData ($tt);
    }
}

sub indent
{
  my $d = shift;
  my %args = @_;

  $d = $d->cloneNode (1);

  my $width = $args{width} || 100;
  
  for my $pu (&F ('.//program-unit', $d))
    {
      $pu->parentNode->insertAfter (&t ("\n"), $pu);
    }

  for my $stmt (&F ('.//ANY-stmt', $d))
    {
      my $name = $stmt->nodeName;

      if ($name eq 'implicit-none-stmt')
        {
          $stmt->parentNode->insertBefore (&t ("\n"), $stmt);
          $stmt->parentNode->insertAfter (&t ("\n"), $stmt);
          $stmt->firstChild->setData ('IMPLICIT NONE');
        }

      my $text = $stmt->textContent;
      my @text;
  
      my $length = length ($text);
  
      if ($length < $width)
        {
          @text = ($text);
        }
      else
        {
          my $n = int (($length+$width-1) / $width);
          my $w = 2 + int ($length / $n);
          while (length (my $t = substr ($text, 0, $w, '')))
            {
              $w-- unless (@text);
             
              while ($text && ($text =~ m/^\b/o))
                {
                  $t .= substr ($text, 0, 1, '');
                }

              push @text, $t;
            }
        }
      $text = join ("&\n&", @text);
      my $name = $stmt->nodeName;
      my $t = &t ($text);
      my $s = &n ("<$name/>");
      $s->appendChild ($t);
      $stmt->replaceNode ($s);
    }
  
  for my $construct (&F ('.//ANY-construct', $d))
    {
      if (my @block = &F ('./ANY-block', $construct))
        {
          for my $i (0 .. $#block)
            {
              my $block = $block[$i];
              &indentCr ($block);
            }
        }
      else
        {
          &indentCr ($construct);
        }
      $construct->normalize ();
      $construct->parentNode->insertAfter (&t ("\n"), $construct);
      my $prev = $construct->previousSibling;
      if ($prev->nodeName eq '#text')
        {
          (my $tt = $prev->data) =~ s/\n/\n\n/goms;
          $prev->setData ($tt);
        }
    }

  return $d->textContent;
}


1;
