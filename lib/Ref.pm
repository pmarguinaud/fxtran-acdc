package Ref;

use strict;
use Fxtran;

sub parensToArrayRef
{
  my $r = shift;
  if ($r->nodeName eq 'parens-R')
    {   
      my ($elt) = &F ('./element-LT', $r);
      $r->setNodeName ('array-R');
      $elt->setNodeName ('section-subscript-LT');
      for my $i (&F ('./element', $elt))
        {
          $i->setNodeName ('section-subscript');
          my ($lb) = &n ('<lower-bound/>');
          $lb->appendChild ($_) for ($i->childNodes ());
          $i->appendChild ($lb);
        }
    }   
}

sub resolveParensRef
{
  my $d = shift;
  my @r = &F ('./parens-R', $d);

  for my $r (@r)
    {
      my $rlt = $r->parentNode;
      my @r = &F ('./ANY-R', $rlt);
      if (scalar (@r) == 1)
        {
          my $expr = $rlt->parentNode;
          my ($N) = &F ('./N', $expr, 1);
          next if (substr ($N, 0, 1) eq 'F')
        }
      &parensToArrayRef ($r);
    }

}

sub getRLT
{
  my $expr = shift;

  my ($rlt) = &F ('./R-LT', $expr);

  unless ($rlt)
    {   
      $expr->appendChild ($rlt = &n ('<R-LT/>'));
    }   

  return $rlt;
}


1;
