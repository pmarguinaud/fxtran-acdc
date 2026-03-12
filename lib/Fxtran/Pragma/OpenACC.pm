package Fxtran::Pragma::OpenACC;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

use base qw (Fxtran::Pragma);

use strict;
use Fxtran;
use List::MoreUtils qw (uniq);

sub unfoldDirectives
{
  my $d = shift;

  for my $openacc (&F ('.//ANY-openacc', $d))
    {
      for my $x (&F ('.//acc|.//cnt|.//text()[contains(.,"?")]', "\n", $openacc))
        {
          if ($x->nodeName eq '#text')
            {
              my $tt = $x->textContent; 
              $tt =~ s/\s+/ /go;
              $x->setData ($tt);
            }
          else
            {
              $x->unbindNode ();
            }
        }
    }
}

sub insertDirective
{
  my ($p, $d, %c)  = @_;

  my $P = $p->parentNode;

  my @d = (" $d ");

  for my $c (sort keys (%c))
    {
      next unless (my @l = sort &uniq (@{ $c{$c} }));
      my $f = 1;

      while (@l)
        {
          my @x;

          while (my $l = shift (@l))
            {
              push @x, $l;
              last if (length (join ('', @x)) > 50);
            }

          push @d, join (', ', @x); $d[-1] .= ', ' if (scalar (@l));
          if ($f)
            {
              $d[-1] = "$c (" . $d[-1];
              $f = 0;
            }
          else
            {
              $d[-1] = (' ' x (2 + length ($c))) . $d[-1];
            }
          unless (@l)
            {
              $d[-1] = $d[-1] . ") ";
            }
        }
    }
  
  for my $i (0 .. $#d)
    {
      if ($i < $#d)
        {
          $d[$i] = $d[$i] . '&amp;'
        }
      if ($i > 0)
        {
          $d[$i] = '&amp;' . $d[$i];
        }
      $d[$i] = "!\$ACC$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  for my $d (@d)
    {
      $P->insertBefore (&n ("<C>$d</C>"), $p);
      $P->insertBefore (&t ("\n"), $p);
    }


  $P->insertBefore (&t ("\n"), $p);
      
}

sub insertParallelLoopGang
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL LOOP GANG', %c);
}

sub insertParallelLoopGangVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'PARALLEL LOOP GANG VECTOR', %c);
}

sub insertData
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'DATA', %c);
  $p->parentNode->insertAfter (&t ("\n"), $p);
  $p->parentNode->insertAfter (&n ("<C>!\$ACC END DATA</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub insertLoopVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'LOOP VECTOR', %c);
}

sub insertRoutineVector
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  $d->insertAfter (&n ("<C>!\$ACC ROUTINE ($N) VECTOR</C>"), $d->firstChild);
  $d->insertAfter (&t ("\n"), $d->firstChild);
}

sub insertRoutineSeq
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  $d->insertAfter (&n ("<C>!\$ACC ROUTINE ($N) SEQ</C>"), $d->firstChild);
  $d->insertAfter (&t ("\n"), $d->firstChild);
}

sub insertSerial
{
  shift;
  my ($p, %c) = @_;
  &insertDirective ($p, 'SERIAL', %c);
  $p->parentNode->insertAfter (&n ("<C>!\$ACC END SERIAL</C>"), $p);
  $p->parentNode->insertAfter (&t ("\n"), $p);
}

sub enterDataCreate
{
  shift;
  return @_ ? '!$ACC ENTER DATA CREATE (' . join (', ', @_) . ')' : '';
}

sub exitDataDelete
{
  shift;
  return @_ ? '!$ACC EXIT DATA DELETE (' . join (', ', @_) . ')' : '';
}

sub updateDevice
{
  shift;
  return @_ ? '!$ACC UPDATE DEVICE (' . join (', ', @_) . ')' : '';
}

sub enterDataAttach
{
  shift;
  return @_ ? '!$ACC ENTER DATA ATTACH (' .  join (', ', @_) . ')' : '';
}

sub exitDataDetach
{
  shift;
  return @_ ? '!$ACC EXIT DATA DETACH (' .  join (', ', @_) . ')' : '';
}

sub getACC
{
  my $openacc = shift;
  my @x;
  for (my $x = $openacc; $x; $x = $x->previousSibling)
    {
      unshift (@x, $x);
      last if ($x->nodeName eq 'acc');
    }
  return @x;
}

sub pruneClauses
{
  my ($openacc, $list, $keep) = @_;  # list =  clauses to prune and return, keep = clauses to return but not prune

  my @c;

  for my $c (&F ('./clause', $openacc))
    {
      my ($n) = &F ('./N', $c, 1); $n = lc ($n);
      next unless ($list->{$n});
      if ($keep && $keep->{$n})
        {
          push @c, $c->cloneNode (1);
        }
      else
        {
          push @c, $c;
          my $p = $c->previousSibling;

          if (($p->nodeName eq '#text') && ((my $tt = $p->textContent) =~ s/^(\w+)\s+/$1/o))
            {
              $p->setData ($tt);
            }
          else
            {
              $c->previousSibling->unbindNode ();
            }
          $c->unbindNode ();
        }
    }

  return @c;
}


# Clauses specific to the loop directive

my %clauseLoop = map { ($_, 1) } qw (collapse gang worker vector seq independent auto 
                                     tile device_type private reduction);

# Create a dedicated LOOP directive (GANG and/or VECTOR)
sub expandParallelLoop
{
  my $pl = shift;

  my $p = $pl->parentNode;

  my ($do) = &F ('following-sibling::do-construct', $pl);
  my ($acc) = &getACC ($pl);
  my $indent = $acc->previousSibling;
  $indent = $indent->textContent;
  $indent =~ s/\s*\n/\n/o;
  
  my @c = &pruneClauses ($pl, \%clauseLoop);

  my $loop = &n ('<loop-openacc>LOOP</loop-openacc>');

  for my $c (@c)
    {
      $loop->appendChild ($_) for (&t (' '), $c);
    }

  $acc = $acc->cloneNode (1);

  my $q = $pl->nextSibling;
  $p->insertBefore ($_, $q) for (&t ($indent), $acc, &t (' '), $loop);

  my $f = $pl->firstChild;
  unless ($f)
    {
      $pl->appendChild ($f = &t (' '));
    }
  $f->setData ('PARALLEL');

  $p->insertAfter ($_, $do) for (reverse (&t ($indent), &n ('<acc>!$ACC</acc>'), &t (' '), &n ('<end-parallel-openacc>END PARALLEL</end-parallel-openacc>')));

  $pl->setNodeName ('parallel-openacc');
}

# Clauses specific to the data directive

my %clauseData = map { ($_, 1) } qw (if async wait device_type copy copyin copyout
                                     create no_create present deviceptr attach);

# Create a dedicated DATA directive for handling DATA clauses
sub expandParallelData
{
  my $pa = shift;

  (my $type = $pa->nodeName) =~ s/-openacc$//o; # parallel or serial

  die ("Unexpected node $pa") unless ($type =~ m{^(?:parallel|serial)$}o);

  my $p = $pa->parentNode;

  my ($pe) = &F ("following-sibling::end-$type-openacc", $pa);
  $pe or die ("Could not find ACC END \U$type directive");

  my ($acc) = &getACC ($pa);
  my $indent = $acc->previousSibling;
  $indent = $indent->textContent;
  $indent =~ s/\s*\n/\n/o;

  my %keep = (if => 1, async => 1);

  my @c = &pruneClauses ($pa, \%clauseData, \%keep);

  my $data = &n ('<data-openacc>DATA</data-openacc>');

  my $nokeep;

  for my $c (@c)
    {
      $data->appendChild ($_) for (&t (' '), $c);
      my ($n) = &F ('./N', $c, 1);
      unless ($keep{lc ($n)})
        {
          $nokeep++;
        }
    }

  # Insert this DATA directive only if it is required
  if ($nokeep)
    {
      $p->insertBefore ($_, $acc) for ($acc->cloneNode (1), &t (' '), $data, &t ($indent));
     
      my $q = $pe->nextSibling;
     
      $p->insertBefore ($_, $q) for (&t ($indent), $acc->cloneNode (1), &t (' '), &n ('<end-data-openacc>END DATA</end-data-openacc>'));
    }

  # Move PRIVATE directive from ACC PARALLEL to the inner LOOP GANG directives

  for my $private (grep { my ($N) = &F ('./N', $_, 1); lc ($N) eq 'private' } &F ('./clause', $pa))
    { 
      if ($type eq 'serial')
        {
          $private->unbindNode ();
        }
      else
        {
          &movePrivateDirectiveToLoops ($pa, $pe, $private);
        }
    }
}

sub movePrivateDirectiveToLoops
{
  my ($pa, $pe, $private) = @_; # ACC PARALLEL, ACC END PARALLEL, PRIVATE (...)

  my $p = $pa->parentNode;

  # Gather temporarity all nodes between ACC PARALLEL and ACC END PARALLEL in an XML node

  my $pp = &n ('<pp/>');

  $p->insertBefore ($pp, $pa);

  for my $s ($pa, &F ('following-sibling::node()', $pa))
    {
      $pp->appendChild ($s);
      last if ($s->unique_key == $pe->unique_key);
    }

  for my $loop (&F ('.//loop-openacc', $pp))
    {
      my @c = map { lc ($_) } &F ('./clause/N/text()', $loop, 1);
      if (grep { $_ eq 'gang' } @c)
        {
          $loop->appendChild ($_) for (&t (' '), $private->cloneNode (1));
        }
    }

  for my $s (&F ('./node()', $pp))
    {
      $p->insertBefore ($s, $pp);
    }

  $pp->unbindNode ();

  $private->unbindNode ();
}

# An !$ACC PARALLEL LOOP may be ended by an !$ACC END PARALLEL (without LOOP); remove these !$ACC END PARALLEL
# Remove also trailing !$ACC END PARALLEL LOOP directive (if any)
sub cleanOpenACCParallelLoop
{
  my $pl = shift;

  my @n =&F ('following-sibling::node()', $pl);

  # Skip text nodes
  shift (@n) while (@n && ($n[0]->nodeName eq '#text'));

  # This must be a do-construct
  die unless (@n && (shift (@n)->nodeName eq 'do-construct'));

  # Skip text nodes
  shift (@n) while (@n && ($n[0]->nodeName eq '#text'));

  if (@n && $n[0]->nodeName eq 'acc')
    {
      my ($acc, $space, $openacc) = @n;
      if ($openacc->nodeName eq 'end-parallel-openacc')
        {
          $_->unbindNode () for ($acc, $space, $openacc);
        }
      if ($openacc->nodeName eq 'end-parallel-loop-openacc')
        {
          $_->unbindNode () for ($acc, $space, $openacc);
        }
    }
}

1;
