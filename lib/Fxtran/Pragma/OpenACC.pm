package Fxtran::Pragma::OpenACC;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Pragma::OpenACC

=head1 DESCRIPTION

Pragma class for generating and manipulating OpenACC directives in
FORTRAN source code. This class derives from C<Fxtran::Pragma> and
provides methods to insert, expand, and clean up C<!$ACC> directives in
an fxtran XML document tree.

The following directive-insertion methods are available:

=over 4

=item C<insertParallelLoopGang>

Inserts a C<!$ACC PARALLEL LOOP GANG> directive before a given node.

=item C<insertParallelLoopGangVector>

Inserts a C<!$ACC PARALLEL LOOP GANG VECTOR> directive before a given node.

=item C<insertData>

Inserts a C<!$ACC DATA> / C<!$ACC END DATA> pair around a given node.

=item C<insertLoopVector>

Inserts a C<!$ACC LOOP VECTOR> directive before a given node.

=item C<insertRoutineVector>

Inserts a C<!$ACC ROUTINE (...) VECTOR> annotation inside a subroutine.

=item C<insertRoutineSeq>

Inserts a C<!$ACC ROUTINE (...) SEQ> annotation inside a subroutine.

=item C<insertSerial>

Inserts a C<!$ACC SERIAL> / C<!$ACC END SERIAL> pair around a given node.

=back

Data-movement helper methods (C<enterDataCreate>, C<exitDataDelete>,
C<updateDevice>, C<enterDataAttach>, C<exitDataDetach>) return the
corresponding C<!$ACC> directive strings.

The C<expandParallelLoop> and C<expandParallelData> utility functions
split a combined C<!$ACC PARALLEL LOOP> directive into separate
C<!$ACC PARALLEL> and C<!$ACC LOOP> directives, and extract data clauses
into a dedicated C<!$ACC DATA> region respectively.

=cut

use base qw (Fxtran::Pragma);

use Data::Dumper;
use List::MoreUtils qw (uniq);

use strict;

use Fxtran;
use fxtran;
use fxtran::parser;

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

my $PARSER = 'fxtran::parser'->new ();
$PARSER->setOptions (qw (Fragment -openacc));

sub openacc
{
  return $PARSER->parse (fragment => $_[0]);
}

sub findParallel
{
  my ($self, $node) = @_;
  return &F ('.//parallel-loop-openacc', $node);
}

sub copyin
{
  my $self = shift;
  (undef, undef, my $acc) = &openacc ('!$ACC DATA COPYIN (' . join (', ', @_) . ')' . "\n");
  my ($c) = &F ('./clause', $acc);
  return $c;
}

sub sentinel
{
  my $self = shift;
  return &n ('<acc>!$ACC</acc>');
}

sub insertDirectiveBefore
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
          $d[$i] = $d[$i] . ' & '
        }
      if ($i > 0)
        {
          $d[$i] = ' & ' . $d[$i];
        }
      $d[$i] = "!\$ACC$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  my $dir = join ("\n", @d, '');

  for my $x (&openacc ($dir))
    {
      $P->insertBefore ($x, $p);
    }
}

sub insertDirectiveAfter
{
  my ($p, $d) = @_;
  $p->parentNode->insertAfter ($_, $p) for (reverse (&openacc ($d)), &t ("\n"));
}

sub insertParallelLoopGang
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'PARALLEL LOOP GANG', %c);
}

sub insertParallelLoopGangVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'PARALLEL LOOP GANG VECTOR', %c);
}

sub insertData
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'DATA', %c);
  &insertDirectiveAfter ($p, '!$ACC END DATA');
}

sub insertLoopVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'LOOP VECTOR', %c);
}

sub insertRoutineVector
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  &insertDirectiveAfter ($d->firstChild, "!\$ACC ROUTINE ($N) VECTOR");
}

sub insertRoutineSeq
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  &insertDirectiveAfter ($d->firstChild, "!\$ACC ROUTINE ($N) SEQ");
}

sub insertSerial
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'SERIAL', %c);
  &insertDirectiveAfter ($p, '!$ACC END SERIAL');
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

=head1 SEE ALSO

L<Fxtran::Pragma>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
