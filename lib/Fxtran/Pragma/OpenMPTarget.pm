package Fxtran::Pragma::OpenMPTarget;

#
# Copyright 2025 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#

=head1 NAME

Fxtran::Pragma::OpenMPTarget

=head1 DESCRIPTION

Pragma class for generating OpenMP Target directives in FORTRAN source
code. This class derives from C<Fxtran::Pragma> and provides the same
interface as C<Fxtran::Pragma::OpenACC> but emits C<!$OMP> directives
instead of C<!$ACC> directives. It is intended to be used as a drop-in
replacement when targeting OpenMP offloading instead of OpenACC.

The following directive-insertion methods are available:

=over 4

=item C<insertParallelLoopGang>

Inserts a C<!$OMP TARGET TEAMS DISTRIBUTE> directive before a given node.

=item C<insertParallelLoopGangVector>

Inserts a C<!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD> directive
before a given node.

=item C<insertData>

Inserts a C<!$OMP TARGET DATA> / C<!$OMP END TARGET DATA> pair around a
given node.

=item C<insertLoopVector>

Inserts a C<!$OMP PARALLEL DO SIMD> directive before a given node.

=item C<insertRoutineSeq>

Inserts a C<!$OMP DECLARE TARGET> annotation inside a subroutine.

=item C<insertSerial>

Inserts a C<!$OMP TARGET> / C<!$OMP END TARGET> pair around a given node.

=back

Data-movement helper methods (C<enterDataCreate>, C<exitDataDelete>,
C<updateDevice>, C<enterDataAttach>, C<exitDataDetach>) return the
corresponding C<!$OMP TARGET> directive strings.

=cut

use base qw (Fxtran::Pragma);

use List::MoreUtils qw (uniq);
use Data::Dumper;

use strict;

use Fxtran;
use fxtran;
use fxtran::parser;

my $PARSER = 'fxtran::parser'->new ();
$PARSER->setOptions (qw (Fragment -openmp-target));

sub openmptarget
{
  return $PARSER->parse (fragment => $_[0]);
}

sub findParallel
{
  my ($self, $node) = @_;
  return &F ('.//target-teams-openmp[clause[string(.)="DISTRIBUTE"]', $node);
}

sub copyin
{
  my $self = shift;
  (undef, undef, my $acc) = &openmptarget ('!$OMP TARGET DATA  MAP(TO:' . join (', ', @_) . ')' . "\n");
  my ($c) = &F ('./clause', $acc);
  return $c;
}

sub sentinel
{
  my $self = shift;
  return &n ('<omptarget>!$OMP</omptarget>');
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
              if ($c eq 'CREATE')
                {$d[-1] = "MAP(ALLOC:" . $d[-1];}
              elsif ($c eq 'PRESENT')

# PRESENT does not work with NVIDIA (not implemented), we would need to use TO instead

                {$d[-1] = "MAP(PRESENT:" . $d[-1];}
              else
                {$d[-1] = "$c (" . $d[-1];}
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
      $d[$i] = "!\$OMP$d[$i]";
    }

  $P->insertBefore (&t ("\n"), $p);

  my $dir = join ("\n", @d, '');

  for my $x (&openmptarget ($dir))
    {
      $P->insertBefore ($x, $p);
    }
}

sub insertDirectiveAfter
{
  my ($p, $d) = @_;
  $p->parentNode->insertAfter ($_, $p) for (reverse (&openmptarget ($d)), &t ("\n"));
}

sub insertParallelLoopGang
{
  shift;
  my ($p, %c) = @_;
  $c{THREAD_LIMIT} = delete $c{VECTOR_LENGTH};
  &insertDirectiveBefore ($p, 'TARGET TEAMS DISTRIBUTE', %c);
}

sub insertParallelLoopGangVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD', %c);
}

sub insertData
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'TARGET DATA ', %c);
  &insertDirectiveAfter ($p, '!$OMP END TARGET DATA');
}

sub insertLoopVector
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'PARALLEL DO SIMD', %c);
}

sub insertRoutineVector
{
  shift;
  die;
}

sub insertRoutineSeq
{
  shift;
  my $d = shift;
  my ($N) = &F ('./subroutine-stmt/subroutine-N', $d, 1); 
  &insertDirectiveAfter ($d->firstChild, '!$OMP DECLARE TARGET');
}

sub insertSerial
{
  shift;
  my ($p, %c) = @_;
  &insertDirectiveBefore ($p, 'TARGET', %c);
  &insertDirectiveAfter ($p, '!$OMP END TARGET');
}

sub enterDataCreate
{
  shift;
  return @_ ? '!$OMP TARGET ENTER DATA MAP (ALLOC: ' . join (', ', @_) . ')' : '';
}

sub exitDataDelete
{
  shift;
  return @_ ? '!$OMP TARGET EXIT DATA MAP (DELETE: ' . join (', ', @_) . ')' : '';
}

sub updateDevice
{
  shift;
  return @_ ? '!$OMP TARGET UPDATE TO (' . join (', ', @_) . ')' : '';
}

sub enterDataAttach
{
  shift;
  return @_ ? '!$OMP TARGET ENTER DATA MAP (TO: ' .  join (', ', @_) . ')' : '';
}

sub exitDataDetach
{
  shift;
  return @_ ? '!$OMP TARGET EXIT DATA MAP (RELEASE: ' .  join (', ', @_) . ')' : '';
}

=head1 SEE ALSO

L<Fxtran::Pragma>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
