package Pointer::Parallel::OpenACCSingleColumn;

use strict;
use Pointer::Parallel;
use Fxtran;
use DIR;
use Loop;
use OpenACC;
use Data::Dumper;

sub getDefaultWhere
{
  'device';
}

sub makeParallel
{
  shift;
  my ($par, $t) = @_;

  &DIR::removeDIR ($par);

  my ($do) = &F ('./do-construct', $par);

  die unless ($do);

  &Loop::removeJlonConstructs ($par);


  my @x = &F ('./node()', $do);

  for my $x (@x)
    {
      $x->unbindNode ();
    }

  my ($do_jlon) = &Fxtran::parse (fragment => << 'EOF');
DO JLON = 1, MIN (YDCPG_OPTS%KLON, YDCPG_OPTS%KGPCOMP - (JBLK - 1) * YDCPG_OPTS%KLON)
ENDDO
EOF

  my $in_do_jlon = 0;

  my $indent = 0;

  while (my $x = shift (@x))
    {

      if (($x->nodeName eq 'call-stmt') && ($x->textContent =~ m/^CALL YLCPG_BNDS%UPDATE/o))
        {
          $do->appendChild ($do_jlon);
          $indent = &Fxtran::getIndent ($do_jlon);

          $do->insertBefore (&t ("\n" . (' ' x $indent)), $do_jlon);
          $do->insertBefore (&t ("\n" . (' ' x $indent)), $do_jlon);

          $do_jlon->insertAfter (&s ("YLSTACK%U = stack_u (YSTACK, JBLK, YDCPG_OPTS%KGPBLKS)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLSTACK%L = stack_l (YSTACK, JBLK, YDCPG_OPTS%KGPBLKS)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);

          $do_jlon->insertAfter (&s ("YLCPG_BNDS%KFDIA = JLON"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLCPG_BNDS%KIDIA = JLON"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);


          $in_do_jlon = 1;
          $do->appendChild (shift (@x));
          next;
        }
      elsif ($x->nodeName eq 'end-do-stmt')
        {
          $do_jlon->insertBefore (&t (' ' x $indent), $do_jlon->lastChild);
          $do->appendChild ($x);
        }
      elsif ($in_do_jlon)
        {
          $do_jlon->insertBefore ($x, $do_jlon->lastChild);
        }
      else
        {
          $do->appendChild ($x);
        }

    }


  for my $N (&F ('.//named-E[R-LT/array-R/section-subscript-LT[string(section-subscript[1])=":"]]/N', $do))
    {
      next unless ($t->{$N->textContent}{nproma});    # Skip non-NPROMA stuff
      my $expr = $N->parentNode;
      next if ($expr->parentNode->nodeName eq 'arg'); # Skip routine arguments
      my ($ss) = &F ('./R-LT/array-R/section-subscript-LT/section-subscript[1]', $expr);
      $ss->replaceNode (&n ('<section-subscript><lower-bound><named-E><N><n>JLON</n></N></named-E></lower-bound></section-subscript>'));
    }

  my @call = &F ('.//call-stmt', $do_jlon);

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      $proc->setData ($proc->textContent . '_OPENACC');
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (','));
      $argspec->appendChild (&n ('<arg><arg-N><k>YDSTACK</k></arg-N>=<named-E><N><n>YLSTACK</n></N></named-E></arg>'));
    }

  my @NPROMA = sort grep { $t->{$_}{nproma} } &F ('.//named-E/N', $do_jlon, 1);

  my ($do_jblk) = &F ('./do-construct/do-stmt[string(do-V)="JBLK"]', $par);
  my @priv = &Pointer::Parallel::getPrivateVariables ($do_jlon, $t);
  my %priv = map { ($_, 1) } @priv;

  my @const = grep { ! $priv{$_} } &Pointer::Parallel::getConstantObjects ($do_jlon, $t);


  &OpenACC::parallelLoopGang ($do_jblk, 
                              PRIVATE => ['JBLK'], 
                              PRESENT => [@NPROMA, @const, 'YSTACK'], 
                              VECTOR_LENGTH => ['YDCPG_OPTS%KLON']);


  &OpenACC::loopVector ($do_jlon, PRIVATE => \@priv);

  return $par;
}

1;