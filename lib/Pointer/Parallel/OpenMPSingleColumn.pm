package Pointer::Parallel::OpenMPSingleColumn;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use strict;
use Pointer::Parallel;
use Fxtran;
use DIR;
use Loop;
use Data::Dumper;
use ReDim;
use Stack;
use Print;

sub getDefaultWhere
{
  'host';
}

sub onlySimpleFields
{
  0;
}

sub requireUtilMod
{
  0;
}

sub setOpenMPDirective
{
  my ($par, $t) = @_;

  my $style = $par->getAttribute ('style') || 'ARPIFS';

  my @priv = &Pointer::Parallel::getPrivateVariables ($par, $t);

  my ($do) = &F ('.//do-construct[./do-stmt[string(do-V)="JBLK"]]', $par);

  my $indent = &Fxtran::getIndent ($do);

  my $firstprivate = '';

  if ($style eq 'MESONH')
    {
      $firstprivate = " FIRSTPRIVATE (D)";
      @priv = grep { $_ ne 'D' } @priv;
    }

  my $C = &n ('<C>!$OMP PARALLEL DO PRIVATE (' . join (', ', @priv)  . ')' . $firstprivate . '</C>');
  
  $do->parentNode->insertBefore ($C, $do);
  $do->parentNode->insertBefore (&t ("\n" . (' ' x $indent)), $do);

}

sub makeParallel
{
  shift;
  my ($par1, $t, %opts) = @_;

  my $style = $par1->getAttribute ('style') || 'ARPIFS';
  my $FILTER = $par1->getAttribute ('filter');

  &DIR::removeDIR ($par1);

  my ($do) = &F ('./do-construct', $par1);

  die unless ($do);

  &Loop::removeJlonConstructs ($par1);
  &Loop::removeJlonArraySyntax ($par1);


  my @x = &F ('./node()', $do);

  for my $x (@x)
    {
      $x->unbindNode ();
    }

  my ($KLON, $KGPTOT, $KGPBLKS, $JBLKMIN);

  if ($FILTER)
    {
      $KLON    = 'YL_FGS%KLON';
      $KGPTOT  = 'YL_FGS%KGPTOT';
      $KGPBLKS = 'YL_FGS%KGPBLKS';
      $JBLKMIN = '1';
    }
  else
    {
      $KLON    = 'YDCPG_OPTS%KLON';
      $KGPTOT  = 'YDCPG_OPTS%KGPCOMP';
      $KGPBLKS = 'YDCPG_OPTS%KGPBLKS';
      $JBLKMIN = 'YDCPG_OPTS%JBLKMIN';
    }

  my ($do_jlon) = &Fxtran::parse (fragment => << "EOF");
DO JLON = 1, MIN ($KLON, $KGPTOT - (JBLK - 1) * $KLON)
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

          &Stack::iniStack ($do_jlon, $indent, $opts{stack84}, $JBLKMIN, $KGPBLKS);
          
          if ($style eq 'MESONH')
            {
              $do_jlon->insertAfter (&s ("D%NIE = JLON"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
              $do_jlon->insertAfter (&s ("D%NIB = JLON"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);

              $do_jlon->insertAfter (&s ("D%NIJE = JLON"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
              $do_jlon->insertAfter (&s ("D%NIJB = JLON"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);

            }

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
      next unless ($t->{$N->textContent}{isFieldAPI});    # Skip non-NPROMA stuff
      my $expr = $N->parentNode;
      next if ($expr->parentNode->nodeName eq 'arg'); # Skip routine arguments
      my ($ss) = &F ('./R-LT/array-R/section-subscript-LT/section-subscript[1]', $expr);
      $ss->replaceNode (&n ('<section-subscript><lower-bound><named-E><N><n>JLON</n></N></named-E></lower-bound></section-subscript>'));
    }


  my @call = &F ('.//call-stmt', $do_jlon);

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      next if ($proc eq 'ABOR1');
      $proc->setData ($proc->textContent . '_OPENACC');
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (','));
      $argspec->appendChild (&n ('<arg><arg-N><k>YDSTACK</k></arg-N>=<named-E><N><n>YLSTACK</n></N></named-E></arg>'));
    }

  &Print::useABOR1_ACC ($do_jlon);

  &setOpenMPDirective ($par1, $t);

  &ReDim::redimArguments ($par1) if ($opts{'redim-arguments'});

  return $par1;
}

1;
