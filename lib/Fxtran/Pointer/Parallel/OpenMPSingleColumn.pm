package Fxtran::Pointer::Parallel::OpenMPSingleColumn;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use Data::Dumper;

use strict;

use Fxtran::Pointer::Parallel;
use Fxtran;
use Fxtran::DIR;
use Fxtran::Loop;
use Fxtran::ReDim;
use Fxtran::Stack;
use Fxtran::Print;

sub getAddBlockIndex
{
  1;
}

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
  my ($par, $t, %opts) = @_;

  my $style = $par->getAttribute ('style');
  $style = $style ? 'Fxtran::Style'->new (style => $style) : $opts{style};

  my @priv = &Fxtran::Pointer::Parallel::getPrivateVariables ($par, $t);

  my ($do) = &F ('.//do-construct[./do-stmt[string(do-V)="JBLK"]]', $par);

  my @firstprivate;

  if (my $it = $style->customIterator ())
    {
      push @firstprivate, $it;
      @priv = grep { $_ ne $it } @priv;
    }

  my $C = &n ('<C>!$OMP PARALLEL DO PRIVATE (' .  join (', ', @priv)  . ')' . 
              (@firstprivate ? ' FIRSTPRIVATE (' . join (', ', @firstprivate) . ')' : '') . 
              '</C>');
  
  $do->parentNode->insertBefore ($C, $do);
  $do->parentNode->insertBefore (&t ("\n"), $do);

}

sub makeParallel
{
  shift;
  my ($pu, $par1, $t, %opts) = @_;

  my $style = $par1->getAttribute ('style');
  $style = $style ? 'Fxtran::Style'->new (style => $style) : $opts{style};

  my $FILTER = $par1->getAttribute ('filter');

  &Fxtran::DIR::removeDIR ($par1);

  my ($do) = &F ('./do-construct', $par1);

  die unless ($do);

  &Fxtran::Loop::removeNpromaConstructs ($par1, %opts);

  my @call = &F ('.//call-stmt', $do);

  my $stackRequired = 0;

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator', $call, 1);
      next if ($proc eq 'YLCPG_BNDS%UPDATE');
      $stackRequired = 1;
      last;
    }

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

  my $jlon = $opts{style}->jlon ();

  my ($do_jlon) = &Fxtran::parse (fragment => << "EOF");
DO $jlon = 1, MIN ($KLON, $KGPTOT - (JBLK - 1) * $KLON)
ENDDO
EOF

  my $in_do_jlon = 0;

  while (my $x = shift (@x))
    {

      if (($x->nodeName eq 'call-stmt') && ($x->textContent =~ m/^CALL YLCPG_BNDS%UPDATE/o))
        {
          $do->appendChild ($do_jlon);

          $do->insertBefore (&t ("\n"), $do_jlon);
          $do->insertBefore (&t ("\n"), $do_jlon);

          &Fxtran::Stack::iniStackManyBlocks ($do_jlon, stack84 => $opts{stack84}, JBLKMIN => $JBLKMIN, 
                                              KGPBLKS => $KGPBLKS, 'stack-method' => $opts{'stack-method'})
            if ($stackRequired);
          
          if ($style->customIterator ())
            {
              $style->updateCustomIterator ($do_jlon->firstChild);
            }

          $do_jlon->insertAfter (&s ("YLCPG_BNDS%KFDIA = $jlon"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLCPG_BNDS%KIDIA = $jlon"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);

          $in_do_jlon = 1;
          $do->appendChild (shift (@x));
          next;
        }
      elsif ($x->nodeName eq 'end-do-stmt')
        {
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
      $ss->replaceNode (&n ("<section-subscript><lower-bound><named-E><N><n>$jlon</n></N></named-E></lower-bound></section-subscript>"));
    }


  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator/named-E/N/n/text()', $call);
      next if ($proc eq 'ABOR1');
      next if ($proc eq 'PCRC');
      $proc->setData ($proc->textContent . $opts{'suffix-singlecolumn'});
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (','));
      $argspec->appendChild (&n ('<arg><arg-N><k>YDSTACK</k></arg-N>=<named-E><N><n>YLSTACK</n></N></named-E></arg>'));
    }

  &Fxtran::Print::useABOR1_ACC ($do_jlon);

  &setOpenMPDirective ($par1, $t, %opts);

  &Fxtran::ReDim::redimArguments ($par1) if ($opts{'redim-arguments'});

  return $par1;
}

1;
