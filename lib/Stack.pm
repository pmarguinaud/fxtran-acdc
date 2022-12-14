package Stack;

use Fxtran;
use strict;
use Data::Dumper;
use Scope;

sub addStack
{
  my ($d, %args) = @_;

  my $skip = $args{skip};
  my $local = exists $args{local} ? $args{local} : 1;

  my @call = &F ('.//call-stmt[string(procedure-designator)!="ABOR1" and string(procedure-designator)!="REDUCE"]', $d);

  my %contained = map { ($_, 1) } &F ('.//subroutine-N[count(ancestor::program-unit)>1]', $d, 1);

  my $YLSTACK = $local ? 'YLSTACK' : 'YDSTACK';

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator', $call, 1);
      next if ($proc eq 'DR_HOOK');
      next if ($contained{$proc});
      next if ($proc =~ m/%/o);
      if ($skip)
        {
          next if ($skip->($proc, $call));
        }
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (', '));
      $argspec->appendChild (&e ($YLSTACK));
    }

  my ($dummy_arg_lt) = &F ('.//subroutine-stmt/dummy-arg-LT', $d);

  my ($last) = &F ('./arg-N[last()]', $dummy_arg_lt, 1);

  $dummy_arg_lt->appendChild (&t (', '));
  $dummy_arg_lt->appendChild (&n ("<arg-N><N><n>YDSTACK</n></N></arg-N>"));

  my ($use) = &F ('.//use-stmt[last()]', $d);
  $use->parentNode->insertAfter (&n ("<include>#include &quot;<filename>stack.h</filename>&quot;</include>"), $use);
  $use->parentNode->insertAfter (&t ("\n"), $use);
  $use->parentNode->insertAfter (&s ("USE STACK_MOD"), $use);
  $use->parentNode->insertAfter (&t ("\n"), $use);


  my ($decl) = &F ('.//T-decl-stmt[.//EN-N[string(.)="?"]]', $last, $d);

  if ($local)
    {
      $decl->parentNode->insertAfter (&s ("TYPE(STACK) :: YLSTACK"), $decl);
      $decl->parentNode->insertAfter (&t ("\n"), $decl);
    }

  $decl->parentNode->insertAfter (&s ("TYPE(STACK) :: YDSTACK"), $decl);
  $decl->parentNode->insertAfter (&t ("\n"), $decl);

  
  my $noexec = &Scope::getNoExec ($d);

  my $C = &n ("<C/>");

  $noexec->parentNode->insertAfter (&t ("\n"), $noexec);
  $noexec->parentNode->insertAfter ($C, $noexec);

  if ($local)
    {
      $C->parentNode->insertBefore (&t ("\n"), $C);
      $C->parentNode->insertBefore (&t ("\n"), $C);
      $C->parentNode->insertBefore (&s ("YLSTACK = YDSTACK"), $C);
      $C->parentNode->insertBefore (&t ("\n"), $C);
      $C->parentNode->insertBefore (&t ("\n"), $C);
    }

  my @KLON = qw (KLON YDCPG_OPTS%KLON);

  for my $KLON (@KLON)
    {
      my @en_decl = &F ('.//T-decl-stmt[not(string(.//attribute-N)="INTENT")]'
                      . '//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(./upper-bound)="?"]]', 
                      $KLON, $d);
      
      for my $en_decl (@en_decl)
        {
          my ($n) = &F ('./EN-N', $en_decl, 1);

          my $stmt = &Fxtran::stmt ($en_decl);
      
          my ($t) = &F ('./_T-spec_',   $stmt);     &Fxtran::expand ($t); $t = $t->textContent;
          my ($s) = &F ('./array-spec', $en_decl);  &Fxtran::expand ($s); $s = $s->textContent;
      
          if ($local)
            {
              $stmt->parentNode->insertBefore (my $temp = &t ("temp ($t, $n, $s)"), $stmt);
      
              if (&Fxtran::removeListElement ($en_decl))
                {
                  $stmt->unbindNode ();
                }
              else
                {
                  $temp->parentNode->insertAfter (&t ("\n"), $temp);
                }
      
              $C->parentNode->insertBefore (&t ("alloc ($n)\n"), $C);
            }
          else
            {
              die "No local stack, but KLON arrays were found";
            }

        }

    }

  $C->unbindNode ();

}

1;
