package Fxtran::Spectral;

use Data::Dumper;

use strict;

use Fxtran;

sub processSingleRoutine
{
  my ($pu, %opts) = @_;

  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  # Arrays

  my %arr = map { ($_, 1) } 
            &F ('./T-decl-stmt/EN-decl-LT/EN-decl[string(./array-spec/shape-spec-LT/shape-spec[last()])="KSTA:KEND"]/EN-N', $dp, 1);

  # Non argument scalars

  my %sca = map { ($_, 1) }
            &F ('./T-decl-stmt[not(./attribute[string(attribute-N)="INTENT"])]/EN-decl-LT/EN-decl[not(./array-spec)]/EN-N', $dp, 1);

  # Argument derived types

  my %typ = map { ($_, 1) } 
            &F ('./T-decl-stmt[_T-spec_/derived-T-spec][string(./attribute/attribute-N)="INTENT"]/EN-decl-LT/EN-decl[not(./array-spec)]/EN-N', $dp, 1);

  # Arguments

  my %arg = map { ($_, 1) } 
            &F ('./T-decl-stmt[string(./attribute/attribute-N)="INTENT"]/EN-decl-LT/EN-decl/EN-N', $dp, 1);

  my @present = grep ({ $arg{$_} } sort (keys (%arr)), sort (keys (%typ)));
  my @create = grep { ! $arg{$_} } sort (keys (%arr));

  my @do_jsp = &F ('.//do-construct[./do-stmt[string(do-V)="JSP"]]', $ep);

  for my $do_jsp (@do_jsp)
    {
      my $collapse;

      if (my @do_jlev = &F ('.//do-construct[./do-stmt[string(do-V)="JLEV"]]', $do_jsp))
        {
          for my $do_jlev (@do_jlev)
            {
              my @n = &F ('./node()', $do_jlev); 
              shift (@n); shift (@n); pop (@n); pop (@n);
              my $p =$do_jlev->parentNode;
              for my $n (@n)
                {
                  $p->insertBefore ($n, $do_jlev);
                }
              $do_jlev->unbindNode ();
            }

          my ($do_jlev) = &Fxtran::parse (fragment => << 'EOF');
DO JLEV = 1, YDGEOMETRY%YRDIMV%NFLEVL
!
ENDDO
EOF
          my ($C) = &F ('./C', $do_jlev);

          my $p = $C->parentNode;

          my @n = &F ('./node()', $do_jsp);
          shift (@n); shift (@n); my $end = pop (@n);

          for my $n (@n)
            {
              $p->insertBefore ($n, $C);
            }

          $C->unbindNode ();

          $end->parentNode->insertBefore ($_, $end) for ($do_jlev, &t ("\n"));

          $collapse = 1;
        }

      my (%present, %private);

      for my $N (&F ('.//named-E/N', $do_jsp))
        {
          my $e = $N->parentNode;
          my $p = $e->parentNode;
          my $n = $N->textContent;
          if ($sca{$n})
            {
              $private{$n} = 1 if ($p->nodeName eq 'E-1');
              $private{$n} = 1 if ($p->nodeName eq 'do-V');
            }
        }

      $pragma->insertParallelLoopGangVector 
        ($do_jsp, 
         PRIVATE => [sort (keys (%private))], 
         ($collapse ? (COLLAPSE => [2]) : ()),
         IF => ['LDACC']);
    }

  $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create, IF => ['LDACC']);

  my ($arglist) = &F ('./subroutine-stmt/dummy-arg-LT', $pu);

  $arglist->appendChild ($_) for (&t (", "), &n ('<arg-N><N><n>LDACC</n></N></arg-N>'));
  my @decl = &F ('./T-decl-stmt[string(./attribute/attribute-N)="INTENT"]', $dp);
  $dp->insertAfter ($_, $decl[-1]) for (&s ("LOGICAL, INTENT (IN) :: LDACC"), &t ("\n"));
  
}

1;
