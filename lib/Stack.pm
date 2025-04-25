package Stack;

#
# Copyright 2022 Meteo-France
# All rights reserved
# philippe.marguinaud@meteo.fr
#


use Fxtran;
use strict;
use Data::Dumper;
use Scope;

sub iniStack
{
  my ($do_jlon, $indent, $stack84, $JBLKMIN, $KGPBLKS) = @_;

  if ($stack84)
    {
      for my $size (4, 8)
        {
          $do_jlon->insertAfter (&s ("YLSTACK%U${size} = stack_u${size} (YSTACK, JBLK-$JBLKMIN+1, $KGPBLKS)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLSTACK%L${size} = stack_l${size} (YSTACK, JBLK-$JBLKMIN+1, $KGPBLKS)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
        }
    }
  else
    {
      $do_jlon->insertAfter (&s ("YLSTACK%U = stack_u (YSTACK, JBLK-$JBLKMIN+1, $KGPBLKS)"), $do_jlon->firstChild);
      $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
      $do_jlon->insertAfter (&s ("YLSTACK%L = stack_l (YSTACK, JBLK-$JBLKMIN+1, $KGPBLKS)"), $do_jlon->firstChild);
      $do_jlon->insertAfter (&t ("\n" . (' ' x $indent)), $do_jlon->firstChild);
    }


}

sub addStack
{
  my ($d, %opts) = @_;

  my @KLON = @{ $opts{KLON} || [qw (KLON YDCPG_OPTS%KLON)] };

  my @pointer = @{ $opts{pointer} || [] };

  my $skip = $opts{skip};
  my $local = exists $opts{local} ? $opts{local} : 1;

  my ($up) = &F ('./specification-part/use-part', $d);
  my ($dp) = &F ('./specification-part/declaration-part', $d);
  my ($ep) = &F ('./execution-part', $d);

  my @call = $ep ? &F ('.//call-stmt', $ep) : ();

  for my $call (@call)
    {
      my ($proc) = &F ('./procedure-designator', $call, 1);
      next if ($proc eq 'DR_HOOK');
      next if ($proc eq 'ABOR1');
      next if ($proc =~ m/%/o);
      next if ($skip && $skip->($proc, $call));

      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild (&t (', '));

      my $arg = &n ('<arg/>');

      $arg->appendChild (&n ('<arg-N n="YDSTACK"><k>YDSTACK</k></arg-N>'));
      $arg->appendChild (&t ('='));
      $arg->appendChild (&e ('YLSTACK'));

      $argspec->appendChild ($arg);
    }

  my ($dummy_arg_lt) = &F ('./subroutine-stmt/dummy-arg-LT', $d);

  my @args = &F ('./arg-N', $dummy_arg_lt, 1);

  my $last = $args[-1];

  $dummy_arg_lt->appendChild (&t (', '));
  $dummy_arg_lt->appendChild (&n ("<arg-N><N><n>YDSTACK</n></N></arg-N>"));

  for my $n (&n ("<include>#include &quot;<filename>stack.h</filename>&quot;</include>"), &s ("USE STACK_MOD"), &s ("USE ABOR1_ACC_MOD"))
    {
      $up->appendChild (&t ("\n"));
      $up->appendChild ($n);
    }

  my ($decl) = &F ('./T-decl-stmt[.//EN-N[string(.)="?"]]', $last, $dp);

  if ($local)
    {
      $dp->insertAfter (&s ("TYPE(STACK) :: YLSTACK"), $decl);
      $dp->insertAfter (&t ("\n"), $decl);
    }

  $dp->insertAfter (&s ("TYPE(STACK) :: YDSTACK"), $decl);
  $dp->insertAfter (&t ("\n"), $decl);

  return unless ($local && $ep);
  
  my $assignstack = &s ("YLSTACK = YDSTACK");

  for (&t ("\n"), &t ("\n"), $assignstack, &t ("\n"))
     {
       $ep->insertBefore ($_, $ep->firstChild);
     }

  my %args = map { ($_, 1) } @args;

  for my $KLON (@KLON)
    {
      my @en_decl = &F ('./T-decl-stmt'
                      . '//EN-decl[./array-spec/shape-spec-LT/shape-spec[string(./upper-bound)="?"]]', 
                      $KLON, $dp);
      

      for my $en_decl (reverse (@en_decl))
        {
          my ($n) = &F ('./EN-N', $en_decl, 1);

          next if ($args{$n});

          my $stmt = &Fxtran::stmt ($en_decl);
      
          my ($t) = &F ('./_T-spec_',   $stmt);     &Fxtran::expand ($t); $t = $t->textContent;
          my ($s) = &F ('./array-spec', $en_decl);  &Fxtran::expand ($s); $s = $s->textContent;
      
          
          $stmt->replaceNode (&s ("temp ($t, $n, $s)"));
      
          if (! grep { $n eq $_ } @pointer)
            {
              if ($opts{stack84})
                {
                  my ($if) = &fxtran::parse (fragment => << "EOF");
IF (KIND ($n) == 8) THEN
  alloc8 ($n)
ELSEIF (KIND ($n) == 4) THEN
  alloc4 ($n)
ELSE
  STOP 1
ENDIF
EOF
                 $ep->insertAfter (&t ("\n"), $assignstack);
                 $ep->insertAfter ($if, $assignstack);
               }
             else
               {
                 $ep->insertAfter (&t ("\nalloc ($n)"), $assignstack);
               }

           }

        }

    }

  $ep->insertAfter (&t ("\n"), $assignstack);
}

1;
