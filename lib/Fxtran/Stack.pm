package Fxtran::Stack;

=head1 NAME

Fxtran::Stack

=head1 DESCRIPTION

This module contains utilities to allocate temporary variables using the fxtran stack
data structures.

=head1 FUNCTIONS

=cut

use Data::Dumper;

use strict;

use Fxtran::Common;
use Fxtran::Scope;
use Fxtran;

sub iniStackSingleBlock
{

=head2 iniStackSingleBlock

Initialize the fxtran stack object at the top of an OpenACC kernel. This is the version
used for regular single-column kernels.

To be simplified when we enforce the C<stack-method> option.

=cut

  my ($do_jlon, %opts) = @_;

  if ($opts{'stack-method'})
    {
      die unless ($opts{stack84});
      $do_jlon->insertAfter ($_, $do_jlon->firstChild)
        for (&s ("YLSTACK = fxtran_acdc_stack_init (YLSTACK, 1, 1)"), &t ("\n"));
       
    }
  else
    {
      if ($opts{stack84})
        {
          for my $size (4, 8)
            {
              $do_jlon->insertAfter (&s ("YLSTACK%U${size} = fxtran_acdc_stack_u${size} (YFXTRAN_ACDC_STACK, 1, 1)"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&s ("YLSTACK%L${size} = fxtran_acdc_stack_l${size} (YFXTRAN_ACDC_STACK, 1, 1)"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
            }
        }
      else
        {
          $do_jlon->insertAfter (&s ("YLSTACK%U = fxtran_acdc_stack_u (YFXTRAN_ACDC_STACK, 1, 1)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLSTACK%L = fxtran_acdc_stack_l (YFXTRAN_ACDC_STACK, 1, 1)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
        }
    }


}

sub iniStackManyBlocks
{
  my ($do_jlon, %opts) = @_;

=head2 iniStackManyBlocks

Stack initialization for the manyblocks transformation. Use the number of blocks and the stack offset.

To be simplified when the option C<stack-method> is enforced.

=cut

  my ($JBLKMIN, $KGPBLKS, $YDOFFSET) = @opts{qw (JBLKMIN KGPBLKS YDOFFSET)};

  if ($opts{'stack-method'})
    {
      next unless ($opts{stack84});
      my $ydoffset = $YDOFFSET ? ", $YDOFFSET" : "";
      $do_jlon->insertAfter ($_, $do_jlon->firstChild)
        for (&s ("YLSTACK = fxtran_acdc_stack_init (YLSTACK, (JBLK-$JBLKMIN)+1, $KGPBLKS$ydoffset)"), &t ("\n"));
    }
  else
    {
      if ($opts{stack84})
        {
          for my $size (4, 8)
            {
              my $base = $YDOFFSET ? '_base' : ''; my $ydoffset = $YDOFFSET ? ", $YDOFFSET" : "";
              $do_jlon->insertAfter (&s ("YLSTACK%U${size} = fxtran_acdc_stack_u${size}${base} (YFXTRAN_ACDC_STACK, (JBLK-$JBLKMIN)+1, $KGPBLKS$ydoffset)"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&s ("YLSTACK%L${size} = fxtran_acdc_stack_l${size}${base} (YFXTRAN_ACDC_STACK, (JBLK-$JBLKMIN)+1, $KGPBLKS$ydoffset)"), $do_jlon->firstChild);
              $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
            }
        }
      else
        {
          my $base = $YDOFFSET ? '_base' : ''; my $ydoffset = $YDOFFSET ? ", $YDOFFSET" : "";
          $do_jlon->insertAfter (&s ("YLSTACK%U = fxtran_acdc_stack_u${base} (YFXTRAN_ACDC_STACK, (JBLK-$JBLKMIN)+1, $KGPBLKS$ydoffset)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&s ("YLSTACK%L = fxtran_acdc_stack_l${base} (YFXTRAN_ACDC_STACK, (JBLK-$JBLKMIN)+1, $KGPBLKS$ydoffset)"), $do_jlon->firstChild);
          $do_jlon->insertAfter (&t ("\n"), $do_jlon->firstChild);
        }
    }


}

sub addStackInSection
{

=head2 addStackInSection

Add the C<YDSTACK> argument to calls to calculation routines. Serial routines (we use a hard-wired list for
now) are excluded. This works on a code section.

=cut

  my ($s, %opts) = @_;

  my $skip = $opts{skip};

  my @call = $s ? &F ('.//call-stmt', $s) : ();

  my $count = 0;

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

      $count++;
    }

  return $count;
}

sub addStack
{
  my ($pu, %opts) = @_;

=head2 addStack

Add a stack object argument and local variable to the program unit passed as argument. Then 
pass the local stack to the calculation routines, and allocate temporaries within the local
stack frame (wrap their declaration with the C<fxtran_acdc_temp> macro and allocate with
stack methods.


  SUBROUTINE ACTKE (..., YDSTACK)

  ...                                              ! dummy arguments declaration

  TYPE (FXTRAN_ACDC_STACK), INTENT (IN) :: YDSTACK ! Stack argument

  TYPE (FXTRAN_ACDC_STACK) :: YLSTACK ! Local stack
  
  fxtran_acdc_temp (REAL (KIND=JPRB), ZTEMP, (KLON, KLEV))  ! Declare temporary with macro

  YLSTACK = YDSTACK          ! local stack frame setup
  
  fxtran_acdc_alloc (ZTEMP)  ! allocate in YLSTACK

  CALL ACTURB (..., YLSTACK) ! pass current stack pointer to ACTURB

=cut

  my @klon = $opts{style}->nproma ();

  my @pointer = @{ $opts{pointer} || [] };

  my ($up) = &F ('./specification-part/use-part', $pu);
  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);

  my $needYLSTACK = &addStackInSection ($ep, %opts);

  my ($dummy_arg_lt) = &F ('./subroutine-stmt/dummy-arg-LT', $pu);

  my @args = &F ('./arg-N', $dummy_arg_lt, 1);

  my $last = $args[-1];

  $dummy_arg_lt->appendChild (&t (', '));
  $dummy_arg_lt->appendChild (&n ("<arg-N><N><n>YDSTACK</n></N></arg-N>"));

  my ($declOfLastArg) = &F ('./T-decl-stmt[.//EN-N[string(.)="?"]]', $last, $dp);
  my $declOfYDSTACK = &s ("TYPE(FXTRAN_ACDC_STACK), INTENT (IN) :: YDSTACK");
  
  $dp->insertAfter ($declOfYDSTACK, $declOfLastArg);
  $dp->insertAfter (&t ("\n"), $declOfLastArg);

  $declOfLastArg = $declOfYDSTACK;

  for my $n (&n ("<include>#include &quot;<filename>fxtran_acdc_stack.h</filename>&quot;</include>"), &s ("USE FXTRAN_ACDC_STACK_MOD"), &s ("USE FXTRAN_ACDC_ABORT_MOD"))
    {
      $up->appendChild (&t ("\n"));
      $up->appendChild ($n);
    }

  return unless ($ep);

  my %args = map { ($_, 1) } @args;

  for my $klon (@klon)
    {
      my @en_decl = &F ('./T-decl-stmt/EN-decl-LT/EN-decl[./array-spec/shape-spec-LT/shape-spec[string(./upper-bound)="?"]]', $klon, $dp);

      for my $en_decl (reverse (@en_decl))
        {
          my ($n) = &F ('./EN-N', $en_decl, 1);

          next if ($args{$n});

          my $stmt = &Fxtran::stmt ($en_decl);
      
          my ($t) = &F ('./_T-spec_',   $stmt);     &Fxtran::expand ($t); $t = $t->textContent;
          my ($s) = &F ('./array-spec', $en_decl);  &Fxtran::expand ($s); $s = $s->textContent;
      
          
          $stmt->replaceNode (&s ("fxtran_acdc_temp ($t, $n, $s)"));
      
          if (! grep { $n eq $_ } @pointer)
            {
              if ($opts{stack84} && $opts{'stack-method'})
                {
                 $ep->insertBefore (&t ("\n"), $ep->firstChild);
                 $ep->insertBefore (&s ("fxtran_acdc_stack_alloc ($n)"), $ep->firstChild);
                }
              elsif ($opts{stack84})
                {
                  my ($if) = &fxtran::parse (fragment => << "EOF");
IF (KIND ($n) == 8) THEN
  fxtran_acdc_alloc8 ($n)
ELSEIF (KIND ($n) == 4) THEN
  fxtran_acdc_alloc4 ($n)
ELSE
  STOP 1
ENDIF
EOF
                 $ep->insertBefore (&t ("\n"), $ep->firstChild);
                 $ep->insertBefore ($if, $ep->firstChild);
               }
             else
               {
                 $ep->insertBefore (&t ("\nfxtran_acdc_alloc ($n)\n"), $ep->firstChild);
               }

              $needYLSTACK++;
           }

        }

    }


  if ($needYLSTACK)
    {
      my $assignstack = &s ("YLSTACK = YDSTACK");
                 
      for (&t ("\n"), &t ("\n"), $assignstack, &t ("\n"))
         {
           $ep->insertBefore ($_, $ep->firstChild);
         }

      $ep->insertAfter (&t ("\n"), $assignstack);
      $dp->insertAfter (&s ("TYPE(FXTRAN_ACDC_STACK) :: YLSTACK"), $declOfLastArg);
      $dp->insertAfter (&t ("\n"), $declOfLastArg);
    }


}

=head1 EXAMPLES

You can find examples in all single-column routines; for instance:

L<actke.F90|url:../tests/49t2_openacc-bench/src/main/arpifs/phys_dmn/actke.F90>
/
L<actke_openacc.F90|url:../tests/49t2_openacc-bench/ref/util/src/local/arpifs/phys_dmn/actke_openacc.F90>

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

1;
