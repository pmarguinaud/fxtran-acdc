package Fxtran::SingleBlock;

=head1 NAME

Fxtran::SingleBlock

=head1 DESCRIPTION

This transformation takes a C<NPROMA> routine as input, and produces
a routine which processes a single block (the interface of the routine
is not changed). The result routine will execute on CPU and spawn 
OpenACC kernels; one kernel per loop on the C<NPROMA> dimension
or on the vertical dimension. Loop on other dimensions can also be included in 
kernels (as long as they contain some loops on the C<NPROMA> dimension).

C<NPROMA> array arguments and derived type arguments (such as C<YDMODEL>) are
supposed to be on the device when the result routine is invoked.

OpenACC kernels can be merged, using a number of statements heuristic.

The singleblock transformation is compatible with the singlecolumn transformation
(ie OpenACC kernels can contain calls to singlecolumn routines).

=head1 EXAMPLE

The following routine:

  SUBROUTINE SIGAM (YDCST,YDGEOMETRY,YDDYN,KLON,KLEV,KIDIA,KFDIA,PD,PT,PSP)

  USE GEOMETRY_MOD , ONLY : GEOMETRY
  USE PARKIND1     , ONLY : JPIM, JPRB
  USE YOMHOOK      , ONLY : LHOOK, DR_HOOK, JPHOOK
  USE YOMCST       , ONLY : TCST
  USE YOMDYN       , ONLY : TDYN
  
  IMPLICIT NONE
  
  TYPE(TCST)        ,INTENT(IN)    :: YDCST            ! constant data
  TYPE(GEOMETRY)    ,INTENT(IN)    :: YDGEOMETRY       ! constant data
  TYPE(TDYN)        ,INTENT(IN)    :: YDDYN            ! constant data
  INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV             ! scalar argument
  INTEGER(KIND=JPIM),INTENT(IN)    :: KLON             ! scalar argument
  INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA            ! scalar argument
  INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA            ! scalar argument
  REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD(KLON,KLEV)    ! NPROMA data
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV)    ! NPROMA data
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PSP(KLON)        ! NPROMA data
  
  REAL(KIND=JPRB) :: ZSPHI(KLON,0:KLEV+1)              ! NPROMA temporary data
  REAL(KIND=JPRB) :: ZOUT(KLON,KLEV)                   ! NPROMA temporary data
  
  REAL(KIND=JPRB) :: ZSPHIX(KLON)                      ! NPROMA temporary data
  INTEGER(KIND=JPIM) :: JLEV, JLON
  CHARACTER(LEN=4):: CLOPER

  CLOPER='IBOT'
  IF (YDCVER%LVFE_COMPATIBLE) CLOPER='INTG'

  !$OMP PARALLEL PRIVATE(JLEV,JLON)
  !$OMP DO SCHEDULE(STATIC) 
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        ZSPHI(JLON,JLEV)=-YDCST%RD*PT(JLON,JLEV)*SILNPR(JLEV)*YDVETA%VFE_RDETAH(JLEV)     ! NPROMA calculations
      ENDDO
    ENDDO
  !$OMP END DO
  !$OMP END PARALLEL
  
    ZSPHI(KIDIA:KFDIA,0)=0.0_JPRB                                                         ! NPROMA calculations
    ZSPHI(KIDIA:KFDIA,KLEV+1)=0.0_JPRB                                                    ! NPROMA calculations

    CALL VERDISINT(YDVFE,YDCVER,CLOPER,'11',KLON,KIDIA,KFDIA,KLEV,ZSPHI,ZOUT,KCHUNK=YDGEOMETRY%YRDIM%NPROMA) ! call to another NPROMA routine
  
  !$OMP PARALLEL PRIVATE(JLEV,JLON)
  !$OMP DO SCHEDULE(STATIC) 
    DO JLEV=1,KLEV
      DO JLON=KIDIA,KFDIA
        PD(JLON,JLEV)=ZOUT(JLON,JLEV)+PSP(JLON)*SIRPRG                                    ! NPROMA calculations
      ENDDO
    ENDDO
  !$OMP END DO
  !$OMP END PARALLEL

is transformed to:

  SUBROUTINE SIGAM_SINGLEBLOCK (YDCST, YDGEOMETRY, YDDYN&
  &, KLON, KLEV, KIDIA, KFDIA, PD, PT, PSP, LDACC)                ! Routine interface is not changed, except for the extra LDACC argument

  USE GEOMETRY_MOD,ONLY:GEOMETRY
  USE PARKIND1,ONLY:JPIM, JPRB
  USE YOMHOOK,ONLY:LHOOK, DR_HOOK, JPHOOK
  USE YOMCST,ONLY:TCST
  USE YOMDYN,ONLY:TDYN
  USE FXTRAN_ACDC_STACK_MOD
  #include "fxtran_acdc_stack.h"
  
  IMPLICIT NONE
  
  TYPE (TCST), INTENT (IN)::YDCST
  TYPE (GEOMETRY), INTENT (IN)::YDGEOMETRY
  TYPE (TDYN), INTENT (IN)::YDDYN
  INTEGER (KIND=JPIM), INTENT (IN)::KLEV
  INTEGER (KIND=JPIM), INTENT (IN)::KLON
  INTEGER (KIND=JPIM), INTENT (IN)::KIDIA
  INTEGER (KIND=JPIM), INTENT (IN)::KFDIA
  REAL (KIND=JPRB), INTENT (OUT)::PD (KLON, KLEV)
  REAL (KIND=JPRB), INTENT (IN)::PT (KLON, KLEV)
  REAL (KIND=JPRB), INTENT (IN)::PSP (KLON)
  LOGICAL, INTENT (IN) :: LDACC
  REAL (KIND=JPRB)::ZSPHI (KLON, 0:KLEV+1)
  REAL (KIND=JPRB)::ZOUT (KLON, KLEV)
  REAL (KIND=JPRB)::ZSPHIX (KLON)
  INTEGER (KIND=JPIM)::JLON
  INTEGER (KIND=JPIM)::JLEV
  CHARACTER (LEN=4)::CLOPER
  REAL (KIND=JPHOOK)::ZHOOK_HANDLE
  #include "verdisint.intfb.h"
  TYPE (FXTRAN_ACDC_STACK) :: YLSTACK
  
  !$ACC DATA &
  !$ACC&CREATE (ZOUT, ZSPHI, ZSPHIX) &                   ! Create NPROMA temporary data
  !$ACC&IF (LDACC) &
  !$ACC&PRESENT (PD, PSP, PT, YDCST, YDDYN, YDGEOMETRY)  ! State arguments are on device
  
  IF (LHOOK) CALL DR_HOOK ('SIGAM_SINGLEBLOCK', 0, ZHOOK_HANDLE)
  
  IF (YDGEOMETRY%YRVERT_GEOM%YRCVER%LVERTFE) THEN
    CLOPER='IBOT'
  
    IF (YDGEOMETRY%YRVERT_GEOM%YRCVER%LVFE_COMPATIBLE)  THEN
      CLOPER='INTG'
    ENDIF
    
    !$ACC PARALLEL LOOP GANG VECTOR &   ! OpenACC kernel (active if LDACC is true)
    !$ACC&IF (LDACC) &
    !$ACC&PRIVATE (JLEV, JLON) 

    DO JLON = KIDIA, KFDIA              ! loop switch (analog to single column)
      DO JLEV=1, KLEV
        ZSPHI (JLON, JLEV)=-YDCST%RD*PT (JLON, JLEV)*YDDYN%SILNPR&
        &(JLEV)*YDGEOMETRY%YRVERT_GEOM%YRVETA%VFE_RDETAH (JLEV)
      ENDDO
      ZSPHI (JLON, 0)=0.0_JPRB          ! Single line array syntax merged with previous kernel
      ZSPHI (JLON, KLEV+1)=0.0_JPRB     ! Single line array syntax merged with previous kernel
    ENDDO
    
    CALL VERDISINT_SINGLEBLOCK (YDGEOMETRY%YRVERT_GEOM%YRVFE, YDGEOMETRY%YRVERT_GEOM%YRCVER, CLOPER&  ! Call to another singleblock routine
    &,'11', KLON, KIDIA, KFDIA, KLEV, ZSPHI, ZOUT, KCHUNK=YDGEOMETRY%YRDIM%NPROMA, LDACC=LDACC)
    
    !$ACC PARALLEL LOOP GANG VECTOR &   ! OpenACC kernel
    !$ACC&IF (LDACC) &
    !$ACC&PRIVATE (JLEV, JLON) 
  
    DO JLON = KIDIA, KFDIA
      DO JLEV=1, KLEV
        PD (JLON, JLEV)=ZOUT (JLON, JLEV)+PSP (JLON)*YDDYN%SIRPRG
      ENDDO
    ENDDO

=head1 AUTHOR

philippe.marguinaud@meteo.fr

=head1 COPYRIGHT

Meteo-France 2025

=cut

use Data::Dumper;

use strict;

use Fxtran::Common;
use Fxtran::Loop;
use Fxtran::Stack;
use Fxtran::Call;
use Fxtran::Subroutine;
use Fxtran::Pragma;
use Fxtran::Finder;
use Fxtran::Style;
use Fxtran::Decl;
use Fxtran::DetectParallel;
use Fxtran;

sub processSingleRoutine
{
  return __PACKAGE__->processSingleRoutineMethod (@_);
}

sub processSingleRoutineMethod
{
  my $class = shift;
  my ($pu, %opts) = @_;

  my $find = $opts{find};

  # Process abort sections
  for my $abort (&F ('.//abort-section', $pu))
    {
      $abort->replaceNode (&s ("CALL ABOR1 ('NOT SUPPORTED')"));
    }

  my $style = $opts{style};
  my $pragma = $opts{pragma};

  my ($dp) = &F ('./specification-part/declaration-part', $pu);
  my ($ep) = &F ('./execution-part', $pu);
  
  my @nproma = $style->nproma ();
  
  my $jlon = $style->jlon ();
  my $kidia = $style->kidia ();
  my $kfdia = $style->kfdia ();
  
  # Arrays dimensioned with KLON and their dimensions

  my ($var2dim, $var2pos) = &Fxtran::Loop::getVarToDim ($pu, style => $style);

  &Fxtran::DetectParallel::createParallelSections ($pu, $var2dim, %opts) if ($opts{'max-statements-per-parallel'});

  my %pointer;

  {
  # Derived types, assume they are present on the device
  my @type = &F ('./T-decl-stmt[./_T-spec_/derived-T-spec]/EN-decl-LT/EN-decl/EN-N', $dp, 1);
  my %type = map { ($_, 1) } @type;
  
  my @arg = &F ('./subroutine-stmt/dummy-arg-LT/arg-N', $pu, 1);
  my %arg = map { ($_, 1) } @arg;
  
  my @present = grep { $var2dim->{$_} || $type{$_} } @arg;
  my @create;

  for my $n (sort (keys (%$var2dim)))
    {
      next if ($arg{$n});
      if (&F ('./T-decl-stmt[./attribute[string(./attribute-N)="POINTER"]][./EN-decl-LT/EN-decl[string(EN-N)="?"]]', $n, $dp))
        {
          $pointer{$n} = 1;
        }
      else
        {
          push @create, $n;
        }
    }
  
  # Create local arrays, assume argument arrays are on the device
  $pragma->insertData ($ep, PRESENT => \@present, CREATE => \@create, IF => ['LDACC']);
  }

  # Parallel sections

  my @par = &F ('.//parallel-section', $pu);

  for my $par (@par)
    {

      # Make the section single column

      &Fxtran::Loop::removeNpromaLoopsInSection
      (
        $par, 
        style => $style, 
        var2dim => $var2dim,
        var2pos => $var2pos,
      );
     
      # Move section contents into a DO loop over KLON

      my ($do) = &fxtran::parse (fragment => << "EOF");
DO $jlon = $kidia, $kfdia
ENDDO
EOF
  
      for my $x ($par->childNodes ())
        {
          $do->insertBefore ($x, $do->lastChild);
        }
  
      $par->replaceNode ($do);    
  
      # Use a stack

      if (&Fxtran::Stack::addStackInSection ($do))
        {
          &Fxtran::Stack::iniStackSingleBlock ($do, stack84 => 1);
        }

      # Replace KIDIA/KFDIA by JLON in call statements

      for my $call (&F ('.//call-stmt', $do))
        {
          for my $var ($kidia, $kfdia)
            {
              for my $expr (&F ('.//named-E[string(.)="?"]', $var, $call))
                {
                  $expr->replaceNode (&e ($jlon));
                }
            }
        }

      # Add single column suffix to routines called in this section

      &Fxtran::Call::addSuffix 
      (
        $pu,
        section => $do,
        suffix => $opts{'suffix-singlecolumn'},
        'merge-interfaces' => $opts{'merge-interfaces'},
      );
  
      # Find private variables & pointers used in section
      my (%private, %present);
      for my $expr (&F ('.//named-E', $do))
        {
          my ($n) = &F ('./N', $expr, 1);
          if ($var2dim->{$n})
            {
              $present{$n} = 1 if ($pointer{$n});
              next;
            }
          my $p = $expr->parentNode;
          $private{$n}++ if (($p->nodeName eq 'E-1') || ($p->nodeName eq 'do-V'));
        }

      # Add OpenACC directive  

      $class->makeParallel 
        (
          $pu, $do, %opts,
          present => [sort (keys (%present))], 
          private => [sort (keys (%private))], 
          var2dim => $var2dim,
          var2pos => $var2pos,
        );
    }
  
  # Add single block suffix to routines not called from within parallel sections

  &Fxtran::Call::addSuffix 
  (
    $pu,
    suffix => $opts{'suffix-singleblock'},
    'merge-interfaces' => 1,
    match => sub 
    { 
      my $proc = shift; 

      return if ($proc eq 'ABOR1');
      return if ($proc eq 'MXMAOPTR');
      return if ($proc eq 'GSTATS');

      if ($opts{'suffix-singlecolumn'})
        {
          return if ($proc =~ m/$opts{'suffix-singlecolumn'}$/i);
        }

      for my $s (&F ('ancestor::ANY-section', $proc))
        {
          my $nn = $s->nodeName;
          return if ($nn eq 'parallel-section');
          return if ($nn eq 'horizontal-section');
        }

      return 1;
    },
  );

  &Fxtran::Subroutine::addSuffix ($pu, $opts{'suffix-singleblock'});
  
  my ($implicit) = &F ('.//implicit-none-stmt', $pu);
  
  $implicit->parentNode->insertBefore (&n ('<include>#include "<filename>fxtran_acdc_stack.h</filename>"</include>'), $implicit);
  $implicit->parentNode->insertBefore (&t ("\n"), $implicit);
 
  &Fxtran::Decl::declare ($pu, 
                          'TYPE (FXTRAN_ACDC_STACK) :: YLSTACK',
                          'INTEGER :: ' . $jlon);

  &Fxtran::Decl::use ($pu, 'USE FXTRAN_ACDC_STACK_MOD');

  
  my ($arglist) = &F ('./dummy-arg-LT', $pu->firstChild);

  unless (&F ('./arg-N[string(.)="LDACC"]', $arglist))
    {
      $arglist->appendChild ($_) for (&t (', '), &n ('<arg-N><N><n>LDACC</n></N></arg-N>'));
      my @decl = &F ('./T-decl-stmt[./attribute[string(attribute-N)="INTENT"]]', $dp);
      $dp->insertAfter ($_, $decl[-1]) for (&s ("LOGICAL, INTENT (IN) :: LDACC"), &t ("\n"));
     }

  for my $call (&F ('.//call-stmt', $pu))
    {
      my ($proc) = &F ('./procedure-designator', $call, 1);
      next unless (($proc =~ m/$opts{'suffix-singleblock'}$/) || ($proc eq 'MXMAOPTR'));
      my ($argspec) = &F ('./arg-spec', $call);
      $argspec->appendChild ($_) for (&t (', '), &n ('<arg><arg-N n="LDACC"><k>LDACC</k></arg-N>=' . &e ('LDACC') . '</arg>'));
    }
  
}

sub makeParallel
{
  my $class = shift;
  my ($pu, $do, %opts) = @_;

  my $pragma = $opts{pragma};
  my $present = $opts{present} || [];
  my $private = $opts{private} || [];

  $pragma->insertParallelLoopGangVector ($do, PRESENT => $present, PRIVATE => $private, IF => ['LDACC']);
}

1;
