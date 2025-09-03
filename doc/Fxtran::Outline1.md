# NAME

[Fxtran::Outline1](../lib/Fxtran/Outline1.pm)

# DESCRIPTION

The purpose of this module is to move sections of code from a 
subroutine into external routines. These code sections are 
replaced by a call to the external routine.

# EXAMPLE

For instance the following routine:

    SUBROUTINE SUBR (YD, P, K)
    
    USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK
    USE YOMTT,ONLY:TT
    
    IMPLICIT NONE
    
    TYPE (TT), INTENT (INOUT)::YD
    REAL, INTENT (OUT)::P (K)
    INTEGER, INTENT (IN)::K
    
    INTEGER::I, J
    REAL :: ZZ
    
    REAL (KIND=JPHOOK) :: ZHOOK_HANDLE
    
    IF (LHOOK) CALL DR_HOOK ('SUBR', 0, ZHOOK_HANDLE)
    
    ZZ = 34.
    
    !$ACDC PARALLEL {
    
    IF (YD%LL) THEN
      P (:)=12.
    ENDIF
    
    !$ACDC }
    
    !$ACDC PARALLEL {
    
    DO I = 1, 100
      YD%ZZ (I) = 12. + ZZ
    ENDDO
    
    !$ACDC }
    
    IF (LHOOK) CALL DR_HOOK ('SUBR', 1, ZHOOK_HANDLE)
    
    END SUBROUTINE

is transformed into:

    SUBROUTINE SUBR (YD, P, K)
    USE YOMHOOK,ONLY:LHOOK, DR_HOOK, JPHOOK
    USE YOMTT,ONLY:TT
    
    IMPLICIT NONE
    
    TYPE (TT), INTENT (INOUT)::YD
    REAL, INTENT (OUT)::P (K)
    INTEGER, INTENT (IN)::K
    INTEGER::J
    
    REAL::ZZ
    REAL (KIND=JPHOOK)::ZHOOK_HANDLE
    #include "subr_outline_000.intfb.h"
    #include "subr_outline_001.intfb.h"
    IF (LHOOK) CALL DR_HOOK ('SUBR', 0, ZHOOK_HANDLE)
    ZZ=34.
    CALL SUBR_OUTLINE_000 (K, P, YD)
    CALL SUBR_OUTLINE_001 (ZZ, YD)
    IF (LHOOK) CALL DR_HOOK ('SUBR', 1, ZHOOK_HANDLE)
    END SUBROUTINE

and the following two routines are created in separate files, as well as
their interfaces:

    SUBROUTINE SUBR_OUTLINE_000 (K, P, YD) 
    USE YOMTT,ONLY:TT
    USE YOMHOOK,ONLY:LHOOK, JPHOOK, DR_HOOK
    
    IMPLICIT NONE
    
    INTEGER, INTENT (IN)::K
    REAL, INTENT (OUT)::P (K) 
    TYPE (TT), INTENT (INOUT)::YD
    REAL (KIND=JPHOOK)::ZHOOK_HANDLE
    IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_000', 0, ZHOOK_HANDLE)
    
    IF (YD%LL) THEN
      P (:)=12.
    ENDIF
    
    IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_000', 1, ZHOOK_HANDLE)
    END SUBROUTINE
    
    SUBROUTINE SUBR_OUTLINE_001 (PZ, YD)
    USE YOMTT,ONLY:TT
    USE YOMHOOK,ONLY:LHOOK, JPHOOK, DR_HOOK
    
    IMPLICIT NONE
    
    REAL, INTENT (INOUT)::PZ
    TYPE (TT), INTENT (INOUT)::YD
    INTEGER::I 
    REAL (KIND=JPHOOK)::ZHOOK_HANDLE
    IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_001', 0, ZHOOK_HANDLE)
    
    DO I=1, 100
      YD%ZZ (I)=12.+PZ
    ENDDO
    
    IF (LHOOK) CALL DR_HOOK ('SUBR_OUTLINE_001', 1, ZHOOK_HANDLE)
    END SUBROUTINE

# MORE EXAMPLES

Examples are provided with test data; for instance:

[apl\_arpege.F90](../tests/49t2_openacc-outline1/src/main/arpifs/phys_dmn/apl_arpege.F90)

and:

[apl\_arpege.F90](../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege.F90),
[apl\_arpege\_zde2mr.F90](../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege_zde2mr.F90),
[apl\_arpege\_zbay\_qrconv.F90](../tests/49t2_openacc-outline1/ref/outline1/src/local/arpifs/phys_dmn/apl_arpege_zbay_qrconv.F90),
etc ...

# AUTHOR

philippe.marguinaud@meteo.fr

# COPYRIGHT

Meteo-France 2025
