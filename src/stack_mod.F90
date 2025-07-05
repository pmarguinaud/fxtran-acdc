MODULE STACK_MOD

IMPLICIT NONE

TYPE STACK_DATA
  REAL (KIND=4), POINTER :: ZDATA4 (:,:,:,:)
  REAL (KIND=8), POINTER :: ZDATA8 (:,:,:,:)
  INTEGER*8 :: IALIGN
CONTAINS
END TYPE

TYPE STACK
  INTEGER*8 :: L4
  INTEGER*8 :: U4
  INTEGER*8 :: L8
  INTEGER*8 :: U8
END TYPE

TYPE (STACK_DATA) :: YSTACK

PRIVATE
PUBLIC :: STACK, YSTACK, STACK_BASE, STACK_INIT, STACK_ALLOC

CONTAINS

SUBROUTINE STACK_INIT (SELF, YDSTACK, IBL, NBL, YDSTACKBASE) 

!$acc routine seq

TYPE (STACK) :: SELF
TYPE (STACK_DATA), INTENT (IN) :: YDSTACK
INTEGER, INTENT (IN) :: IBL, NBL
TYPE (STACK), INTENT (IN), OPTIONAL :: YDSTACKBASE

INTEGER*8 :: MALIGN, P, K
MALIGN (P, K) = ((P+K-1)/K) * K

IF (PRESENT (YDSTACKBASE)) THEN

SELF%L4 = &
  malign(ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8), ydstack%IALIGN) 
SELF%U4 = &
        (ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8))

SELF%L8 = &
  malign(ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8), ydstack%IALIGN) 
SELF%U8 = &
        (ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8))

ELSE

SELF%L4 = &
  malign(LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8), ydstack%IALIGN) 
SELF%U4 = &
        (LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8))

SELF%L8 = &
  malign(LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8), ydstack%IALIGN) 
SELF%U8 = &
        (LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8))

ENDIF

END SUBROUTINE
 
SUBROUTINE STACK_ALLOC (SELF, KPTR, KSIZE, KKIND, CDFILE)

!$acc routine seq

USE ABOR1_ACC_MOD

TYPE (STACK) :: SELF
INTEGER*8 :: KPTR
INTEGER*8 :: KSIZE
INTEGER   :: KKIND
CHARACTER(LEN=*) :: CDFILE

SELECT CASE (KKIND)
  CASE (4) 

    KPTR = SELF%L4
   
    SELF%L4 = SELF%L4 + 4 * KSIZE
   
    IF (SELF%L4 > SELF%U4) GOTO 999

  CASE (8) 

    KPTR = SELF%L8

    SELF%L8 = SELF%L8 + 8 * KSIZE

    IF (SELF%L8 > SELF%U8) GOTO 999

  CASE DEFAULT

    GOTO 999

END SELECT

RETURN

999 CONTINUE

CALL ABOR1_ACC (CDFILE)

END SUBROUTINE

INTEGER*8 FUNCTION STACK_BASE (YDSTACK, KKIND, CDW, IBL, NBL, YDSTACKBASE)

!$acc routine seq

USE ABOR1_ACC_MOD

TYPE (STACK_DATA), INTENT (IN) :: YDSTACK
INTEGER, INTENT (IN) :: KKIND, IBL, NBL
CHARACTER :: CDW
TYPE (STACK), INTENT (IN) :: YDSTACKBASE

INTEGER*8 :: MALIGN, P, K
MALIGN (P, K) = ((P+K-1)/K) * K

STACK_BASE = 0

SELECT CASE (KKIND)

  CASE (4)
    SELECT CASE (CDW)
      CASE ('L')
        STACK_BASE = &
  malign(ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8), ydstack%IALIGN) 

      CASE ('U')
        STACK_BASE = &
        (ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8))

      CASE DEFAULT

        CALL ABOR1_ACC (__FILE__)
    END SELECT

  CASE (8)
    SELECT CASE (CDW)
      CASE ('L')
        STACK_BASE = &
  malign(ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8), ydstack%IALIGN) 
  
      CASE ('U')
        STACK_BASE = &
        (ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8))

      CASE DEFAULT

        CALL ABOR1_ACC (__FILE__)
    END SELECT

  CASE DEFAULT

    CALL ABOR1_ACC (__FILE__)
END SELECT

RETURN

END FUNCTION

END MODULE

