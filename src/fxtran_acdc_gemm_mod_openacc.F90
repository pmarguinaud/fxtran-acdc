MODULE FXTRAN_ACDC_GEMM_MOD_OPENACC

!
! Copyright 2025 Meteo-France
! All rights reserved
! philippe.marguinaud@meteo.fr
!

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMM_OPENACC

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMM_OPENACC (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, &
                                   & LDA, B, LDB, BETA, C, LDC, LDDONE, YDSTACK)

!$acc routine seq

USE FXTRAN_ACDC_STACK_MOD
USE FXTRAN_ACDC_ABORT_MOD

                                             ! VERINT
INTEGER                  :: KIDIA
INTEGER                  :: KFDIA
CHARACTER*1              :: TRANSA           ! 'N'
CHARACTER*1              :: TRANSB           ! 'T'
INTEGER                  :: M                ! KPROMA
INTEGER                  :: N                ! KLEVOUT-1  if verder/verint, 1 if verints  
INTEGER                  :: K                ! KLEVIN
REAL*8                   :: ALPHA            ! 1.0_JPRD
REAL*8                   :: A (LDA, *)       ! ZIN
INTEGER                  :: LDA              ! KPROMA
REAL*8                   :: B (LDB, *)       ! PINTE
INTEGER                  :: LDB              ! KLEVOUT
REAL*8                   :: BETA             ! 0.0_JPRB
REAL*8                   :: C (LDC, *)       ! ZOUT
INTEGER                  :: LDC              ! KPROMA
LOGICAL                  :: LDDONE
TYPE (FXTRAN_ACDC_STACK) :: YDSTACK

INTEGER :: JM, JN, JK

IF (TRANSA /= 'N') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: TRANSA /= N')
IF (TRANSB /= 'T') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: TRANSB /= T')

JM = KIDIA

DO JN = 1, N
  C (JM, JN) = 0.
ENDDO
DO JK = 1, K
  DO JN = 1, N
    C (JM, JN) = C (JM, JN) + B (JN, JK) * A (JM, JK)
  ENDDO
ENDDO

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
