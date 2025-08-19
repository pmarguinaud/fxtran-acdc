MODULE FXTRAN_ACDC_GEMV_MOD_OPENACC

!
! Copyright 2025 Meteo-France
! All rights reserved
! philippe.marguinaud@meteo.fr
!

IMPLICIT NONE

PRIVATE

PUBLIC :: FXTRAN_ACDC_GEMV_OPENACC

CONTAINS

SUBROUTINE FXTRAN_ACDC_GEMV_OPENACC (KIDIA, KFDIA, TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC, LDDONE, YDSTACK)

!$acc routine seq

USE FXTRAN_ACDC_STACK_MOD
USE FXTRAN_ACDC_ABORT_MOD
                                             ! VERINTS
INTEGER                  :: KIDIA
INTEGER                  :: KFDIA
CHARACTER*1              :: TRANSA           ! 'N'
CHARACTER*1              :: TRANSB           ! 'T'
INTEGER                  :: M                ! KPROMA
INTEGER                  :: N                ! 1
INTEGER                  :: K                ! KLEVIN
REAL*8                   :: ALPHA            ! 1.0_JPRD
REAL*8                   :: A (:, :)         ! ZIN
INTEGER                  :: LDA              ! KPROMA
REAL*8                   :: B (:, :)         ! PINTE 
INTEGER                  :: LDB              ! KLEVOUT
REAL*8                   :: BETA             ! 0.0_JPRB
REAL*8                   :: C (:)            ! POUTS
INTEGER                  :: LDC              ! KPROMA
LOGICAL                  :: LDDONE
TYPE (FXTRAN_ACDC_STACK) :: YDSTACK

INTEGER :: JM, JN, JK

IF (TRANSA /= 'N') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: TRANSA /=N')
IF (TRANSB /= 'T') CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: TRANSB /=T')

IF (LDA /= SIZE (A, 1)) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: LDA /= SIZE (A, 1)')
IF (LDB /= SIZE (B, 1)) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: LDB /= SIZE (B, 1)')
IF (LDC /= SIZE (C, 1)) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: LDC /= SIZE (C, 1)')

IF (ALPHA /= 1._8) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: ALPHA /= 1._8')
IF (BETA /= 0._8) CALL FXTRAN_ACDC_ABORT ('FXTRAN_ACDC_GEMM: BETA /= 0._8')

JM = KIDIA

C (JM) = 0._8

DO JK = 1, K
  C (JM) = C (JM) + B (LDB, JK) * A (JM, JK)
ENDDO

LDDONE = .TRUE.

END SUBROUTINE

END MODULE
