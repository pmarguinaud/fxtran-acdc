INTERFACE
SUBROUTINE ARO_GROUND_DIAG_Z0( KBL, KLON, KST, KEND, & 
           & KDGUNG, KDGUXG,KDLUNG, KDLUXG,&
           & PINDX, PINDY, &
           & PGZ0, PGZ0H)

USE PARKIND1, ONLY : JPRB, JPIM

INTEGER(KIND=JPIM),                 INTENT(IN)  :: KBL       ! NPROMA-packat number
INTEGER(KIND=JPIM),                 INTENT(IN)  :: KLON      ! horizontal dimension
INTEGER(KIND=JPIM),                 INTENT(IN)  :: KST       ! start of physical values
INTEGER(KIND=JPIM),                 INTENT(IN)  :: KEND      ! end of physical values
                                                  ! bottom to top, -1 otherwise
INTEGER(KIND=JPIM),                 INTENT(IN)  :: KDGUNG, KDGUXG,KDLUNG, KDLUXG !dim param
                                                  ! to fill E-Zone
REAL(KIND=JPRB), DIMENSION(KLON),   INTENT(IN)  :: PINDX     ! x coordinate on the plane
REAL(KIND=JPRB), DIMENSION(KLON),   INTENT(IN)  :: PINDY     ! y coordinate on the plane
REAL(KIND=JPRB), DIMENSION(KLON),   INTENT(OUT) :: PGZ0      ! rougness lenght for momentum
REAL(KIND=JPRB), DIMENSION(KLON),   INTENT(OUT) :: PGZ0H     ! rouhness length for heat 

END SUBROUTINE ARO_GROUND_DIAG_Z0
END INTERFACE
