MODULE RANDOM_NUMBERS_MIX
USE PARKIND1    , ONLY : JPIM, JPRB
USE YOMHOOK     , ONLY : LHOOK, DR_HOOK


!**** *RANDOM_NUMBERS_MIX*  - Portable Random Number Generator

!     Purpose.
!     --------
!           Generate machine-independent pseudo-random numbers

!**   Interface.
!     ----------
!        CALL initialize_random_numbers (kseed, yd_stream)
!        CALL uniform_distribution      (px   , yd_stream)
!        CALL gaussian_distribution     (px   , yd_stream)
!        CALL random_number_restartfile (fname, action)
!        CALL wr_rangen_state           (nunit)

!        Explicit arguments :
!        --------------------
!        kseed  (input)    : integer seed in the range [0,HUGE(kseed)]
!        yd_stream (optional) : the state of the random number generator
!        px     (output)   : array to receive random numbers in the range

!        In the case of uniform_distribution, px has values in the range [0.0,1.0)

!        Implicit arguments :
!        --------------------
!        None

!     Method.
!     -------
!        Based loosly on ZUFALL (Petersen, 1994).

!        The main difference between this generator and ZUFALL is that integer arithmetic
!        is used. This ensures portability to vector machines that implement different
!        real arithmetic. In particular, vector machines often implement non-IEEE
!        arithmetic for their vector pipes. This routine will give identical results for
!        any integer type with at least 32 bits.

!        The generator is a lagged-Fibonacci generator: x(i) = x(i-p) + x(i-q) mod 2**m.
!        Lagged-Fibonacci generators have very long repeat periods: (2**q -1) * 2**(m-1)
!        (i.e about 2.85E191 for q=607, m=30). They pass most tests for randomness.

!        p and q must be chosen carefully. Values from the following table are OK.
!        Larger values give better random numbers, but smaller values are more
!        cache-friendly.

!          q         p
!        9689      4187
!        4423      2098
!        2281      1029
!        1279       418
!         607       273
!         521       168
!         250       103
!         127        63
!          97        33
!          55        24
!          43        22
!          31        13
!          24        10

!        The initial q values of x are set using the binary shirt register method of
!        Burns and Pryor 1999.

!        Mascagni et al (1995) show how to choose different sets of initial values that
!        are guaranteed to be drawn from different maximal-length cycles. This requires
!        the initial values of x(1)...x(q) to be in "canonical form". Specifically,
!        x(1) must be zero and all but a particular one or two values of x must be
!        even. For q=607 and p=273, only one element (jpq-jps) must be odd.

!     Externals.
!     ----------
!        None

!     Reference.
!     ----------
!        Burns P.J. and Pryor D.V. 1999,
!                             Surface Radiative Transport at Large Scale via Monte Carlo.
!                             Annual Review of Heat Transfer, Vol 9.
!
!        Petersen W.P., 1994, Lagged Fibonacci Series Random Number Generator
!                             for the NEC SX-3. International Journal of High Speed Computing
!                             Vol. 6, No. 3, pp387-398.
!
!        Mascagni M., Cuccaro S.A., Pryor D.V., Robinson M.L., 1995,
!                             A Fast, High Quality and Reproducible Parallel Lagged-Fibonacci
!                             Pseudorandom Number Generator. Journal of Computational Physics
!                             Vol 119. pp211-219.

!     Author.
!     -------
!        Mike Fisher *ECMWF*

!     Modifications.
!     --------------
!        Original : 2002-09-25
!        Made parallel friendly: 2003-08-11 Robert Pincus
!        M Leutbecher: 2004-05-10 restart capability
!        M Fisher:     2005-03-30 replaced LCG initialization with shift register
!        F. Vana:      2015-12-17 single precision version 
!     ------------------------------------------------------------------

IMPLICIT NONE

SAVE

PRIVATE
PUBLIC RANDOMNUMBERSTREAM

INTEGER(KIND=JPIM), PARAMETER      :: JPP=273, JPQ=607, JPS=105
INTEGER(KIND=JPIM), PARAMETER      :: JPMM=30
INTEGER(KIND=JPIM), PARAMETER      :: JPM=2**JPMM
INTEGER(KIND=JPIM), PARAMETER      :: JPNUMSPLIT=(JPQ-2)/(JPP-1)
INTEGER(KIND=JPIM), PARAMETER      :: JPLENSPLIT=(JPQ-JPP+JPNUMSPLIT-1)/JPNUMSPLIT
INTEGER(KIND=JPIM), PARAMETER      :: INITVALUE = 12345678

TYPE RANDOMNUMBERSTREAM
  PRIVATE
  INTEGER(KIND=JPIM)                 :: IUSED
  INTEGER(KIND=JPIM)                 :: INITTEST ! Should initialize to zero, but can't in F90
  INTEGER(KIND=JPIM), DIMENSION(JPQ) :: IX 
  REAL(KIND=JPRB)                    :: ZRM
END TYPE RANDOMNUMBERSTREAM

CONTAINS
END MODULE RANDOM_NUMBERS_MIX
