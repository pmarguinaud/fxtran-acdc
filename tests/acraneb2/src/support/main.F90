PROGRAM MAIN

! read command line arguments (nproma, klev, ngpblk)

USE PARKIND1, ONLY : JPIM
USE WRAPPER_MOD
#ifdef _OPENACC
USE CUDAFOR
#endif

IMPLICIT NONE

INTEGER :: IARG
CHARACTER(LEN=256) :: CARG
INTEGER(KIND=JPIM) :: NPROMA,NLEV,NGPBLK,NCOUNT
LOGICAL :: LCHECK, LSAVE
INTEGER :: HEAPSIZE4
INTEGER :: ISTAT
INTEGER*8 :: HEAPSIZE

! default arguments
NPROMA=32
NLEV=87
NGPBLK=1
NCOUNT=1
LCHECK=.FALSE.
HEAPSIZE4=0

! parse arguments
IARG=1
DO WHILE ( IARG <= COMMAND_ARGUMENT_COUNT() )
  CALL GET_COMMAND_ARGUMENT(IARG,CARG)
	SELECT CASE (TRIM(CARG))
		CASE ('--heapsize')
			IARG=IARG+1
			CALL GET_COMMAND_ARGUMENT(IARG,CARG)
			READ(CARG,*) HEAPSIZE4
		CASE ('--nproma')
			IARG=IARG+1
			CALL GET_COMMAND_ARGUMENT(IARG,CARG)
			READ(CARG,*) NPROMA
		CASE ('--nlev')
			IARG=IARG+1
			CALL GET_COMMAND_ARGUMENT(IARG,CARG)
			READ(CARG,*) NLEV
		CASE ('--ncount')
			IARG=IARG+1
			CALL GET_COMMAND_ARGUMENT(IARG,CARG)
			READ(CARG,*) NCOUNT
		CASE ('--ngpblk')
			IARG=IARG+1
			CALL GET_COMMAND_ARGUMENT(IARG,CARG)
			READ(CARG,*) NGPBLK
		CASE ('--save')
		  LSAVE=.TRUE.
		CASE ('--check')
		  LCHECK=.TRUE.
		CASE ('--help')
			WRITE (*,*) 'Acraneb2 wrapper'
			WRITE (*,*)
			WRITE (*,*) '  acraneb2 is an atmospheric radiation parameterization scheme'
			WRITE (*,*)
			WRITE (*,*) '  A scientific description can be found in Geleyn et al., 2017 (doi:10.1002/qj.3006) '
			WRITE (*,*) '  and Masek et al., 2015 (doi: 10.1002/qj.2653).'
			WRITE (*,*)
			WRITE (*,*) '  Data for this wrapper are taken from a 1.3km resolution run with 87 vertical levels '
			WRITE (*,*) '  with a timestep of 60s, on August 1 2016. Forecast lead time is 12h.'
			WRITE (*,*) '  Intermittency is switched off.'
			WRITE (*,*)
			WRITE (*,*) 'Command line options:'
			WRITE (*,*) '  --help              show this help'
			WRITE (*,*) '  --nproma {nproma}   blocking size'
			WRITE (*,*) '  --nlev   {nlev}     number of vertical levels'
			WRITE (*,*) '  --ngpblk {ngpblk}   number of blocks'
			WRITE (*,*) '  --ncount {ncount}   number of repetitions'
			WRITE (*,*) '  --check             check results'
			STOP
		CASE DEFAULT
		  WRITE (*,*) 'WARNING:  unknown argument ',TRIM(CARG)
	END SELECT
	IARG=IARG+1
ENDDO

WRITE (*,*) 'Running acraneb2 wrapper with'
WRITE (*,*) '  nproma = ',NPROMA
WRITE (*,*) '  nlev   = ',NLEV
WRITE (*,*) '  ngpblk = ',NGPBLK
WRITE (*,*) '  ncount = ',NCOUNT
WRITE (*,*) '  lcheck = ',LCHECK
WRITE (*,*) '  lsave  = ',LSAVE

#ifdef _OPENACC
IF (HEAPSIZE4 > 0) THEN
HEAPSIZE = HEAPSIZE4*1024_8*1024_8
ISTAT = CUDADEVICESETLIMIT (CUDALIMITMALLOCHEAPSIZE, HEAPSIZE)
ENDIF
#endif


! call wrapper
CALL WRAPPER(NPROMA,NLEV,NGPBLK,NCOUNT,LCHECK,LSAVE)

END PROGRAM MAIN
