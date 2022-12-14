#ifndef _STACK_N
#define _STACK_N

#ifdef USE_STACK

#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)

#define alloc(n) IP_##n##_=YLSTACK%L;YLSTACK%L=YLSTACK%L+JPRB*SIZE(n);IF(YLSTACK%L>YLSTACK%U)CALL STACK_ABORT(__FILE__,__LINE__)

#else

#define temp(t, n, s) t, DIMENSION s :: n
#define alloc(n)

#endif

#endif
