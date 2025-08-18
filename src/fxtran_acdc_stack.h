#ifndef _STACK_N
#define _STACK_N

!
! Copyright 2025 Meteo-France
! All rights reserved
! philippe.marguinaud@meteo.fr
!

USE FXTRAN_ACDC_ABORT_MOD

#ifdef __PGI
#define fxtran_acdc_stack_init(ydstack, ibl, nbl, ...) FXTRAN_ACDC_STACK (0, 0, 0, 0); CALL FXTRAN_ACDC_STACK_INIT (ydstack, YFXTRAN_ACDC_STACK, ibl, nbl, ##__VA_ARGS__)
#endif

#define fxtran_acdc_temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)

#define fxtran_acdc_assoc(p,q) IP_##p##_ = LOC(q)

#define fxtran_acdc_nullptr(p) IP_##p##_ = 0

#define fxtran_acdc_stack_alloc(n) CALL FXTRAN_ACDC_STACK_ALLOC (YLSTACK, IP_##n##_, SIZE (n, KIND=8), KIND (n), __FILE__, __LINE__)

#define fxtran_acdc_alloc4(n) IP_##n##_=YLSTACK%L4;YLSTACK%L4=YLSTACK%L4+4*SIZE(n,KIND=8);IF(YLSTACK%L4>YLSTACK%U4)CALL FXTRAN_ACDC_ABORT(__FILE__)
#define fxtran_acdc_alloc8(n) IP_##n##_=YLSTACK%L8;YLSTACK%L8=YLSTACK%L8+8*SIZE(n,KIND=8);IF(YLSTACK%L8>YLSTACK%U8)CALL FXTRAN_ACDC_ABORT(__FILE__)

#define fxtran_acdc_malign(p,k) ((((p)+(k)-1)/(k)) * (k))

#define fxtran_acdc_stack_l4(ydstack,ibl,nbl) fxtran_acdc_malign(LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8), ydstack%IALIGN) 
#define fxtran_acdc_stack_u4(ydstack,ibl,nbl)                   (LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8))
  
#define fxtran_acdc_stack_l8(ydstack,ibl,nbl) fxtran_acdc_malign(LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8), ydstack%IALIGN) 
#define fxtran_acdc_stack_u8(ydstack,ibl,nbl)                   (LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8))
 
#define fxtran_acdc_stack_l4_base(ydstack,ibl,nbl,ydstackbase) \
  fxtran_acdc_malign(ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8), ydstack%IALIGN) 
#define fxtran_acdc_stack_u4_base(ydstack,ibl,nbl,ydstackbase) \
                     (ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8))

#define fxtran_acdc_stack_l8_base(ydstack,ibl,nbl,ydstackbase) \
  fxtran_acdc_malign(ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8), ydstack%IALIGN) 
#define fxtran_acdc_stack_u8_base(ydstack,ibl,nbl,ydstackbase) \
                    (ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8))

#endif
