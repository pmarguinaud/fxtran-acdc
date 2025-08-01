#ifndef _STACK_N
#define _STACK_N

USE ABOR1_ACC_MOD

#ifndef __INTEL_COMPILER
#define stack_init(ydstack, ibl, nbl, ...) STACK (0, 0, 0, 0); CALL STACK_INIT (ydstack, YSTACK, ibl, nbl, ##__VA_ARGS__)
#endif

#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)

#define assoc(p,q) IP_##p##_ = LOC(q)

#define nullptr(p) IP_##p##_ = 0

#define stack_alloc(n) CALL STACK_ALLOC (YLSTACK, IP_##n##_, SIZE (n, KIND=8), KIND (n), __FILE__, __LINE__)

#define alloc4(n) IP_##n##_=YLSTACK%L4;YLSTACK%L4=YLSTACK%L4+4*SIZE(n,KIND=8);IF(YLSTACK%L4>YLSTACK%U4)CALL ABOR1_ACC(__FILE__)
#define alloc8(n) IP_##n##_=YLSTACK%L8;YLSTACK%L8=YLSTACK%L8+8*SIZE(n,KIND=8);IF(YLSTACK%L8>YLSTACK%U8)CALL ABOR1_ACC(__FILE__)

#define malign(p,k) ((((p)+(k)-1)/(k)) * (k))

#define stack_l4(ydstack,ibl,nbl) malign(LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8), ydstack%IALIGN) 
#define stack_u4(ydstack,ibl,nbl)       (LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4)) / INT (nbl, 8))
  
#define stack_l8(ydstack,ibl,nbl) malign(LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8), ydstack%IALIGN) 
#define stack_u8(ydstack,ibl,nbl)       (LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8)) / INT (nbl, 8))
 
#define stack_l4_base(ydstack,ibl,nbl,ydstackbase) \
  malign(ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8), ydstack%IALIGN) 
#define stack_u4_base(ydstack,ibl,nbl,ydstackbase) \
        (ydstackbase%L4 + LOC (ydstack%ZDATA4 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA4,KIND=8) * KIND (ydstack%ZDATA4) - ydstackbase%L4)) / INT (nbl, 8))

#define stack_l8_base(ydstack,ibl,nbl,ydstackbase) \
  malign(ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8) - 1) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8), ydstack%IALIGN) 
#define stack_u8_base(ydstack,ibl,nbl,ydstackbase) \
        (ydstackbase%L8 + LOC (ydstack%ZDATA8 (1,1,1,1)) + ((INT (ibl, 8)    ) * (SIZE (ydstack%ZDATA8,KIND=8) * KIND (ydstack%ZDATA8) - ydstackbase%L8)) / INT (nbl, 8))

#endif
