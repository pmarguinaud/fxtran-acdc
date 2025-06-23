#ifndef _STACK_H
#define _STACK_H

USE ABOR1_ACC_MOD
USE PARKIND1, ONLY : JPRB
USE STACK_MOD

#ifdef _OPENACC
USE OPENACC, ONLY : ACC_DEVICEPTR
#endif

#include "stack.macros.h"

#endif
