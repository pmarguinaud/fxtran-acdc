
#include "fxtran_acdc_config.h"

#ifdef _FXTRAN_ACDC_USE_OPENACC
#include <openacc.h>
#endif

#include <stdlib.h>

void fxtran_acdc_is_present_intf (void * kptr, size_t ksize, int * kpresent)
{
  *kpresent = 0;

#ifdef _FXTRAN_ACDC_USE_OPENACC
  *kpresent = acc_is_present (kptr, ksize);
#endif
}
