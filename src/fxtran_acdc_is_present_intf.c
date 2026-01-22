#ifdef _OPENACC
#include <openacc.h>
#endif

#include <stdlib.h>

void fxtran_acdc_is_present_intf (void * kptr, size_t ksize, int * kpresent)
{
  *kpresent = 0;

#ifdef _OPENACC
  *kpresent = acc_is_present (kptr, ksize);
#endif
}
