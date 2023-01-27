#include <sys/time.h>
#include <time.h>

void get_time_ (double * t)
{
  struct timeval tv;
  gettimeofday (&tv, NULL);
  *t = tv.tv_sec + ((double)tv.tv_usec)/1e6;
}
