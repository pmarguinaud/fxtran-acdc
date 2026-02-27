#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "fxtran_acdc_config.h"

#ifdef _FXTRAN_ACDC_USE_OPENACC
#include <openacc.h>
#include <accel.h>
#endif

#define OPENACC_BIND_TXT "openacc_bind.txt"

/*
 * Copyright 2025 Meteo-France
 * All rights reserved
 * philippe.marguinaud@meteo.fr
 */

void fxtran_acdc_openacc_bind_ (int * prank, int * psize)
{
  int rank = *prank;
  FILE * fp;
  int i;
  size_t len  = 256;
  char * buf = (char*)malloc (len);
  const char * MF_OPENACC_BIND;
  int idev;

  MF_OPENACC_BIND = getenv ("MF_OPENACC_BIND");

  if (MF_OPENACC_BIND == NULL)
    MF_OPENACC_BIND = OPENACC_BIND_TXT;

  fp = fopen (MF_OPENACC_BIND, "r");

  if (fp == NULL)
    {
      fprintf (stderr, "`%s' was not found\n", MF_OPENACC_BIND);
      goto end;
    }

  for (i = 0; i < rank+1; i++)
    {
      if (getline (&buf, &len, fp) == -1)
        {
          fprintf (stderr, "Unexpected EOF while reading `" OPENACC_BIND_TXT "'\n");
          goto end;
        }
    }

  if (sscanf (buf, "%d", &idev) != 1)
    {
      fprintf (stderr, "Expected INTEGER while reading `" OPENACC_BIND_TXT "'\n");
      goto end;
    }

#ifdef _FXTRAN_ACDC_USE_OPENACC
  printf (" openacc_bind_ : %d -> %d\n", rank, idev);
  acc_set_device_num (idev, acc_device_nvidia);
#endif

end:

  if (fp != NULL)
    fclose (fp);

  free (buf);
}

