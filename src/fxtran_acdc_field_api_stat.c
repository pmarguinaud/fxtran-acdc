#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>

/*
 * Copyright 2025 Meteo-France
 * All rights reserved
 * philippe.marguinaud@meteo.fr
 */

void fxtran_acdc_field_api_stat_ (long * device_cur, long * device_max)
{
  void * h = NULL;
  char * error;

  *device_cur = 0;
  *device_max = 0;

  h = dlopen (NULL, RTLD_LAZY);

  if (h == NULL) 
    {
      fprintf (stderr, "%s\n", dlerror ());
      abort ();
    }

  void (*field_api_stat) (long *, long *) = dlsym (h, "field_api_stat");

  if (field_api_stat != NULL)
    field_api_stat (device_cur, device_max);

  dlclose (h);
  h = NULL;

}

