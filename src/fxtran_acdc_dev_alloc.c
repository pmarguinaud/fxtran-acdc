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

static void (*dev_malloc) (size_t, void **) = NULL;
static void (*dev_free) (void *) = NULL;

static void init ()
{
  void * h = NULL;
  char * error;

  static int done = 0;

  if (done++)
    return;
  
  h = dlopen (NULL, RTLD_LAZY);

  if (h == NULL) 
    {
      fprintf (stderr, "%s\n", dlerror ());
      abort ();
    }

  dev_malloc = dlsym (h, "dev_malloc");
  dev_free   = dlsym (h, "dev_free");

  fprintf (stderr, " dev_malloc = 0x%llx, dev_free = 0x%llx\n", dev_malloc, dev_free);
  fflush (stderr);

  dlclose (h);
  h = NULL;
}

void fxtran_acdc_has_dev_malloc (int * ok)
{
  init ();
  *ok = ((dev_malloc != NULL) && (dev_free != NULL));
}

void fxtran_acdc_dev_malloc (size_t siz, void ** ptr)
{
  dev_malloc (siz, ptr);
}

void fxtran_acdc_dev_free (void * ptr)
{
  dev_free (ptr);
}

