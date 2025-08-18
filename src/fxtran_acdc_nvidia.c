#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef _OPENACC
#include <openacc.h>
#include <accel.h>
#endif

/*
 * Copyright 2025 Meteo-France
 * All rights reserved
 * philippe.marguinaud@meteo.fr
 */

static int runCommand (const char * fmt, ...)
{
  char command[512];
  va_list ap;
  int count;
  int rc;

  va_start (ap, fmt);
  count = vsnprintf (command, sizeof (command), fmt, ap);
  va_end (ap);

  if (count >= sizeof (command))
    abort ();

  rc = system (command);

  if (rc)
    abort ();
}

static int getMPIRank ()
{
  const char * MPIRank = getenv ("OMPI_COMM_WORLD_RANK");
  return MPIRank ? atoi (MPIRank) : -1;
}

void fxtran_acdc_nvidia_smi_ (ssize_t * psize)
{
  char file[64];
  FILE * fp = NULL;
  int idev;

  *psize = -1;

#ifdef _OPENACC
  idev = acc_get_device_num (acc_device_nvidia);

  sprintf (file, ".nvidia-smi-%6.6d.txt", 1 + getMPIRank ());
  unlink (file);
  runCommand ("/usr/bin/nvidia-smi --query-compute-apps=used_memory  --format=csv,noheader,nounits -i %d > %s", idev, file);
  if (fp = fopen (file, "r"))
    {
      fscanf (fp, "%lld", psize);
      fclose (fp);
    }

#endif

}

void fxtran_acdc_nvidia_free_memory_ (ssize_t * psize)
{
  *psize = 0;
#ifdef _OPENACC
  *psize = acc_get_memory () - acc_get_free_memory ();
#endif
}

static int redirectStderr (const char * file)
{
  int i, j, k;

  unlink (file);
  k = open (file, O_WRONLY | O_CREAT, 0644);

  fflush (stderr);
  i = fileno (stderr);
  j = dup (i);
  dup2 (k, i);
  return j;
}

static void restoreStderr (int j)
{
  fflush (stderr);
  dup2 (j, 2);
  close (j);
}

void fxtran_acdc_nvidia_present_dump_ (ssize_t * pallocated, ssize_t * pdeleted)
{
  int i;
  char file[64];
  FILE * fp = NULL;
  size_t linesize = 0;
  char * line = NULL;
  const char * ALLOCATED_BLOCK = "allocated block ";
  const char * DELETED_BLOCK = "deleted block ";
  const char * SIZE = "size:";

  *pallocated = 0;
  *pdeleted   = 0;

#ifdef _OPENACC

  sprintf (file, ".nvidia-acc_present_dump-%6.6d.txt", 1 + getMPIRank ());
  unlink (file);

  i = redirectStderr (file);
  acc_present_dump ();
  restoreStderr (i);

  fp = fopen (file, "r");

  while (getline (&line, &linesize, fp) >= 0)
    {
      if (strncmp (line, ALLOCATED_BLOCK, strlen (ALLOCATED_BLOCK)) == 0)
        {
          const char * str = strstr (line, SIZE);
          size_t size;
          if (str)
            {
              str += strlen (SIZE);
              sscanf (str, "%lld", &size);
              *pallocated += size;
            }
        }
      else if (strncmp (line, DELETED_BLOCK, strlen (DELETED_BLOCK)) == 0)
        {
          const char * str = strstr (line, SIZE);
          size_t size;
          if (str)
            {
              str += strlen (SIZE);
              sscanf (str, "%lld", &size);
              *pdeleted += size;
            }
        }
    }

  free (line);

  fclose (fp);

#endif

}

