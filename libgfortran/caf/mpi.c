/* MPI implementation of GNU Fortran Coarray Library
   Copyright (C) 2011
   Free Software Foundation, Inc.
   Contributed by Tobias Burnus <burnus@net-b.de>

This file is part of the GNU Fortran Coarray Runtime Library (libcaf).

Libcaf is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libcaf is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libcaf.h"
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/* Define GFC_CAF_CHECK to enable run-time checking.  */
/* #define GFC_CAF_CHECK  1  */


static void error_stop (int error) __attribute__ ((noreturn));

/* Global variables.  */
static int caf_this_image;
static int caf_num_images;
static MPI_Win caf_world_window;


/* Initialize coarray program.  This routine assumes that no other
   MPI initialization happened before; otherwise MPI_Initialized
   had to be used.  As the MPI library might modify the command-line
   arguments, the routine should be called before the run-time
   libaray is initialized.  */

void
_gfortran_caf_init (int *argc, char ***argv, int *this_image, int *num_images)
{
  int flag;

  /* The following is only the case if one does not have a Fortran
     main program. */
  MPI_Initialized (&flag);
  if (!flag)
    MPI_Init (argc, argv);

  MPI_Comm_rank (MPI_COMM_WORLD, &caf_this_image);
  *this_image = caf_this_image + 1;
  MPI_Comm_size (MPI_COMM_WORLD, &caf_num_images);
  *num_images = caf_num_images;

  /* Obtain window for CRITICAL section locking.  */
  MPI_Win_create (NULL, 0, 1, MPI_INFO_NULL, MPI_COMM_WORLD,
		  &caf_world_window);
}


/* Finalize coarray program. Note: This is only called before the
   program ends; thus the MPI_Initialized status of _gfortran_caf_init
   does not play a role.  */

void
_gfortran_caf_finalize (void)
{
  MPI_Win_free (&caf_world_window);
  MPI_Finalize ();
}


/* SYNC ALL - the return value matches Fortran's STAT argument.  */

int
_gfortran_caf_sync_all (char *errmsg, int errmsg_len)
{
  int ierr;
  ierr = MPI_Barrier (MPI_COMM_WORLD);

  if (ierr && errmsg_len > 0)
    {
      const char msg[] = "SYNC ALL failed";
      int len = ((int) sizeof (msg) > errmsg_len) ? errmsg_len
						  : (int) sizeof (msg);
      memcpy (errmsg, msg, len);
      if (errmsg_len > len)
	memset (&errmsg[len], ' ', errmsg_len-len);
    }

  /* TODO: Is ierr correct? When should STAT_STOPPED_IMAGE be used?  */
  return ierr;
}


/* SYNC IMAGES. Note: SYNC IMAGES(*) is passed as count == -1 while
   SYNC IMAGES([]) has count == 0. Note further that SYNC IMAGES(*)
   is not equivalent to SYNC ALL.  The return value matches Fortran's
   STAT argument.  */
int
_gfortran_caf_sync_images (int count, int images[], char *errmsg,
			   int errmsg_len)
{
  int ierr;

  if (count == 0 || (count == 1 && images[0] == caf_this_image))
    return 0;

#ifdef GFC_CAF_CHECK
  {
    int i;

    for (i = 0; i < count; i++)
      if (images[i] < 1 || images[i] > caf_num_images)
	{
	  fprintf (stderr, "COARRAY ERROR: Invalid image index %d to SYNC "
		   "IMAGES", images[i]);
	  error_stop (1);
	}
  }
#endif

  /* FIXME: SYNC IMAGES with a nontrivial argument cannot easily be
     mapped to MPI communicators. Thus, exist early with an error message.  */
  if (count > 0)
    {
      fprintf (stderr, "COARRAY ERROR: SYNC IMAGES not yet implemented");
      error_stop (1);
    }

  /* Handle SYNC IMAGES(*).  */
  ierr = MPI_Barrier (MPI_COMM_WORLD);

  if (ierr && errmsg_len > 0)
    {
      const char msg[] = "SYNC IMAGES failed";
      int len = ((int) sizeof (msg) > errmsg_len) ? errmsg_len
						  : (int) sizeof (msg);
      memcpy (errmsg, msg, len);
      if (errmsg_len > len)
	memset (&errmsg[len], ' ', errmsg_len-len);
    }

  /* TODO: Is ierr correct? When should STAT_STOPPED_IMAGE be used?  */
  return ierr;
}


/* CRITICAL BLOCK. */

void
_gfortran_caf_critical (void)
{
  MPI_Win_lock (MPI_LOCK_SHARED, 0, 0, caf_world_window);
}


void
_gfortran_caf_end_critical (void)
{
  MPI_Win_unlock (0, caf_world_window);
}


/* ERROR STOP the other images.  */

static void
error_stop (int error)
{
  /* FIXME: Shutdown the Fortran RTL to flush the buffer.  PR 43849.  */
  /* FIXME: Do some more effort than just MPI_ABORT.  */
  MPI_Abort (MPI_COMM_WORLD, error);

  /* Should be unreachable, but to make sure also call exit.  */
  exit (error);
}


/* ERROR STOP function for string arguments.  */

void
_gfortran_caf_error_stop_str (const char *string, int32_t len)
{
  fputs ("ERROR STOP ", stderr);
  while (len--)
    fputc (*(string++), stderr);
  fputs ("\n", stderr);

  error_stop (1);
}


/* ERROR STOP function for numerical arguments.  */

void
_gfortran_caf_error_stop (int32_t error)
{
  fprintf (stderr, "ERROR STOP %d\n", error);
  error_stop (error);
}
