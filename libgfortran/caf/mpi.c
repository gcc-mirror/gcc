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
#include <string.h>	/* For memcpy.  */
#include <mpi.h>


/* Define GFC_CAF_CHECK to enable run-time checking.  */
/* #define GFC_CAF_CHECK  1  */


static void error_stop (int error) __attribute__ ((noreturn));

/* Global variables.  */
static int caf_mpi_initialized;
static int caf_this_image;
static int caf_num_images;

caf_static_t *caf_static_list = NULL;


/* Initialize coarray program.  This routine assumes that no other
   MPI initialization happened before; otherwise MPI_Initialized
   had to be used.  As the MPI library might modify the command-line
   arguments, the routine should be called before the run-time
   libaray is initialized.  */

void
_gfortran_caf_init (int *argc, char ***argv, int *this_image, int *num_images)
{
  if (caf_num_images == 0)
    {
      /* caf_mpi_initialized is only true if the main program is
       not written in Fortran.  */
      MPI_Initialized (&caf_mpi_initialized);
      if (!caf_mpi_initialized)
	MPI_Init (argc, argv);

      MPI_Comm_size (MPI_COMM_WORLD, &caf_num_images);
      MPI_Comm_rank (MPI_COMM_WORLD, &caf_this_image);
      caf_this_image++;
    }

  if (this_image)
    *this_image = caf_this_image;
  if (num_images)
    *num_images = caf_num_images;
}


/* Finalize coarray program.   */

void
_gfortran_caf_finalize (void)
{
  while (caf_static_list != NULL)
    {
      free(caf_static_list->token[caf_this_image-1]);
      caf_static_list = caf_static_list->prev;
    }

  if (!caf_mpi_initialized)
    MPI_Finalize ();
}


void *
_gfortran_caf_register (ptrdiff_t size, caf_register_t type,
                        void **token)
{
  void *local;

  /* Start MPI if not already started.  */
  if (caf_num_images == 0)
    _gfortran_caf_init (NULL, NULL, NULL, NULL);

  /* Token contains only a list of pointers.  */
  local = malloc (size);
  token = malloc (sizeof (void*) * caf_num_images);

  /* token[img-1] is the address of the token in image "img".  */
  MPI_Allgather (&local, sizeof (void*), MPI_BYTE,
		 token,  sizeof (void*), MPI_BYTE, MPI_COMM_WORLD);

  if (type == CAF_REGTYPE_COARRAY_STATIC)
    {
      caf_static_t *tmp = malloc (sizeof (caf_static_t));
      tmp->prev  = caf_static_list;
      tmp->token = token;
      caf_static_list = tmp;
    }
  return local;
}


int
_gfortran_caf_deregister (void **token __attribute__ ((unused)))
{
  return 0;
}


void
_gfortran_caf_sync_all (int *stat, char *errmsg, int errmsg_len)
{
  /* TODO: Is ierr correct? When should STAT_STOPPED_IMAGE be used?  */
  int ierr = MPI_Barrier (MPI_COMM_WORLD);

  if (stat)
    *stat = ierr;

  if (ierr)
    {
      const char msg[] = "SYNC ALL failed";
      if (errmsg_len > 0)
	{
	  int len = ((int) sizeof (msg) > errmsg_len) ? errmsg_len
						      : (int) sizeof (msg);
	  memcpy (errmsg, msg, len);
	  if (errmsg_len > len)
	    memset (&errmsg[len], ' ', errmsg_len-len);
	}
      else
	{
	  fprintf (stderr, "SYNC ALL failed\n");
	  error_stop (ierr);
	}
    }
}


/* SYNC IMAGES. Note: SYNC IMAGES(*) is passed as count == -1 while
   SYNC IMAGES([]) has count == 0. Note further that SYNC IMAGES(*)
   is not equivalent to SYNC ALL. */
void
_gfortran_caf_sync_images (int count, int images[], int *stat, char *errmsg,
			   int errmsg_len)
{
  int ierr;
  if (count == 0 || (count == 1 && images[0] == caf_this_image))
    {
      if (stat)
	*stat = 0;
      return;
    }

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
  /* TODO: Is ierr correct? When should STAT_STOPPED_IMAGE be used?  */
  ierr = MPI_Barrier (MPI_COMM_WORLD);
  if (stat)
    *stat = ierr;

  if (ierr)
    {
      const char msg[] = "SYNC IMAGES failed";
      if (errmsg_len > 0)
	{
	  int len = ((int) sizeof (msg) > errmsg_len) ? errmsg_len
						      : (int) sizeof (msg);
	  memcpy (errmsg, msg, len);
	  if (errmsg_len > len)
	    memset (&errmsg[len], ' ', errmsg_len-len);
	}
      else
	{
	  fprintf (stderr, "SYNC IMAGES failed\n");
	  error_stop (ierr);
	}
    }
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
