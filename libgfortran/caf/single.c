/* Single-image implementation of GNU Fortran Coarray Library
   Copyright (C) 2011-2014 Free Software Foundation, Inc.
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
#include <stdio.h>  /* For fputs and fprintf.  */
#include <stdlib.h> /* For exit and malloc.  */
#include <string.h> /* For memcpy and memset.  */
#include <stdarg.h> /* For variadic arguments.  */

/* Define GFC_CAF_CHECK to enable run-time checking.  */
/* #define GFC_CAF_CHECK  1  */

typedef void* single_token_t;
#define TOKEN(X) ((single_token_t) (X))

/* Single-image implementation of the CAF library.
   Note: For performance reasons -fcoarry=single should be used
   rather than this library.  */

/* Global variables.  */
caf_static_t *caf_static_list = NULL;


/* Keep in sync with mpi.c.  */
static void
caf_runtime_error (const char *message, ...)
{
  va_list ap;
  fprintf (stderr, "Fortran runtime error: ");
  va_start (ap, message);
  vfprintf (stderr, message, ap);
  va_end (ap);
  fprintf (stderr, "\n");

  /* FIXME: Shutdown the Fortran RTL to flush the buffer.  PR 43849.  */
  exit (EXIT_FAILURE);
}

void
_gfortran_caf_init (int *argc __attribute__ ((unused)),
		    char ***argv __attribute__ ((unused)))
{
}


void
_gfortran_caf_finalize (void)
{
  while (caf_static_list != NULL)
    {
      caf_static_t *tmp = caf_static_list->prev;
      free (caf_static_list->token);
      free (caf_static_list);
      caf_static_list = tmp;
    }
}


int
_gfortran_caf_this_image (int distance __attribute__ ((unused)))
{
  return 1;
}


int
_gfortran_caf_num_images (int distance __attribute__ ((unused)),
			  int failed __attribute__ ((unused)))
{
  return 1;
}


void *
_gfortran_caf_register (size_t size, caf_register_t type, caf_token_t *token,
			int *stat, char *errmsg, int errmsg_len)
{
  void *local;

  local = malloc (size);
  *token = malloc (sizeof (single_token_t));

  if (unlikely (local == NULL || token == NULL))
    {
      const char msg[] = "Failed to allocate coarray";
      if (stat)
	{
	  *stat = 1;
	  if (errmsg_len > 0)
	    {
	      int len = ((int) sizeof (msg) > errmsg_len) ? errmsg_len
							  : (int) sizeof (msg);
	      memcpy (errmsg, msg, len);
	      if (errmsg_len > len)
		memset (&errmsg[len], ' ', errmsg_len-len);
	    }
	  return NULL;
	}
      else
	  caf_runtime_error (msg);
    }

  *token = local;

  if (stat)
    *stat = 0;

  if (type == CAF_REGTYPE_COARRAY_STATIC)
    {
      caf_static_t *tmp = malloc (sizeof (caf_static_t));
      tmp->prev  = caf_static_list;
      tmp->token = *token;
      caf_static_list = tmp;
    }
  return local;
}


void
_gfortran_caf_deregister (caf_token_t *token, int *stat,
			  char *errmsg __attribute__ ((unused)),
			  int errmsg_len __attribute__ ((unused)))
{
  free (TOKEN(*token));

  if (stat)
    *stat = 0;
}


void
_gfortran_caf_sync_all (int *stat,
			char *errmsg __attribute__ ((unused)),
			int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}


void
_gfortran_caf_sync_images (int count __attribute__ ((unused)),
			   int images[] __attribute__ ((unused)),
			   int *stat,
			   char *errmsg __attribute__ ((unused)),
			   int errmsg_len __attribute__ ((unused)))
{
#ifdef GFC_CAF_CHECK
  int i;

  for (i = 0; i < count; i++)
    if (images[i] != 1)
      {
	fprintf (stderr, "COARRAY ERROR: Invalid image index %d to SYNC "
		 "IMAGES", images[i]);
	exit (EXIT_FAILURE);
      }
#endif

  if (stat)
    *stat = 0;
}


void
_gfortran_caf_error_stop_str (const char *string, int32_t len)
{
  fputs ("ERROR STOP ", stderr);
  while (len--)
    fputc (*(string++), stderr);
  fputs ("\n", stderr);

  exit (1);
}


void
_gfortran_caf_error_stop (int32_t error)
{
  fprintf (stderr, "ERROR STOP %d\n", error);
  exit (error);
}


void
_gfortran_caf_co_sum (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    stat = 0;
}

void
_gfortran_caf_co_min (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int src_len __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    stat = 0;
}

void
_gfortran_caf_co_max (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int src_len __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    stat = 0;
}

void
_gfortran_caf_get (caf_token_t token, size_t offset,
		   int image_index __attribute__ ((unused)),
		   gfc_descriptor_t *src ,
		   caf_vector_t *src_vector __attribute__ ((unused)),
		   gfc_descriptor_t *dest, int src_kind, int dst_kind)
{
  /* FIXME: Handle vector subscript, type conversion and assignment "array = scalar".
     check in particular whether strings of different kinds are permitted and
     whether it makes sense to handle array = scalar.  */
  size_t i, k, size;
  int j;
  int rank = GFC_DESCRIPTOR_RANK (dest);
  size_t src_size = GFC_DESCRIPTOR_SIZE (src);
  size_t dst_size = GFC_DESCRIPTOR_SIZE (dest);

  if (rank == 0)
    {
      void *sr = (void *) ((char *) TOKEN (token) + offset);
      if (dst_kind == src_kind)
	memmove (GFC_DESCRIPTOR_DATA (dest), sr,
		 dst_size > src_size ? src_size : dst_size);
      /* else: FIXME: type conversion.  */
      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	{
	  if (dst_kind == 1)
	    memset ((void*)(char*) GFC_DESCRIPTOR_DATA (dest) + src_size, ' ',
		    dst_size-src_size);
	  else /* dst_kind == 4.  */
	    for (i = src_size/4; i < dst_size/4; i++)
	      ((int32_t*) GFC_DESCRIPTOR_DATA (dest))[i] = (int32_t)' ';
	}
      return;
    }

  size = 1;
  for (j = 0; j < rank; j++)
    {
      ptrdiff_t dimextent = dest->dim[j]._ubound - dest->dim[j].lower_bound + 1;
      if (dimextent < 0)
	dimextent = 0;
      size *= dimextent;
    }

  if (size == 0)
    return;

  for (i = 0; i < size; i++)
    {
      ptrdiff_t array_offset_dst = 0;
      ptrdiff_t stride = 1;
      ptrdiff_t extent = 1;
      for (j = 0; j < rank-1; j++)
	{
	  array_offset_dst += ((i / (extent*stride))
			       % (dest->dim[j]._ubound
				  - dest->dim[j].lower_bound + 1))
			      * dest->dim[j]._stride;
	  extent = (dest->dim[j]._ubound - dest->dim[j].lower_bound + 1);
          stride = dest->dim[j]._stride;
	}
      array_offset_dst += (i / extent) * dest->dim[rank-1]._stride;
      void *dst = dest->base_addr + array_offset_dst*GFC_DESCRIPTOR_SIZE (dest);

      void *sr;
      if (GFC_DESCRIPTOR_RANK (src) != 0)
	{
	  ptrdiff_t array_offset_sr = 0;
	  stride = 1;
	  extent = 1;
	  for (j = 0; j < GFC_DESCRIPTOR_RANK (src)-1; j++)
	    {
	      array_offset_sr += ((i / (extent*stride))
				  % (src->dim[j]._ubound
				     - src->dim[j].lower_bound + 1))
				 * src->dim[j]._stride;
	      extent = (src->dim[j]._ubound - src->dim[j].lower_bound + 1);
	      stride = src->dim[j]._stride;
	    }
	  array_offset_sr += (i / extent) * src->dim[rank-1]._stride;
	  sr = (void *)((char *) TOKEN (token) + offset
			+ array_offset_sr*GFC_DESCRIPTOR_SIZE (src));
	}
      else
	sr = (void *)((char *) TOKEN (token) + offset);

      if (dst_kind == src_kind)
	memmove (dst, sr, dst_size > src_size ? src_size : dst_size);
      /* else: FIXME: type conversion.  */
      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	{
	  if (dst_kind == 1)
	    memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	  else /* dst_kind == 4.  */
	    for (k = src_size/4; k < dst_size/4; i++)
	      ((int32_t*) dst)[i] = (int32_t)' ';
	}
    }
}


void
_gfortran_caf_send (caf_token_t token, size_t offset,
		    int image_index __attribute__ ((unused)),
		    gfc_descriptor_t *dest,
		    caf_vector_t *dst_vector __attribute__ ((unused)),
		    gfc_descriptor_t *src, int dst_kind,
		    int src_kind __attribute__ ((unused)))
{
  /* FIXME: Handle vector subscript, type conversion and assignment "array = scalar".
     check in particular whether strings of different kinds are permitted.  */
  size_t i, k, size;
  int j;
  int rank = GFC_DESCRIPTOR_RANK (dest);
  size_t src_size = GFC_DESCRIPTOR_SIZE (src);
  size_t dst_size = GFC_DESCRIPTOR_SIZE (dest);

  if (rank == 0)
    {
      void *dst = (void *) ((char *) TOKEN (token) + offset);
      if (dst_kind == src_kind)
	memmove (dst, GFC_DESCRIPTOR_DATA (src),
		 dst_size > src_size ? src_size : dst_size);
      /* else: FIXME: type conversion.  */
      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	{
	  if (dst_kind == 1)
	    memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	  else /* dst_kind == 4.  */
	    for (i = src_size/4; i < dst_size/4; i++)
	      ((int32_t*) dst)[i] = (int32_t)' ';
	}
      return;
    }

  size = 1;
  for (j = 0; j < rank; j++)
    {
      ptrdiff_t dimextent = dest->dim[j]._ubound - dest->dim[j].lower_bound + 1;
      if (dimextent < 0)
	dimextent = 0;
      size *= dimextent;
    }

  if (size == 0)
    return;

#if 0
  if (dst_len == src_len && PREFIX (is_contiguous) (dest)
      && PREFIX (is_contiguous) (src))
    {
      void *dst = (void *)((char *) TOKEN (token) + offset);
      memmove (dst, src->base_addr, GFC_DESCRIPTOR_SIZE (dest)*size);
      return;
    }
#endif

  for (i = 0; i < size; i++)
    {
      ptrdiff_t array_offset_dst = 0;
      ptrdiff_t stride = 1;
      ptrdiff_t extent = 1;
      for (j = 0; j < rank-1; j++)
	{
	  array_offset_dst += ((i / (extent*stride))
			       % (dest->dim[j]._ubound
				  - dest->dim[j].lower_bound + 1))
			      * dest->dim[j]._stride;
	  extent = (dest->dim[j]._ubound - dest->dim[j].lower_bound + 1);
          stride = dest->dim[j]._stride;
	}
      array_offset_dst += (i / extent) * dest->dim[rank-1]._stride;
      void *dst = (void *)((char *) TOKEN (token) + offset
			   + array_offset_dst*GFC_DESCRIPTOR_SIZE (dest));
      void *sr;
      if (GFC_DESCRIPTOR_RANK (src) != 0)
	{
	  ptrdiff_t array_offset_sr = 0;
	  stride = 1;
	  extent = 1;
	  for (j = 0; j < GFC_DESCRIPTOR_RANK (src)-1; j++)
	    {
	      array_offset_sr += ((i / (extent*stride))
				  % (src->dim[j]._ubound
				     - src->dim[j].lower_bound + 1))
				 * src->dim[j]._stride;
	      extent = (src->dim[j]._ubound - src->dim[j].lower_bound + 1);
	      stride = src->dim[j]._stride;
	    }
	  array_offset_sr += (i / extent) * src->dim[rank-1]._stride;
	  sr = (void *)((char *) src->base_addr
			+ array_offset_sr*GFC_DESCRIPTOR_SIZE (src));
	}
      else
	sr = src->base_addr;

      if (dst_kind == src_kind)
	memmove (dst, sr, dst_size > src_size ? src_size : dst_size);
      /* else: FIXME: type conversion.  */
      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	{
	  if (dst_kind == 1)
	    memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	  else /* dst_kind == 4.  */
	    for (k = src_size/4; k < dst_size/4; i++)
	      ((int32_t*) dst)[i] = (int32_t)' ';
	}
    }
}


void
_gfortran_caf_sendget (caf_token_t dst_token, size_t dst_offset,
		       int dst_image_index, gfc_descriptor_t *dest,
		       caf_vector_t *dst_vector, caf_token_t src_token,
		       size_t src_offset,
		       int src_image_index __attribute__ ((unused)),
		       gfc_descriptor_t *src,
		       caf_vector_t *src_vector __attribute__ ((unused)),
		       int dst_len, int src_len)
{
  /* FIXME: Handle vector subscript of 'src_vector'.  */
  /* For a single image, src->base_addr should be the same as src_token + offset
     but to play save, we do it properly.  */
  void *src_base = GFC_DESCRIPTOR_DATA (src);
  GFC_DESCRIPTOR_DATA (src) = (void *) ((char *) TOKEN (src_token) + src_offset);
  _gfortran_caf_send (dst_token, dst_offset, dst_image_index, dest, dst_vector,
		      src, dst_len, src_len);
  GFC_DESCRIPTOR_DATA (src) = src_base;
}
