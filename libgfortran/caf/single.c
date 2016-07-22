/* Single-image implementation of GNU Fortran Coarray Library
   Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
#include <assert.h>

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

  if (type == CAF_REGTYPE_LOCK_STATIC || type == CAF_REGTYPE_LOCK_ALLOC
      || type == CAF_REGTYPE_CRITICAL || type == CAF_REGTYPE_EVENT_STATIC
      || type == CAF_REGTYPE_EVENT_ALLOC)
    local = calloc (size, sizeof (bool));
  else
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

  if (type == CAF_REGTYPE_COARRAY_STATIC || type == CAF_REGTYPE_LOCK_STATIC
      || type == CAF_REGTYPE_CRITICAL || type == CAF_REGTYPE_EVENT_STATIC
      || type == CAF_REGTYPE_EVENT_ALLOC)
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
  __asm__ __volatile__ ("":::"memory");
  if (stat)
    *stat = 0;
}


void
_gfortran_caf_sync_memory (int *stat,
			   char *errmsg __attribute__ ((unused)),
			   int errmsg_len __attribute__ ((unused)))
{
  __asm__ __volatile__ ("":::"memory");
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

  __asm__ __volatile__ ("":::"memory");
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_stop_numeric(int32_t stop_code)
{
  fprintf (stderr, "STOP %d\n", stop_code);
  exit (0);
}

void
_gfortran_caf_stop_str(const char *string, int32_t len)
{
  fputs ("STOP ", stderr);
  while (len--)
    fputc (*(string++), stderr);
  fputs ("\n", stderr);

  exit (0);
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
_gfortran_caf_co_broadcast (gfc_descriptor_t *a __attribute__ ((unused)),
			    int source_image __attribute__ ((unused)),
			    int *stat, char *errmsg __attribute__ ((unused)),
			    int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_sum (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_min (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_max (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      int errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}


void
_gfortran_caf_co_reduce (gfc_descriptor_t *a __attribute__ ((unused)),
                        void * (*opr) (void *, void *)
                               __attribute__ ((unused)),
                        int opr_flags __attribute__ ((unused)),
                        int result_image __attribute__ ((unused)),
                        int *stat, char *errmsg __attribute__ ((unused)),
                        int a_len __attribute__ ((unused)),
                        int errmsg_len __attribute__ ((unused)))
 {
   if (stat)
     *stat = 0;
 }


static void
assign_char4_from_char1 (size_t dst_size, size_t src_size, uint32_t *dst,
			 unsigned char *src)
{
  size_t i, n;
  n = dst_size/4 > src_size ? src_size : dst_size/4;
  for (i = 0; i < n; ++i)
    dst[i] = (int32_t) src[i];
  for (; i < dst_size/4; ++i)
    dst[i] = (int32_t) ' ';
}


static void
assign_char1_from_char4 (size_t dst_size, size_t src_size, unsigned char *dst,
			 uint32_t *src)
{
  size_t i, n;
  n = dst_size > src_size/4 ? src_size/4 : dst_size;
  for (i = 0; i < n; ++i)
    dst[i] = src[i] > UINT8_MAX ? (unsigned char) '?' : (unsigned char) src[i];
  if (dst_size > n)
    memset(&dst[n], ' ', dst_size - n);
}


static void
convert_type (void *dst, int dst_type, int dst_kind, void *src, int src_type,
	      int src_kind, int *stat)
{
#ifdef HAVE_GFC_INTEGER_16
  typedef __int128 int128t;
#else
  typedef int64_t int128t;
#endif

#if defined(GFC_REAL_16_IS_LONG_DOUBLE)
  typedef long double real128t;
  typedef _Complex long double complex128t;
#elif defined(HAVE_GFC_REAL_16)
  typedef _Complex float __attribute__((mode(TC))) __complex128;
  typedef __float128 real128t;
  typedef __complex128 complex128t;
#elif defined(HAVE_GFC_REAL_10)
  typedef long double real128t;
  typedef long double complex128t;
#else
  typedef double real128t;
  typedef _Complex double complex128t;
#endif

  int128t int_val = 0;
  real128t real_val = 0;
  complex128t cmpx_val = 0;

  switch (src_type)
    {
    case BT_INTEGER:
      if (src_kind == 1)
	int_val = *(int8_t*) src;
      else if (src_kind == 2)
	int_val = *(int16_t*) src;
      else if (src_kind == 4)
	int_val = *(int32_t*) src;
      else if (src_kind == 8)
	int_val = *(int64_t*) src;
#ifdef HAVE_GFC_INTEGER_16
      else if (src_kind == 16)
	int_val = *(int128t*) src;
#endif
      else
	goto error;
      break;
    case BT_REAL:
      if (src_kind == 4)
	real_val = *(float*) src;
      else if (src_kind == 8)
	real_val = *(double*) src;
#ifdef HAVE_GFC_REAL_10
      else if (src_kind == 10)
	real_val = *(long double*) src;
#endif
#ifdef HAVE_GFC_REAL_16
      else if (src_kind == 16)
	real_val = *(real128t*) src;
#endif
      else
	goto error;
      break;
    case BT_COMPLEX:
      if (src_kind == 4)
	cmpx_val = *(_Complex float*) src;
      else if (src_kind == 8)
	cmpx_val = *(_Complex double*) src;
#ifdef HAVE_GFC_REAL_10
      else if (src_kind == 10)
	cmpx_val = *(_Complex long double*) src;
#endif
#ifdef HAVE_GFC_REAL_16
      else if (src_kind == 16)
	cmpx_val = *(complex128t*) src;
#endif
      else
	goto error;
      break;
    default:
      goto error;
    }

  switch (dst_type)
    {
    case BT_INTEGER:
      if (src_type == BT_INTEGER)
	{
	  if (dst_kind == 1)
	    *(int8_t*) dst = (int8_t) int_val;
	  else if (dst_kind == 2)
	    *(int16_t*) dst = (int16_t) int_val;
	  else if (dst_kind == 4)
	    *(int32_t*) dst = (int32_t) int_val;
	  else if (dst_kind == 8)
	    *(int64_t*) dst = (int64_t) int_val;
#ifdef HAVE_GFC_INTEGER_16
	  else if (dst_kind == 16)
	    *(int128t*) dst = (int128t) int_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_REAL)
	{
	  if (dst_kind == 1)
	    *(int8_t*) dst = (int8_t) real_val;
	  else if (dst_kind == 2)
	    *(int16_t*) dst = (int16_t) real_val;
	  else if (dst_kind == 4)
	    *(int32_t*) dst = (int32_t) real_val;
	  else if (dst_kind == 8)
	    *(int64_t*) dst = (int64_t) real_val;
#ifdef HAVE_GFC_INTEGER_16
	  else if (dst_kind == 16)
	    *(int128t*) dst = (int128t) real_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_COMPLEX)
	{
	  if (dst_kind == 1)
	    *(int8_t*) dst = (int8_t) cmpx_val;
	  else if (dst_kind == 2)
	    *(int16_t*) dst = (int16_t) cmpx_val;
	  else if (dst_kind == 4)
	    *(int32_t*) dst = (int32_t) cmpx_val;
	  else if (dst_kind == 8)
	    *(int64_t*) dst = (int64_t) cmpx_val;
#ifdef HAVE_GFC_INTEGER_16
	  else if (dst_kind == 16)
	    *(int128t*) dst = (int128t) cmpx_val;
#endif
	  else
	    goto error;
	}
      else
	goto error;
      break;
    case BT_REAL:
      if (src_type == BT_INTEGER)
	{
	  if (dst_kind == 4)
	    *(float*) dst = (float) int_val;
	  else if (dst_kind == 8)
	    *(double*) dst = (double) int_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(long double*) dst = (long double) int_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(real128t*) dst = (real128t) int_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_REAL)
	{
	  if (dst_kind == 4)
	    *(float*) dst = (float) real_val;
	  else if (dst_kind == 8)
	    *(double*) dst = (double) real_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(long double*) dst = (long double) real_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(real128t*) dst = (real128t) real_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_COMPLEX)
	{
	  if (dst_kind == 4)
	    *(float*) dst = (float) cmpx_val;
	  else if (dst_kind == 8)
	    *(double*) dst = (double) cmpx_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(long double*) dst = (long double) cmpx_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(real128t*) dst = (real128t) cmpx_val;
#endif
	  else
	    goto error;
	}
      break;
    case BT_COMPLEX:
      if (src_type == BT_INTEGER)
	{
	  if (dst_kind == 4)
	    *(_Complex float*) dst = (_Complex float) int_val;
	  else if (dst_kind == 8)
	    *(_Complex double*) dst = (_Complex double) int_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(_Complex long double*) dst = (_Complex long double) int_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(complex128t*) dst = (complex128t) int_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_REAL)
	{
	  if (dst_kind == 4)
	    *(_Complex float*) dst = (_Complex float) real_val;
	  else if (dst_kind == 8)
	    *(_Complex double*) dst = (_Complex double) real_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(_Complex long double*) dst = (_Complex long double) real_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(complex128t*) dst = (complex128t) real_val;
#endif
	  else
	    goto error;
	}
      else if (src_type == BT_COMPLEX)
	{
	  if (dst_kind == 4)
	    *(_Complex float*) dst = (_Complex float) cmpx_val;
	  else if (dst_kind == 8)
	    *(_Complex double*) dst = (_Complex double) cmpx_val;
#ifdef HAVE_GFC_REAL_10
	  else if (dst_kind == 10)
	    *(_Complex long double*) dst = (_Complex long double) cmpx_val;
#endif
#ifdef HAVE_GFC_REAL_16
	  else if (dst_kind == 16)
	    *(complex128t*) dst = (complex128t) cmpx_val;
#endif
	  else
	    goto error;
	}
      else
	goto error;
      break;
    default:
      goto error;
    }

error:
  fprintf (stderr, "libcaf_single RUNTIME ERROR: Cannot convert type %d kind "
	   "%d to type %d kind %d\n", src_type, src_kind, dst_type, dst_kind);
  if (stat)
    *stat = 1;
  else
    abort ();
}


void
_gfortran_caf_get (caf_token_t token, size_t offset,
		   int image_index __attribute__ ((unused)),
		   gfc_descriptor_t *src,
		   caf_vector_t *src_vector __attribute__ ((unused)),
		   gfc_descriptor_t *dest, int src_kind, int dst_kind,
		   bool may_require_tmp, int *stat)
{
  /* FIXME: Handle vector subscripts.  */
  size_t i, k, size;
  int j;
  int rank = GFC_DESCRIPTOR_RANK (dest);
  size_t src_size = GFC_DESCRIPTOR_SIZE (src);
  size_t dst_size = GFC_DESCRIPTOR_SIZE (dest);

  if (stat)
    *stat = 0;

  if (rank == 0)
    {
      void *sr = (void *) ((char *) TOKEN (token) + offset);
      if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	  && dst_kind == src_kind)
	{
	  memmove (GFC_DESCRIPTOR_DATA (dest), sr,
		   dst_size > src_size ? src_size : dst_size);
	  if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	    {
	      if (dst_kind == 1)
		memset ((void*)(char*) GFC_DESCRIPTOR_DATA (dest) + src_size,
			' ', dst_size - src_size);
	      else /* dst_kind == 4.  */
		for (i = src_size/4; i < dst_size/4; i++)
		  ((int32_t*) GFC_DESCRIPTOR_DATA (dest))[i] = (int32_t) ' ';
	    }
	}
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	assign_char1_from_char4 (dst_size, src_size, GFC_DESCRIPTOR_DATA (dest),
				 sr);
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	assign_char4_from_char1 (dst_size, src_size, GFC_DESCRIPTOR_DATA (dest),
				 sr);
      else
	convert_type (GFC_DESCRIPTOR_DATA (dest), GFC_DESCRIPTOR_TYPE (dest),
		      dst_kind, sr, GFC_DESCRIPTOR_TYPE (src), src_kind, stat);
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

  if (may_require_tmp)
    {
      ptrdiff_t array_offset_sr, array_offset_dst;
      void *tmp = malloc (size*src_size);

      array_offset_dst = 0;
      for (i = 0; i < size; i++)
	{
	  ptrdiff_t array_offset_sr = 0;
	  ptrdiff_t stride = 1;
	  ptrdiff_t extent = 1;
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
	  void *sr = (void *)((char *) TOKEN (token) + offset
			  + array_offset_sr*GFC_DESCRIPTOR_SIZE (src));
          memcpy ((void *) ((char *) tmp + array_offset_dst), sr, src_size);
          array_offset_dst += src_size;
	}

      array_offset_sr = 0;
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
	  void *dst = dest->base_addr
		      + array_offset_dst*GFC_DESCRIPTOR_SIZE (dest);
          void *sr = tmp + array_offset_sr;

	  if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	      && dst_kind == src_kind)
	    {
	      memmove (dst, sr, dst_size > src_size ? src_size : dst_size);
	      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER
	          && dst_size > src_size)
		{
		  if (dst_kind == 1)
		    memset ((void*)(char*) dst + src_size, ' ',
			    dst_size-src_size);
		  else /* dst_kind == 4.  */
		    for (k = src_size/4; k < dst_size/4; k++)
		      ((int32_t*) dst)[k] = (int32_t) ' ';
		}
	    }
	  else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	    assign_char1_from_char4 (dst_size, src_size, dst, sr);
	  else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	    assign_char4_from_char1 (dst_size, src_size, dst, sr);
	  else
	    convert_type (dst, GFC_DESCRIPTOR_TYPE (dest), dst_kind,
			  sr, GFC_DESCRIPTOR_TYPE (src), src_kind, stat);
          array_offset_sr += src_size;
	}

      free (tmp);
      return;
    }

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
      void *sr = (void *)((char *) TOKEN (token) + offset
			  + array_offset_sr*GFC_DESCRIPTOR_SIZE (src));

      if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	  && dst_kind == src_kind)
	{
	  memmove (dst, sr, dst_size > src_size ? src_size : dst_size);
	  if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	    {
	      if (dst_kind == 1)
		memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	      else /* dst_kind == 4.  */
		for (k = src_size/4; k < dst_size/4; k++)
		  ((int32_t*) dst)[k] = (int32_t) ' ';
	    }
	}
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	assign_char1_from_char4 (dst_size, src_size, dst, sr);
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	assign_char4_from_char1 (dst_size, src_size, dst, sr);
      else
	convert_type (dst, GFC_DESCRIPTOR_TYPE (dest), dst_kind,
		      sr, GFC_DESCRIPTOR_TYPE (src), src_kind, stat);
    }
}


void
_gfortran_caf_send (caf_token_t token, size_t offset,
		    int image_index __attribute__ ((unused)),
		    gfc_descriptor_t *dest,
		    caf_vector_t *dst_vector __attribute__ ((unused)),
		    gfc_descriptor_t *src, int dst_kind, int src_kind,
		    bool may_require_tmp, int *stat)
{
  /* FIXME: Handle vector subscripts.  */
  size_t i, k, size;
  int j;
  int rank = GFC_DESCRIPTOR_RANK (dest);
  size_t src_size = GFC_DESCRIPTOR_SIZE (src);
  size_t dst_size = GFC_DESCRIPTOR_SIZE (dest);

  if (stat)
    *stat = 0;

  if (rank == 0)
    {
      void *dst = (void *) ((char *) TOKEN (token) + offset);
      if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	  && dst_kind == src_kind)
	{
	  memmove (dst, GFC_DESCRIPTOR_DATA (src),
		   dst_size > src_size ? src_size : dst_size);
	  if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	    {
	      if (dst_kind == 1)
		memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	      else /* dst_kind == 4.  */
		for (i = src_size/4; i < dst_size/4; i++)
		  ((int32_t*) dst)[i] = (int32_t) ' ';
	    }
	}
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	assign_char1_from_char4 (dst_size, src_size, dst,
				 GFC_DESCRIPTOR_DATA (src));
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	assign_char4_from_char1 (dst_size, src_size, dst,
				 GFC_DESCRIPTOR_DATA (src));
      else
	convert_type (dst, GFC_DESCRIPTOR_TYPE (dest), dst_kind,
		      GFC_DESCRIPTOR_DATA (src), GFC_DESCRIPTOR_TYPE (src),
		      src_kind, stat);
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

  if (may_require_tmp)
    {
      ptrdiff_t array_offset_sr, array_offset_dst;
      void *tmp;

      if (GFC_DESCRIPTOR_RANK (src) == 0)
	{
	  tmp = malloc (src_size);
	  memcpy (tmp, GFC_DESCRIPTOR_DATA (src), src_size);
	}
      else
	{
	  tmp = malloc (size*src_size);
	  array_offset_dst = 0;
	  for (i = 0; i < size; i++)
	    {
	      ptrdiff_t array_offset_sr = 0;
	      ptrdiff_t stride = 1;
	      ptrdiff_t extent = 1;
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
	      void *sr = (void *) ((char *) src->base_addr
				   + array_offset_sr*GFC_DESCRIPTOR_SIZE (src));
	      memcpy ((void *) ((char *) tmp + array_offset_dst), sr, src_size);
	      array_offset_dst += src_size;
	    }
	}

      array_offset_sr = 0;
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
          void *sr = tmp + array_offset_sr;
	  if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	      && dst_kind == src_kind)
	    {
	      memmove (dst, sr,
		       dst_size > src_size ? src_size : dst_size);
	      if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER
		  && dst_size > src_size)
		{
		  if (dst_kind == 1)
		    memset ((void*)(char*) dst + src_size, ' ',
			    dst_size-src_size);
		  else /* dst_kind == 4.  */
		    for (k = src_size/4; k < dst_size/4; k++)
		      ((int32_t*) dst)[k] = (int32_t) ' ';
		}
	    }
	  else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	    assign_char1_from_char4 (dst_size, src_size, dst, sr);
	  else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	    assign_char4_from_char1 (dst_size, src_size, dst, sr);
	  else
	    convert_type (dst, GFC_DESCRIPTOR_TYPE (dest), dst_kind,
			  sr, GFC_DESCRIPTOR_TYPE (src), src_kind, stat);
          if (GFC_DESCRIPTOR_RANK (src))
	    array_offset_sr += src_size;
	}
      free (tmp);
      return;
    }

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

      if (GFC_DESCRIPTOR_TYPE (dest) == GFC_DESCRIPTOR_TYPE (src)
	  && dst_kind == src_kind)
	{
	  memmove (dst, sr,
		   dst_size > src_size ? src_size : dst_size);
	  if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_size > src_size)
	    {
	      if (dst_kind == 1)
		memset ((void*)(char*) dst + src_size, ' ', dst_size-src_size);
	      else /* dst_kind == 4.  */
		for (k = src_size/4; k < dst_size/4; k++)
		  ((int32_t*) dst)[k] = (int32_t) ' ';
	    }
	}
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER && dst_kind == 1)
	assign_char1_from_char4 (dst_size, src_size, dst, sr);
      else if (GFC_DESCRIPTOR_TYPE (dest) == BT_CHARACTER)
	assign_char4_from_char1 (dst_size, src_size, dst, sr);
      else
	convert_type (dst, GFC_DESCRIPTOR_TYPE (dest), dst_kind,
		      sr, GFC_DESCRIPTOR_TYPE (src), src_kind, stat);
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
		       int dst_kind, int src_kind, bool may_require_tmp)
{
  /* FIXME: Handle vector subscript of 'src_vector'.  */
  /* For a single image, src->base_addr should be the same as src_token + offset
     but to play save, we do it properly.  */
  void *src_base = GFC_DESCRIPTOR_DATA (src);
  GFC_DESCRIPTOR_DATA (src) = (void *) ((char *) TOKEN (src_token) + src_offset);
  _gfortran_caf_send (dst_token, dst_offset, dst_image_index, dest, dst_vector,
		      src, dst_kind, src_kind, may_require_tmp, NULL);
  GFC_DESCRIPTOR_DATA (src) = src_base;
}


void
_gfortran_caf_atomic_define (caf_token_t token, size_t offset,
			     int image_index __attribute__ ((unused)),
			     void *value, int *stat,
			     int type __attribute__ ((unused)), int kind)
{
  assert(kind == 4);

  uint32_t *atom = (uint32_t *) ((char *) TOKEN (token) + offset);

  __atomic_store (atom, (uint32_t *) value, __ATOMIC_RELAXED);

  if (stat)
    *stat = 0;
}

void
_gfortran_caf_atomic_ref (caf_token_t token, size_t offset,
			  int image_index __attribute__ ((unused)),
			  void *value, int *stat,
			  int type __attribute__ ((unused)), int kind)
{
  assert(kind == 4);

  uint32_t *atom = (uint32_t *) ((char *) TOKEN (token) + offset);

  __atomic_load (atom, (uint32_t *) value, __ATOMIC_RELAXED);

  if (stat)
    *stat = 0;
}


void
_gfortran_caf_atomic_cas (caf_token_t token, size_t offset,
			  int image_index __attribute__ ((unused)),
			  void *old, void *compare, void *new_val, int *stat,
			  int type __attribute__ ((unused)), int kind)
{
  assert(kind == 4);

  uint32_t *atom = (uint32_t *) ((char *) TOKEN (token) + offset);

  *(uint32_t *) old = *(uint32_t *) compare;
  (void) __atomic_compare_exchange_n (atom, (uint32_t *) old,
				      *(uint32_t *) new_val, false,
				      __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  if (stat)
    *stat = 0;
}


void
_gfortran_caf_atomic_op (int op, caf_token_t token, size_t offset,
			 int image_index __attribute__ ((unused)),
			 void *value, void *old, int *stat,
			 int type __attribute__ ((unused)), int kind)
{
  assert(kind == 4);

  uint32_t res;
  uint32_t *atom = (uint32_t *) ((char *) TOKEN (token) + offset);

  switch (op)
    {
    case GFC_CAF_ATOMIC_ADD:
      res = __atomic_fetch_add (atom, *(uint32_t *) value, __ATOMIC_RELAXED);
      break;
    case GFC_CAF_ATOMIC_AND:
      res = __atomic_fetch_and (atom, *(uint32_t *) value, __ATOMIC_RELAXED);
      break;
    case GFC_CAF_ATOMIC_OR:
      res = __atomic_fetch_or (atom, *(uint32_t *) value, __ATOMIC_RELAXED);
      break;
    case GFC_CAF_ATOMIC_XOR:
      res = __atomic_fetch_xor (atom, *(uint32_t *) value, __ATOMIC_RELAXED);
      break;
    default:
      __builtin_unreachable();
    }

  if (old)
    *(uint32_t *) old = res;

  if (stat)
    *stat = 0;
}

void
_gfortran_caf_event_post (caf_token_t token, size_t index, 
			  int image_index __attribute__ ((unused)), 
			  int *stat, char *errmsg __attribute__ ((unused)), 
			  int errmsg_len __attribute__ ((unused)))
{
  uint32_t value = 1;
  uint32_t *event = (uint32_t *) ((char *) TOKEN (token) + index*sizeof(uint32_t));
  __atomic_fetch_add (event, (uint32_t) value, __ATOMIC_RELAXED);
  
  if(stat)
    *stat = 0;
}

void
_gfortran_caf_event_wait (caf_token_t token, size_t index, 
			  int until_count, int *stat,
			  char *errmsg __attribute__ ((unused)), 
			  int errmsg_len __attribute__ ((unused)))
{
  uint32_t *event = (uint32_t *) ((char *) TOKEN (token) + index*sizeof(uint32_t));
  uint32_t value = (uint32_t)-until_count;
   __atomic_fetch_add (event, (uint32_t) value, __ATOMIC_RELAXED);
  
   if(stat)
    *stat = 0;    
}

void
_gfortran_caf_event_query (caf_token_t token, size_t index, 
			   int image_index __attribute__ ((unused)), 
			   int *count, int *stat)
{
  uint32_t *event = (uint32_t *) ((char *) TOKEN (token) + index*sizeof(uint32_t));
  __atomic_load (event, (uint32_t *) count, __ATOMIC_RELAXED);
  
  if(stat)
    *stat = 0;
}

void
_gfortran_caf_lock (caf_token_t token, size_t index,
		    int image_index __attribute__ ((unused)),
		    int *aquired_lock, int *stat, char *errmsg, int errmsg_len)
{
  const char *msg = "Already locked";
  bool *lock = &((bool *) TOKEN (token))[index];

  if (!*lock)
    {
      *lock = true;
      if (aquired_lock)
	*aquired_lock = (int) true;
      if (stat)
	*stat = 0;
      return;
    }

  if (aquired_lock)
    {
      *aquired_lock = (int) false;
      if (stat)
	*stat = 0;
    return;
    }


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
      return;
    }
  _gfortran_caf_error_stop_str (msg, (int32_t) strlen (msg));
}


void
_gfortran_caf_unlock (caf_token_t token, size_t index,
		      int image_index __attribute__ ((unused)),
		      int *stat, char *errmsg, int errmsg_len)
{
  const char *msg = "Variable is not locked";
  bool *lock = &((bool *) TOKEN (token))[index];

  if (*lock)
    {
      *lock = false;
      if (stat)
	*stat = 0;
      return;
    }

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
      return;
    }
  _gfortran_caf_error_stop_str (msg, (int32_t) strlen (msg));
}
