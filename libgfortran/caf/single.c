/* Single-image implementation of GNU Fortran Coarray Library
   Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
#include <stdint.h>
#include <assert.h>

/* Define GFC_CAF_CHECK to enable run-time checking.  */
/* #define GFC_CAF_CHECK  1  */

struct caf_single_token
{
  /* The pointer to the memory registered.  For arrays this is the data member
     in the descriptor.  For components it's the pure data pointer.  */
  void *memptr;
  /* The descriptor when this token is associated to an allocatable array.  */
  gfc_descriptor_t *desc;
  /* Set when the caf lib has allocated the memory in memptr and is responsible
     for freeing it on deregister.  */
  bool owning_memory;
};
typedef struct caf_single_token *caf_single_token_t;

#define TOKEN(X) ((caf_single_token_t) (X))
#define MEMTOK(X) ((caf_single_token_t) (X))->memptr

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

/* Error handling is similar everytime.  */
static void
caf_internal_error (const char *msg, int *stat, char *errmsg,
		    size_t errmsg_len, ...)
{
  va_list args;
  va_start (args, errmsg_len);
  if (stat)
    {
      *stat = 1;
      if (errmsg_len > 0)
	{
	  int len = snprintf (errmsg, errmsg_len, msg, args);
	  if (len >= 0 && errmsg_len > (size_t) len)
	    memset (&errmsg[len], ' ', errmsg_len - len);
	}
      va_end (args);
      return;
    }
  else
    caf_runtime_error (msg, args);
  va_end (args);
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


void
_gfortran_caf_register (size_t size, caf_register_t type, caf_token_t *token,
			gfc_descriptor_t *data, int *stat, char *errmsg,
			size_t errmsg_len)
{
  const char alloc_fail_msg[] = "Failed to allocate coarray";
  void *local;
  caf_single_token_t single_token;

  if (type == CAF_REGTYPE_LOCK_STATIC || type == CAF_REGTYPE_LOCK_ALLOC
      || type == CAF_REGTYPE_CRITICAL)
    local = calloc (size, sizeof (bool));
  else if (type == CAF_REGTYPE_EVENT_STATIC || type == CAF_REGTYPE_EVENT_ALLOC)
    /* In the event_(wait|post) function the counter for events is a uint32,
       so better allocate enough memory here.  */
    local = calloc (size, sizeof (uint32_t));
  else if (type == CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY)
    local = NULL;
  else
    local = malloc (size);

  if (type != CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY)
    *token = malloc (sizeof (struct caf_single_token));

  if (unlikely (*token == NULL
		|| (local == NULL
		    && type != CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY)))
    {
      /* Freeing the memory conditionally seems pointless, but
	 caf_internal_error () may return, when a stat is given and then the
	 memory may be lost.  */
      if (local)
	free (local);
      if (*token)
	free (*token);
      caf_internal_error (alloc_fail_msg, stat, errmsg, errmsg_len);
      return;
    }

  single_token = TOKEN (*token);
  single_token->memptr = local;
  single_token->owning_memory = type != CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY;
  single_token->desc = GFC_DESCRIPTOR_RANK (data) > 0 ? data : NULL;


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
  GFC_DESCRIPTOR_DATA (data) = local;
}


void
_gfortran_caf_deregister (caf_token_t *token, caf_deregister_t type, int *stat,
			  char *errmsg __attribute__ ((unused)),
			  size_t errmsg_len __attribute__ ((unused)))
{
  caf_single_token_t single_token = TOKEN (*token);

  if (single_token->owning_memory && single_token->memptr)
    free (single_token->memptr);

  if (type != CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY)
    {
      free (TOKEN (*token));
      *token = NULL;
    }
  else
    {
      single_token->memptr = NULL;
      single_token->owning_memory = false;
    }

  if (stat)
    *stat = 0;
}


void
_gfortran_caf_sync_all (int *stat,
			char *errmsg __attribute__ ((unused)),
			size_t errmsg_len __attribute__ ((unused)))
{
  __asm__ __volatile__ ("":::"memory");
  if (stat)
    *stat = 0;
}


void
_gfortran_caf_sync_memory (int *stat,
			   char *errmsg __attribute__ ((unused)),
			   size_t errmsg_len __attribute__ ((unused)))
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
			   size_t errmsg_len __attribute__ ((unused)))
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
_gfortran_caf_stop_numeric(int stop_code, bool quiet)
{
  if (!quiet)
    fprintf (stderr, "STOP %d\n", stop_code);
  exit (0);
}


void
_gfortran_caf_stop_str(const char *string, size_t len, bool quiet)
{
  if (!quiet)
    {
      fputs ("STOP ", stderr);
      while (len--)
	fputc (*(string++), stderr);
      fputs ("\n", stderr);
    }
  exit (0);
}


void
_gfortran_caf_error_stop_str (const char *string, size_t len, bool quiet)
{
  if (!quiet)
    {
      fputs ("ERROR STOP ", stderr);
      while (len--)
	fputc (*(string++), stderr);
      fputs ("\n", stderr);
    }
  exit (1);
}


/* Reported that the program terminated because of a fail image issued.
   Because this is a single image library, nothing else than aborting the whole
   program can be done.  */

void _gfortran_caf_fail_image (void)
{
  fputs ("IMAGE FAILED!\n", stderr);
  exit (0);
}


/* Get the status of image IMAGE.  Because being the single image library all
   other images are reported to be stopped.  */

int _gfortran_caf_image_status (int image,
				caf_team_t * team __attribute__ ((unused)))
{
  if (image == 1)
    return 0;
  else
    return CAF_STAT_STOPPED_IMAGE;
}


/* Single image library.  There cannot be any failed images with only one
   image.  */

void
_gfortran_caf_failed_images (gfc_descriptor_t *array,
			     caf_team_t * team __attribute__ ((unused)),
			     int * kind)
{
  int local_kind = kind != NULL ? *kind : 4;

  array->base_addr = NULL;
  array->dtype.type = BT_INTEGER;
  array->dtype.elem_len = local_kind;
   /* Setting lower_bound higher then upper_bound is what the compiler does to
      indicate an empty array.  */
  array->dim[0].lower_bound = 0;
  array->dim[0]._ubound = -1;
  array->dim[0]._stride = 1;
  array->offset = 0;
}


/* With only one image available no other images can be stopped.  Therefore
   return an empty array.  */

void
_gfortran_caf_stopped_images (gfc_descriptor_t *array,
			      caf_team_t * team __attribute__ ((unused)),
			      int * kind)
{
  int local_kind = kind != NULL ? *kind : 4;

  array->base_addr = NULL;
  array->dtype.type =  BT_INTEGER;
  array->dtype.elem_len =  local_kind;
  /* Setting lower_bound higher then upper_bound is what the compiler does to
     indicate an empty array.  */
  array->dim[0].lower_bound = 0;
  array->dim[0]._ubound = -1;
  array->dim[0]._stride = 1;
  array->offset = 0;
}


void
_gfortran_caf_error_stop (int error, bool quiet)
{
  if (!quiet)
    fprintf (stderr, "ERROR STOP %d\n", error);
  exit (error);
}


void
_gfortran_caf_co_broadcast (gfc_descriptor_t *a __attribute__ ((unused)),
			    int source_image __attribute__ ((unused)),
			    int *stat, char *errmsg __attribute__ ((unused)),
			    size_t errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_sum (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_min (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
{
  if (stat)
    *stat = 0;
}

void
_gfortran_caf_co_max (gfc_descriptor_t *a __attribute__ ((unused)),
		      int result_image __attribute__ ((unused)),
		      int *stat, char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
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
                        size_t errmsg_len __attribute__ ((unused)))
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
    memset (&dst[n], ' ', dst_size - n);
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
  typedef _Float128 real128t;
  typedef _Complex _Float128 complex128t;
#elif defined(HAVE_GFC_REAL_10)
  typedef long double real128t;
  typedef _Complex long double complex128t;
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
      return;
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
      return;
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
      return;
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
      void *sr = (void *) ((char *) MEMTOK (token) + offset);
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
	  void *sr = (void *)((char *) MEMTOK (token) + offset
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
      void *sr = (void *)((char *) MEMTOK (token) + offset
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
      void *dst = (void *) ((char *) MEMTOK (token) + offset);
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
	  void *dst = (void *)((char *) MEMTOK (token) + offset
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
      void *dst = (void *)((char *) MEMTOK (token) + offset
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
  GFC_DESCRIPTOR_DATA (src) = (void *) ((char *) MEMTOK (src_token)
					+ src_offset);
  _gfortran_caf_send (dst_token, dst_offset, dst_image_index, dest, dst_vector,
		      src, dst_kind, src_kind, may_require_tmp, NULL);
  GFC_DESCRIPTOR_DATA (src) = src_base;
}


/* Emitted when a theorectically unreachable part is reached.  */
const char unreachable[] = "Fatal error: unreachable alternative found.\n";


static void
copy_data (void *ds, void *sr, int dst_type, int src_type,
	   int dst_kind, int src_kind, size_t dst_size, size_t src_size,
	   size_t num, int *stat)
{
  size_t k;
  if (dst_type == src_type && dst_kind == src_kind)
    {
      memmove (ds, sr, (dst_size > src_size ? src_size : dst_size) * num);
      if ((dst_type == BT_CHARACTER || src_type == BT_CHARACTER)
	  && dst_size > src_size)
	{
	  if (dst_kind == 1)
	    memset ((void*)(char*) ds + src_size, ' ', dst_size-src_size);
	  else /* dst_kind == 4.  */
	    for (k = src_size/4; k < dst_size/4; k++)
	      ((int32_t*) ds)[k] = (int32_t) ' ';
	}
    }
  else if (dst_type == BT_CHARACTER && dst_kind == 1)
    assign_char1_from_char4 (dst_size, src_size, ds, sr);
  else if (dst_type == BT_CHARACTER)
    assign_char4_from_char1 (dst_size, src_size, ds, sr);
  else
    for (k = 0; k < num; ++k)
      {
	convert_type (ds, dst_type, dst_kind, sr, src_type, src_kind, stat);
	ds += dst_size;
	sr += src_size;
      }
}


#define COMPUTE_NUM_ITEMS(num, stride, lb, ub) \
  do { \
    index_type abs_stride = (stride) > 0 ? (stride) : -(stride); \
    num = (stride) > 0 ? (ub) + 1 - (lb) : (lb) + 1 - (ub); \
    if (num <= 0 || abs_stride < 1) return; \
    num = (abs_stride > 1) ? (1 + (num - 1) / abs_stride) : num; \
  } while (0)


static void
get_for_ref (caf_reference_t *ref, size_t *i, size_t *dst_index,
	     caf_single_token_t single_token, gfc_descriptor_t *dst,
	     gfc_descriptor_t *src, void *ds, void *sr,
	     int dst_kind, int src_kind, size_t dst_dim, size_t src_dim,
	     size_t num, int *stat, int src_type)
{
  ptrdiff_t extent_src = 1, array_offset_src = 0, stride_src;
  size_t next_dst_dim;

  if (unlikely (ref == NULL))
    /* May be we should issue an error here, because this case should not
       occur.  */
    return;

  if (ref->next == NULL)
    {
      size_t dst_size = GFC_DESCRIPTOR_SIZE (dst);
      ptrdiff_t array_offset_dst = 0;;
      size_t dst_rank = GFC_DESCRIPTOR_RANK (dst);

      switch (ref->type)
	{
	case CAF_REF_COMPONENT:
	  /* Because the token is always registered after the component, its
	     offset is always greater zero.  */
	  if (ref->u.c.caf_token_offset > 0)
	    /* Note, that sr is dereffed here.  */
	    copy_data (ds, *(void **)(sr + ref->u.c.offset),
		       GFC_DESCRIPTOR_TYPE (dst), src_type,
		       dst_kind, src_kind, dst_size, ref->item_size, 1, stat);
	  else
	    copy_data (ds, sr + ref->u.c.offset,
		       GFC_DESCRIPTOR_TYPE (dst), src_type,
		       dst_kind, src_kind, dst_size, ref->item_size, 1, stat);
	  ++(*i);
	  return;
	case CAF_REF_STATIC_ARRAY:
	  /* Intentionally fall through.  */
	case CAF_REF_ARRAY:
	  if (ref->u.a.mode[src_dim] == CAF_ARR_REF_NONE)
	    {
	      for (size_t d = 0; d < dst_rank; ++d)
		array_offset_dst += dst_index[d];
	      copy_data (ds + array_offset_dst * dst_size, sr,
			 GFC_DESCRIPTOR_TYPE (dst), src_type,
			 dst_kind, src_kind, dst_size, ref->item_size, num,
			 stat);
	      *i += num;
	      return;
	    }
	  break;
	default:
	  caf_runtime_error (unreachable);
	}
    }

  switch (ref->type)
    {
    case CAF_REF_COMPONENT:
      if (ref->u.c.caf_token_offset > 0)
	{
	  single_token = *(caf_single_token_t*)(sr + ref->u.c.caf_token_offset);

	  if (ref->next && ref->next->type == CAF_REF_ARRAY)
	    src = single_token->desc;
	  else
	    src = NULL;

	  if (ref->next && ref->next->type == CAF_REF_COMPONENT)
	    /* The currently ref'ed component was allocatabe (caf_token_offset
	       > 0) and the next ref is a component, too, then the new sr has to
	       be dereffed.  (static arrays cannot be allocatable or they
	       become an array with descriptor.  */
	    sr = *(void **)(sr + ref->u.c.offset);
	  else
	    sr += ref->u.c.offset;

	  get_for_ref (ref->next, i, dst_index, single_token, dst, src,
		       ds, sr, dst_kind, src_kind, dst_dim, 0,
		       1, stat, src_type);
	}
      else
	get_for_ref (ref->next, i, dst_index, single_token, dst,
		     (gfc_descriptor_t *)(sr + ref->u.c.offset), ds,
		     sr + ref->u.c.offset, dst_kind, src_kind, dst_dim, 0, 1,
		     stat, src_type);
      return;
    case CAF_REF_ARRAY:
      if (ref->u.a.mode[src_dim] == CAF_ARR_REF_NONE)
	{
	  get_for_ref (ref->next, i, dst_index, single_token, dst,
		       src, ds, sr, dst_kind, src_kind,
		       dst_dim, 0, 1, stat, src_type);
	  return;
	}
      /* Only when on the left most index switch the data pointer to
	 the array's data pointer.  */
      if (src_dim == 0)
	sr = GFC_DESCRIPTOR_DATA (src);
      switch (ref->u.a.mode[src_dim])
	{
	case CAF_ARR_REF_VECTOR:
	  extent_src = GFC_DIMENSION_EXTENT (src->dim[src_dim]);
	  array_offset_src = 0;
	  dst_index[dst_dim] = 0;
	  for (size_t idx = 0; idx < ref->u.a.dim[src_dim].v.nvec;
	       ++idx)
	    {
#define KINDCASE(kind, type) case kind: \
	      array_offset_src = (((index_type) \
		  ((type *)ref->u.a.dim[src_dim].v.vector)[idx]) \
		  - GFC_DIMENSION_LBOUND (src->dim[src_dim])) \
		  * GFC_DIMENSION_STRIDE (src->dim[src_dim]); \
	      break

	      switch (ref->u.a.dim[src_dim].v.kind)
		{
		KINDCASE (1, GFC_INTEGER_1);
		KINDCASE (2, GFC_INTEGER_2);
		KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		KINDCASE (16, GFC_INTEGER_16);
#endif
		default:
		  caf_runtime_error (unreachable);
		  return;
		}
#undef KINDCASE

	      get_for_ref (ref, i, dst_index, single_token, dst, src,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	    }
	  return;
	case CAF_ARR_REF_FULL:
	  COMPUTE_NUM_ITEMS (extent_src,
			     ref->u.a.dim[src_dim].s.stride,
			     GFC_DIMENSION_LBOUND (src->dim[src_dim]),
			     GFC_DIMENSION_UBOUND (src->dim[src_dim]));
	  stride_src = src->dim[src_dim]._stride
	      * ref->u.a.dim[src_dim].s.stride;
	  array_offset_src = 0;
	  dst_index[dst_dim] = 0;
	  for (index_type idx = 0; idx < extent_src;
	       ++idx, array_offset_src += stride_src)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, src,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	    }
	  return;
	case CAF_ARR_REF_RANGE:
	  COMPUTE_NUM_ITEMS (extent_src,
			     ref->u.a.dim[src_dim].s.stride,
			     ref->u.a.dim[src_dim].s.start,
			     ref->u.a.dim[src_dim].s.end);
	  array_offset_src = (ref->u.a.dim[src_dim].s.start
			      - GFC_DIMENSION_LBOUND (src->dim[src_dim]))
	      * GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	  stride_src = GFC_DIMENSION_STRIDE (src->dim[src_dim])
	      * ref->u.a.dim[src_dim].s.stride;
	  dst_index[dst_dim] = 0;
	  /* Increase the dst_dim only, when the src_extent is greater one
	     or src and dst extent are both one.  Don't increase when the scalar
	     source is not present in the dst.  */
	  next_dst_dim = extent_src > 1
	      || (GFC_DIMENSION_EXTENT (dst->dim[dst_dim]) == 1
		  && extent_src == 1) ? (dst_dim + 1) : dst_dim;
	  for (index_type idx = 0; idx < extent_src; ++idx)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, src,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, next_dst_dim, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	      array_offset_src += stride_src;
	    }
	  return;
	case CAF_ARR_REF_SINGLE:
	  array_offset_src = (ref->u.a.dim[src_dim].s.start
			      - src->dim[src_dim].lower_bound)
	      * GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	  dst_index[dst_dim] = 0;
	  get_for_ref (ref, i, dst_index, single_token, dst, src, ds,
		       sr + array_offset_src * ref->item_size,
		       dst_kind, src_kind, dst_dim, src_dim + 1, 1,
		       stat, src_type);
	  return;
	case CAF_ARR_REF_OPEN_END:
	  COMPUTE_NUM_ITEMS (extent_src,
			     ref->u.a.dim[src_dim].s.stride,
			     ref->u.a.dim[src_dim].s.start,
			     GFC_DIMENSION_UBOUND (src->dim[src_dim]));
	  stride_src = GFC_DIMENSION_STRIDE (src->dim[src_dim])
	      * ref->u.a.dim[src_dim].s.stride;
	  array_offset_src = (ref->u.a.dim[src_dim].s.start
			      - GFC_DIMENSION_LBOUND (src->dim[src_dim]))
	      * GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	  dst_index[dst_dim] = 0;
	  for (index_type idx = 0; idx < extent_src; ++idx)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, src,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	      array_offset_src += stride_src;
	    }
	  return;
	case CAF_ARR_REF_OPEN_START:
	  COMPUTE_NUM_ITEMS (extent_src,
			     ref->u.a.dim[src_dim].s.stride,
			     GFC_DIMENSION_LBOUND (src->dim[src_dim]),
			     ref->u.a.dim[src_dim].s.end);
	  stride_src = GFC_DIMENSION_STRIDE (src->dim[src_dim])
	      * ref->u.a.dim[src_dim].s.stride;
	  array_offset_src = 0;
	  dst_index[dst_dim] = 0;
	  for (index_type idx = 0; idx < extent_src; ++idx)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, src,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	      array_offset_src += stride_src;
	    }
	  return;
	default:
	  caf_runtime_error (unreachable);
	}
      return;
    case CAF_REF_STATIC_ARRAY:
      if (ref->u.a.mode[src_dim] == CAF_ARR_REF_NONE)
	{
	  get_for_ref (ref->next, i, dst_index, single_token, dst,
		       NULL, ds, sr, dst_kind, src_kind,
		       dst_dim, 0, 1, stat, src_type);
	  return;
	}
      switch (ref->u.a.mode[src_dim])
	{
	case CAF_ARR_REF_VECTOR:
	  array_offset_src = 0;
	  dst_index[dst_dim] = 0;
	  for (size_t idx = 0; idx < ref->u.a.dim[src_dim].v.nvec;
	       ++idx)
	    {
#define KINDCASE(kind, type) case kind: \
	     array_offset_src = ((type *)ref->u.a.dim[src_dim].v.vector)[idx]; \
	      break

	      switch (ref->u.a.dim[src_dim].v.kind)
		{
		KINDCASE (1, GFC_INTEGER_1);
		KINDCASE (2, GFC_INTEGER_2);
		KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		KINDCASE (16, GFC_INTEGER_16);
#endif
		default:
		  caf_runtime_error (unreachable);
		  return;
		}
#undef KINDCASE

	      get_for_ref (ref, i, dst_index, single_token, dst, NULL,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	    }
	  return;
	case CAF_ARR_REF_FULL:
	  dst_index[dst_dim] = 0;
	  for (array_offset_src = 0 ;
	       array_offset_src <= ref->u.a.dim[src_dim].s.end;
	       array_offset_src += ref->u.a.dim[src_dim].s.stride)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, NULL,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	    }
	  return;
	case CAF_ARR_REF_RANGE:
	  COMPUTE_NUM_ITEMS (extent_src,
			     ref->u.a.dim[src_dim].s.stride,
			     ref->u.a.dim[src_dim].s.start,
			     ref->u.a.dim[src_dim].s.end);
	  array_offset_src = ref->u.a.dim[src_dim].s.start;
	  dst_index[dst_dim] = 0;
	  for (index_type idx = 0; idx < extent_src; ++idx)
	    {
	      get_for_ref (ref, i, dst_index, single_token, dst, NULL,
			   ds, sr + array_offset_src * ref->item_size,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, stat, src_type);
	      dst_index[dst_dim]
		  += GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	      array_offset_src += ref->u.a.dim[src_dim].s.stride;
	    }
	  return;
	case CAF_ARR_REF_SINGLE:
	  array_offset_src = ref->u.a.dim[src_dim].s.start;
	  get_for_ref (ref, i, dst_index, single_token, dst, NULL, ds,
		       sr + array_offset_src * ref->item_size,
		       dst_kind, src_kind, dst_dim, src_dim + 1, 1,
		       stat, src_type);
	  return;
	/* The OPEN_* are mapped to a RANGE and therefore cannot occur.  */
	case CAF_ARR_REF_OPEN_END:
	case CAF_ARR_REF_OPEN_START:
	default:
	  caf_runtime_error (unreachable);
	}
      return;
    default:
      caf_runtime_error (unreachable);
    }
}


void
_gfortran_caf_get_by_ref (caf_token_t token,
			  int image_index __attribute__ ((unused)),
			  gfc_descriptor_t *dst, caf_reference_t *refs,
			  int dst_kind, int src_kind,
			  bool may_require_tmp __attribute__ ((unused)),
			  bool dst_reallocatable, int *stat,
			  int src_type)
{
  const char vecrefunknownkind[] = "libcaf_single::caf_get_by_ref(): "
				   "unknown kind in vector-ref.\n";
  const char unknownreftype[] = "libcaf_single::caf_get_by_ref(): "
				"unknown reference type.\n";
  const char unknownarrreftype[] = "libcaf_single::caf_get_by_ref(): "
				   "unknown array reference type.\n";
  const char rankoutofrange[] = "libcaf_single::caf_get_by_ref(): "
				"rank out of range.\n";
  const char extentoutofrange[] = "libcaf_single::caf_get_by_ref(): "
				  "extent out of range.\n";
  const char cannotallocdst[] = "libcaf_single::caf_get_by_ref(): "
				"cannot allocate memory.\n";
  const char nonallocextentmismatch[] = "libcaf_single::caf_get_by_ref(): "
      "extent of non-allocatable arrays mismatch (%lu != %lu).\n";
  const char doublearrayref[] = "libcaf_single::caf_get_by_ref(): "
      "two or more array part references are not supported.\n";
  size_t size, i;
  size_t dst_index[GFC_MAX_DIMENSIONS];
  int dst_rank = GFC_DESCRIPTOR_RANK (dst);
  int dst_cur_dim = 0;
  size_t src_size = 0;
  caf_single_token_t single_token = TOKEN (token);
  void *memptr = single_token->memptr;
  gfc_descriptor_t *src = single_token->desc;
  caf_reference_t *riter = refs;
  long delta;
  /* Reallocation of dst.data is needed (e.g., array to small).  */
  bool realloc_needed;
  /* Reallocation of dst.data is required, because data is not alloced at
     all.  */
  bool realloc_required;
  bool extent_mismatch = false;
  /* Set when the first non-scalar array reference is encountered.  */
  bool in_array_ref = false;
  bool array_extent_fixed = false;
  realloc_needed = realloc_required = GFC_DESCRIPTOR_DATA (dst) == NULL;

  assert (!realloc_needed || dst_reallocatable);

  if (stat)
    *stat = 0;

  /* Compute the size of the result.  In the beginning size just counts the
     number of elements.  */
  size = 1;
  while (riter)
    {
      switch (riter->type)
	{
	case CAF_REF_COMPONENT:
	  if (riter->u.c.caf_token_offset)
	    {
	      single_token = *(caf_single_token_t*)
					 (memptr + riter->u.c.caf_token_offset);
	      memptr = single_token->memptr;
	      src = single_token->desc;
	    }
	  else
	    {
	      memptr += riter->u.c.offset;
	      /* When the next ref is an array ref, assume there is an
		 array descriptor at memptr.  Note, static arrays do not have
		 a descriptor.  */
	      if (riter->next && riter->next->type == CAF_REF_ARRAY)
		src = (gfc_descriptor_t *)memptr;
	      else
		src = NULL;
	    }
	  break;
	case CAF_REF_ARRAY:
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_VECTOR:
		  delta = riter->u.a.dim[i].v.nvec;
#define KINDCASE(kind, type) case kind: \
		    memptr += (((index_type) \
			((type *)riter->u.a.dim[i].v.vector)[0]) \
			- GFC_DIMENSION_LBOUND (src->dim[i])) \
			* GFC_DIMENSION_STRIDE (src->dim[i]) \
			* riter->item_size; \
		    break

		  switch (riter->u.a.dim[i].v.kind)
		    {
		    KINDCASE (1, GFC_INTEGER_1);
		    KINDCASE (2, GFC_INTEGER_2);
		    KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		    KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		    KINDCASE (16, GFC_INTEGER_16);
#endif
		    default:
		      caf_internal_error (vecrefunknownkind, stat, NULL, 0);
		      return;
		    }
#undef KINDCASE
		  break;
		case CAF_ARR_REF_FULL:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     GFC_DIMENSION_LBOUND (src->dim[i]),
				     GFC_DIMENSION_UBOUND (src->dim[i]));
		  /* The memptr stays unchanged when ref'ing the first element
		     in a dimension.  */
		  break;
		case CAF_ARR_REF_RANGE:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     riter->u.a.dim[i].s.start,
				     riter->u.a.dim[i].s.end);
		  memptr += (riter->u.a.dim[i].s.start
			     - GFC_DIMENSION_LBOUND (src->dim[i]))
		      * GFC_DIMENSION_STRIDE (src->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_SINGLE:
		  delta = 1;
		  memptr += (riter->u.a.dim[i].s.start
			     - GFC_DIMENSION_LBOUND (src->dim[i]))
		      * GFC_DIMENSION_STRIDE (src->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_END:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     riter->u.a.dim[i].s.start,
				     GFC_DIMENSION_UBOUND (src->dim[i]));
		  memptr += (riter->u.a.dim[i].s.start
			     - GFC_DIMENSION_LBOUND (src->dim[i]))
		      * GFC_DIMENSION_STRIDE (src->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_START:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     GFC_DIMENSION_LBOUND (src->dim[i]),
				     riter->u.a.dim[i].s.end);
		  /* The memptr stays unchanged when ref'ing the first element
		     in a dimension.  */
		  break;
		default:
		  caf_internal_error (unknownarrreftype, stat, NULL, 0);
		  return;
		}
	      if (delta <= 0)
		return;
	      /* Check the various properties of the destination array.
		 Is an array expected and present?  */
	      if (delta > 1 && dst_rank == 0)
		{
		  /* No, an array is required, but not provided.  */
		  caf_internal_error (extentoutofrange, stat, NULL, 0);
		  return;
		}
	      /* Special mode when called by __caf_sendget_by_ref ().  */
	      if (dst_rank == -1 && GFC_DESCRIPTOR_DATA (dst) == NULL)
		{
		  dst_rank = dst_cur_dim + 1;
		  GFC_DESCRIPTOR_RANK (dst) = dst_rank;
		  GFC_DESCRIPTOR_SIZE (dst) = dst_kind;
		}
	      /* When dst is an array.  */
	      if (dst_rank > 0)
		{
		  /* Check that dst_cur_dim is valid for dst.  Can be
		     superceeded only by scalar data.  */
		  if (dst_cur_dim >= dst_rank && delta != 1)
		    {
		      caf_internal_error (rankoutofrange, stat, NULL, 0);
		      return;
		    }
		  /* Do further checks, when the source is not scalar.  */
		  else if (delta != 1)
		    {
		      /* Check that the extent is not scalar and we are not in
			 an array ref for the dst side.  */
		      if (!in_array_ref)
			{
			  /* Check that this is the non-scalar extent.  */
			  if (!array_extent_fixed)
			    {
			      /* In an array extent now.  */
			      in_array_ref = true;
			      /* Check that we haven't skipped any scalar
				 dimensions yet and that the dst is
				 compatible.  */
			      if (i > 0
				  && dst_rank == GFC_DESCRIPTOR_RANK (src))
				{
				  if (dst_reallocatable)
				    {
				      /* Dst is reallocatable, which means that
					 the bounds are not set.  Set them.  */
				      for (dst_cur_dim= 0; dst_cur_dim < (int)i;
					   ++dst_cur_dim)
				       GFC_DIMENSION_SET (dst->dim[dst_cur_dim],
							  1, 1, 1);
				    }
				  else
				    dst_cur_dim = i;
				}
			      /* Else press thumbs, that there are enough
				 dimensional refs to come.  Checked below.  */
			    }
			  else
			    {
			      caf_internal_error (doublearrayref, stat, NULL,
						  0);
			      return;
			    }
			}
		      /* When the realloc is required, then no extent may have
			 been set.  */
		      extent_mismatch = realloc_required
			  || GFC_DESCRIPTOR_EXTENT (dst, dst_cur_dim) != delta;
		      /* When it already known, that a realloc is needed or
			 the extent does not match the needed one.  */
		      if (realloc_required || realloc_needed
			  || extent_mismatch)
			{
			  /* Check whether dst is reallocatable.  */
			  if (unlikely (!dst_reallocatable))
			    {
			      caf_internal_error (nonallocextentmismatch, stat,
						  NULL, 0, delta,
						  GFC_DESCRIPTOR_EXTENT (dst,
								  dst_cur_dim));
			      return;
			    }
			  /* Only report an error, when the extent needs to be
			     modified, which is not allowed.  */
			  else if (!dst_reallocatable && extent_mismatch)
			    {
			      caf_internal_error (extentoutofrange, stat, NULL,
						  0);
			      return;
			    }
			  realloc_needed = true;
			}
		      /* Only change the extent when it does not match.  This is
			 to prevent resetting given array bounds.  */
		      if (extent_mismatch)
			GFC_DIMENSION_SET (dst->dim[dst_cur_dim], 1, delta,
					   size);
		    }

		  /* Only increase the dim counter, when in an array ref.  */
		  if (in_array_ref && dst_cur_dim < dst_rank)
		    ++dst_cur_dim;
		}
	      size *= (index_type)delta;
	    }
	  if (in_array_ref)
	    {
	      array_extent_fixed = true;
	      in_array_ref = false;
	      /* Check, if we got less dimensional refs than the rank of dst
		 expects.  */
	      assert (dst_cur_dim == GFC_DESCRIPTOR_RANK (dst));
	    }
	  break;
	case CAF_REF_STATIC_ARRAY:
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_VECTOR:
		  delta = riter->u.a.dim[i].v.nvec;
#define KINDCASE(kind, type) case kind: \
		    memptr += ((type *)riter->u.a.dim[i].v.vector)[0] \
			* riter->item_size; \
		    break

		  switch (riter->u.a.dim[i].v.kind)
		    {
		    KINDCASE (1, GFC_INTEGER_1);
		    KINDCASE (2, GFC_INTEGER_2);
		    KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		    KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		    KINDCASE (16, GFC_INTEGER_16);
#endif
		    default:
		      caf_internal_error (vecrefunknownkind, stat, NULL, 0);
		      return;
		    }
#undef KINDCASE
		  break;
		case CAF_ARR_REF_FULL:
		  delta = riter->u.a.dim[i].s.end / riter->u.a.dim[i].s.stride
		      + 1;
		  /* The memptr stays unchanged when ref'ing the first element
		     in a dimension.  */
		  break;
		case CAF_ARR_REF_RANGE:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     riter->u.a.dim[i].s.start,
				     riter->u.a.dim[i].s.end);
		  memptr += riter->u.a.dim[i].s.start
		      * riter->u.a.dim[i].s.stride
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_SINGLE:
		  delta = 1;
		  memptr += riter->u.a.dim[i].s.start
		      * riter->u.a.dim[i].s.stride
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_END:
		  /* This and OPEN_START are mapped to a RANGE and therefore
		     cannot occur here.  */
		case CAF_ARR_REF_OPEN_START:
		default:
		  caf_internal_error (unknownarrreftype, stat, NULL, 0);
		  return;
		}
	      if (delta <= 0)
		return;
	      /* Check the various properties of the destination array.
		 Is an array expected and present?  */
	      if (delta > 1 && dst_rank == 0)
		{
		  /* No, an array is required, but not provided.  */
		  caf_internal_error (extentoutofrange, stat, NULL, 0);
		  return;
		}
	      /* Special mode when called by __caf_sendget_by_ref ().  */
	      if (dst_rank == -1 && GFC_DESCRIPTOR_DATA (dst) == NULL)
		{
		  dst_rank = dst_cur_dim + 1;
		  GFC_DESCRIPTOR_RANK (dst) = dst_rank;
		  GFC_DESCRIPTOR_SIZE (dst) = dst_kind;
		}
	      /* When dst is an array.  */
	      if (dst_rank > 0)
		{
		  /* Check that dst_cur_dim is valid for dst.  Can be
		     superceeded only by scalar data.  */
		  if (dst_cur_dim >= dst_rank && delta != 1)
		    {
		      caf_internal_error (rankoutofrange, stat, NULL, 0);
		      return;
		    }
		  /* Do further checks, when the source is not scalar.  */
		  else if (delta != 1)
		    {
		      /* Check that the extent is not scalar and we are not in
			 an array ref for the dst side.  */
		      if (!in_array_ref)
			{
			  /* Check that this is the non-scalar extent.  */
			  if (!array_extent_fixed)
			    {
			      /* In an array extent now.  */
			      in_array_ref = true;
			      /* The dst is not reallocatable, so nothing more
				 to do, then correct the dim counter.  */
			      dst_cur_dim = i;
			    }
			  else
			    {
			      caf_internal_error (doublearrayref, stat, NULL,
						  0);
			      return;
			    }
			}
		      /* When the realloc is required, then no extent may have
			 been set.  */
		      extent_mismatch = realloc_required
			  || GFC_DESCRIPTOR_EXTENT (dst, dst_cur_dim) != delta;
		      /* When it is already known, that a realloc is needed or
			 the extent does not match the needed one.  */
		      if (realloc_required || realloc_needed
			  || extent_mismatch)
			{
			  /* Check whether dst is reallocatable.  */
			  if (unlikely (!dst_reallocatable))
			    {
			      caf_internal_error (nonallocextentmismatch, stat,
						  NULL, 0, delta,
						  GFC_DESCRIPTOR_EXTENT (dst,
								  dst_cur_dim));
			      return;
			    }
			  /* Only report an error, when the extent needs to be
			     modified, which is not allowed.  */
			  else if (!dst_reallocatable && extent_mismatch)
			    {
			      caf_internal_error (extentoutofrange, stat, NULL,
						  0);
			      return;
			    }
			  realloc_needed = true;
			}
		      /* Only change the extent when it does not match.  This is
			 to prevent resetting given array bounds.  */
		      if (extent_mismatch)
			GFC_DIMENSION_SET (dst->dim[dst_cur_dim], 1, delta,
					   size);
		    }
		  /* Only increase the dim counter, when in an array ref.  */
		  if (in_array_ref && dst_cur_dim < dst_rank)
		    ++dst_cur_dim;
		}
	      size *= (index_type)delta;
	    }
	  if (in_array_ref)
	    {
	      array_extent_fixed = true;
	      in_array_ref = false;
	      /* Check, if we got less dimensional refs than the rank of dst
		 expects.  */
	      assert (dst_cur_dim == GFC_DESCRIPTOR_RANK (dst));
	    }
	  break;
	default:
	  caf_internal_error (unknownreftype, stat, NULL, 0);
	  return;
	}
      src_size = riter->item_size;
      riter = riter->next;
    }
  if (size == 0 || src_size == 0)
    return;
  /* Postcondition:
     - size contains the number of elements to store in the destination array,
     - src_size gives the size in bytes of each item in the destination array.
  */

  if (realloc_needed)
    {
      if (!array_extent_fixed)
	{
	  assert (size == 1);
	  /* Special mode when called by __caf_sendget_by_ref ().  */
	  if (dst_rank == -1 && GFC_DESCRIPTOR_DATA (dst) == NULL)
	    {
	      dst_rank = dst_cur_dim + 1;
	      GFC_DESCRIPTOR_RANK (dst) = dst_rank;
	      GFC_DESCRIPTOR_SIZE (dst) = dst_kind;
	    }
	  /* This can happen only, when the result is scalar.  */
	  for (dst_cur_dim = 0; dst_cur_dim < dst_rank; ++dst_cur_dim)
	    GFC_DIMENSION_SET (dst->dim[dst_cur_dim], 1, 1, 1);
	}

      GFC_DESCRIPTOR_DATA (dst) = malloc (size * GFC_DESCRIPTOR_SIZE (dst));
      if (unlikely (GFC_DESCRIPTOR_DATA (dst) == NULL))
	{
	  caf_internal_error (cannotallocdst, stat, NULL, 0);
	  return;
	}
    }

  /* Reset the token.  */
  single_token = TOKEN (token);
  memptr = single_token->memptr;
  src = single_token->desc;
  memset(dst_index, 0, sizeof (dst_index));
  i = 0;
  get_for_ref (refs, &i, dst_index, single_token, dst, src,
	       GFC_DESCRIPTOR_DATA (dst), memptr, dst_kind, src_kind, 0, 0,
	       1, stat, src_type);
}


static void
send_by_ref (caf_reference_t *ref, size_t *i, size_t *src_index,
	     caf_single_token_t single_token, gfc_descriptor_t *dst,
	     gfc_descriptor_t *src, void *ds, void *sr,
	     int dst_kind, int src_kind, size_t dst_dim, size_t src_dim,
	     size_t num, size_t size, int *stat, int dst_type)
{
  const char vecrefunknownkind[] = "libcaf_single::caf_send_by_ref(): "
      "unknown kind in vector-ref.\n";
  ptrdiff_t extent_dst = 1, array_offset_dst = 0, stride_dst;
  const size_t src_rank = GFC_DESCRIPTOR_RANK (src);

  if (unlikely (ref == NULL))
    /* May be we should issue an error here, because this case should not
       occur.  */
    return;

  if (ref->next == NULL)
    {
      size_t src_size = GFC_DESCRIPTOR_SIZE (src);
      ptrdiff_t array_offset_src = 0;;

      switch (ref->type)
	{
	case CAF_REF_COMPONENT:
	  if (ref->u.c.caf_token_offset > 0)
	    {
	      if (*(void**)(ds + ref->u.c.offset) == NULL)
		{
		  /* Create a scalar temporary array descriptor.  */
		  gfc_descriptor_t static_dst;
		  GFC_DESCRIPTOR_DATA (&static_dst) = NULL;
		  GFC_DESCRIPTOR_DTYPE (&static_dst)
		      = GFC_DESCRIPTOR_DTYPE (src);
		  /* The component can be allocated now, because it is a
		     scalar.  */
		  _gfortran_caf_register (ref->item_size,
					  CAF_REGTYPE_COARRAY_ALLOC,
					  ds + ref->u.c.caf_token_offset,
					  &static_dst, stat, NULL, 0);
		  single_token = *(caf_single_token_t *)
					       (ds + ref->u.c.caf_token_offset);
		  /* In case of an error in allocation return.  When stat is
		     NULL, then register_component() terminates on error.  */
		  if (stat != NULL && *stat)
		    return;
		  /* Publish the allocated memory.  */
		  *((void **)(ds + ref->u.c.offset))
		      = GFC_DESCRIPTOR_DATA (&static_dst);
		  ds = GFC_DESCRIPTOR_DATA (&static_dst);
		  /* Set the type from the src.  */
		  dst_type = GFC_DESCRIPTOR_TYPE (src);
		}
	      else
		{
		  single_token = *(caf_single_token_t *)
					       (ds + ref->u.c.caf_token_offset);
		  dst = single_token->desc;
		  if (dst)
		    {
		      ds = GFC_DESCRIPTOR_DATA (dst);
		      dst_type = GFC_DESCRIPTOR_TYPE (dst);
		    }
		  else
		    ds = *(void **)(ds + ref->u.c.offset);
		}
	      copy_data (ds, sr, dst_type, GFC_DESCRIPTOR_TYPE (src),
			 dst_kind, src_kind, ref->item_size, src_size, 1, stat);
	    }
	  else
	    copy_data (ds + ref->u.c.offset, sr, dst_type,
		       GFC_DESCRIPTOR_TYPE (src),
		       dst_kind, src_kind, ref->item_size, src_size, 1, stat);
	  ++(*i);
	  return;
	case CAF_REF_STATIC_ARRAY:
	  /* Intentionally fall through.  */
	case CAF_REF_ARRAY:
	  if (ref->u.a.mode[dst_dim] == CAF_ARR_REF_NONE)
	    {
	      if (src_rank > 0)
		{
		  for (size_t d = 0; d < src_rank; ++d)
		    array_offset_src += src_index[d];
		  copy_data (ds, sr + array_offset_src * src_size,
			     dst_type, GFC_DESCRIPTOR_TYPE (src), dst_kind,
			     src_kind, ref->item_size, src_size, num, stat);
		}
	      else
		copy_data (ds, sr, dst_type, GFC_DESCRIPTOR_TYPE (src),
			   dst_kind, src_kind, ref->item_size, src_size, num,
			   stat);
	      *i += num;
	      return;
	    }
	  break;
	default:
	  caf_runtime_error (unreachable);
	}
    }

  switch (ref->type)
    {
    case CAF_REF_COMPONENT:
      if (ref->u.c.caf_token_offset > 0)
	{
	  if (*(void**)(ds + ref->u.c.offset) == NULL)
	    {
	      /* This component refs an unallocated array.  Non-arrays are
		 caught in the if (!ref->next) above.  */
	      dst = (gfc_descriptor_t *)(ds + ref->u.c.offset);
	      /* Assume that the rank and the dimensions fit for copying src
		 to dst.  */
	      GFC_DESCRIPTOR_DTYPE (dst) = GFC_DESCRIPTOR_DTYPE (src);
	      dst->offset = 0;
	      stride_dst = 1;
	      for (size_t d = 0; d < src_rank; ++d)
		{
		  extent_dst = GFC_DIMENSION_EXTENT (src->dim[d]);
		  GFC_DIMENSION_LBOUND (dst->dim[d]) = 0;
		  GFC_DIMENSION_UBOUND (dst->dim[d]) = extent_dst - 1;
		  GFC_DIMENSION_STRIDE (dst->dim[d]) = stride_dst;
		  stride_dst *= extent_dst;
		}
	      /* Null the data-pointer to make register_component allocate
		 its own memory.  */
	      GFC_DESCRIPTOR_DATA (dst) = NULL;

	      /* The size of the array is given by size.  */
	      _gfortran_caf_register (size * ref->item_size,
				      CAF_REGTYPE_COARRAY_ALLOC,
				      ds + ref->u.c.caf_token_offset,
				      dst, stat, NULL, 0);
	      /* In case of an error in allocation return.  When stat is
		 NULL, then register_component() terminates on error.  */
	      if (stat != NULL && *stat)
		return;
	    }
	  single_token = *(caf_single_token_t*)(ds + ref->u.c.caf_token_offset);
	  /* When a component is allocatable (caf_token_offset != 0) and not an
	     array (ref->next->type == CAF_REF_COMPONENT), then ds has to be
	     dereffed.  */
	  if (ref->next && ref->next->type == CAF_REF_COMPONENT)
	    ds = *(void **)(ds + ref->u.c.offset);
	  else
	    ds += ref->u.c.offset;

	  send_by_ref (ref->next, i, src_index, single_token,
		       single_token->desc, src, ds, sr,
		       dst_kind, src_kind, 0, src_dim, 1, size, stat, dst_type);
	}
      else
	send_by_ref (ref->next, i, src_index, single_token,
		     (gfc_descriptor_t *)(ds + ref->u.c.offset), src,
		     ds + ref->u.c.offset, sr, dst_kind, src_kind, 0, src_dim,
		     1, size, stat, dst_type);
      return;
    case CAF_REF_ARRAY:
      if (ref->u.a.mode[dst_dim] == CAF_ARR_REF_NONE)
	{
	  send_by_ref (ref->next, i, src_index, single_token,
		       (gfc_descriptor_t *)ds, src, ds, sr, dst_kind, src_kind,
		       0, src_dim, 1, size, stat, dst_type);
	  return;
	}
      /* Only when on the left most index switch the data pointer to
	 the array's data pointer.  And only for non-static arrays.  */
      if (dst_dim == 0 && ref->type != CAF_REF_STATIC_ARRAY)
	ds = GFC_DESCRIPTOR_DATA (dst);
      switch (ref->u.a.mode[dst_dim])
	{
	case CAF_ARR_REF_VECTOR:
	  array_offset_dst = 0;
	  src_index[src_dim] = 0;
	  for (size_t idx = 0; idx < ref->u.a.dim[dst_dim].v.nvec;
	       ++idx)
	    {
#define KINDCASE(kind, type) case kind: \
	      array_offset_dst = (((index_type) \
		  ((type *)ref->u.a.dim[dst_dim].v.vector)[idx]) \
		  - GFC_DIMENSION_LBOUND (dst->dim[dst_dim])) \
		  * GFC_DIMENSION_STRIDE (dst->dim[dst_dim]); \
	      break

	      switch (ref->u.a.dim[dst_dim].v.kind)
		{
		KINDCASE (1, GFC_INTEGER_1);
		KINDCASE (2, GFC_INTEGER_2);
		KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		KINDCASE (16, GFC_INTEGER_16);
#endif
		default:
		  caf_internal_error (vecrefunknownkind, stat, NULL, 0);
		  return;
		}
#undef KINDCASE

	      send_by_ref (ref, i, src_index, single_token, dst, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	    }
	  return;
	case CAF_ARR_REF_FULL:
	  COMPUTE_NUM_ITEMS (extent_dst,
			     ref->u.a.dim[dst_dim].s.stride,
			     GFC_DIMENSION_LBOUND (dst->dim[dst_dim]),
			     GFC_DIMENSION_UBOUND (dst->dim[dst_dim]));
	  array_offset_dst = 0;
	  stride_dst = GFC_DIMENSION_STRIDE (dst->dim[dst_dim])
	      * ref->u.a.dim[dst_dim].s.stride;
	  src_index[src_dim] = 0;
	  for (index_type idx = 0; idx < extent_dst;
	       ++idx, array_offset_dst += stride_dst)
	    {
	      send_by_ref (ref, i, src_index, single_token, dst, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	    }
	  return;
	case CAF_ARR_REF_RANGE:
	  COMPUTE_NUM_ITEMS (extent_dst,
			     ref->u.a.dim[dst_dim].s.stride,
			     ref->u.a.dim[dst_dim].s.start,
			     ref->u.a.dim[dst_dim].s.end);
	  array_offset_dst = ref->u.a.dim[dst_dim].s.start
	      - GFC_DIMENSION_LBOUND (dst->dim[dst_dim]);
	  stride_dst = GFC_DIMENSION_STRIDE (dst->dim[dst_dim])
	      * ref->u.a.dim[dst_dim].s.stride;
	  src_index[src_dim] = 0;
	  for (index_type idx = 0; idx < extent_dst; ++idx)
	    {
	      send_by_ref (ref, i, src_index, single_token, dst, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	      array_offset_dst += stride_dst;
	    }
	  return;
	case CAF_ARR_REF_SINGLE:
	  array_offset_dst = (ref->u.a.dim[dst_dim].s.start
			       - GFC_DIMENSION_LBOUND (dst->dim[dst_dim]))
			     * GFC_DIMENSION_STRIDE (dst->dim[dst_dim]);
	  send_by_ref (ref, i, src_index, single_token, dst, src, ds
		       + array_offset_dst * ref->item_size, sr,
		       dst_kind, src_kind, dst_dim + 1, src_dim, 1,
		       size, stat, dst_type);
	  return;
	case CAF_ARR_REF_OPEN_END:
	  COMPUTE_NUM_ITEMS (extent_dst,
			     ref->u.a.dim[dst_dim].s.stride,
			     ref->u.a.dim[dst_dim].s.start,
			     GFC_DIMENSION_UBOUND (dst->dim[dst_dim]));
	  array_offset_dst = ref->u.a.dim[dst_dim].s.start
	      - GFC_DIMENSION_LBOUND (dst->dim[dst_dim]);
	  stride_dst = GFC_DIMENSION_STRIDE (dst->dim[dst_dim])
	      * ref->u.a.dim[dst_dim].s.stride;
	  src_index[src_dim] = 0;
	  for (index_type idx = 0; idx < extent_dst; ++idx)
	    {
	      send_by_ref (ref, i, src_index, single_token, dst, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	      array_offset_dst += stride_dst;
	    }
	  return;
	case CAF_ARR_REF_OPEN_START:
	  COMPUTE_NUM_ITEMS (extent_dst,
			     ref->u.a.dim[dst_dim].s.stride,
			     GFC_DIMENSION_LBOUND (dst->dim[dst_dim]),
			     ref->u.a.dim[dst_dim].s.end);
	  array_offset_dst = 0;
	  stride_dst = GFC_DIMENSION_STRIDE (dst->dim[dst_dim])
	      * ref->u.a.dim[dst_dim].s.stride;
	  src_index[src_dim] = 0;
	  for (index_type idx = 0; idx < extent_dst; ++idx)
	    {
	      send_by_ref (ref, i, src_index, single_token, dst, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	      array_offset_dst += stride_dst;
	    }
	  return;
	default:
	  caf_runtime_error (unreachable);
	}
      return;
    case CAF_REF_STATIC_ARRAY:
      if (ref->u.a.mode[dst_dim] == CAF_ARR_REF_NONE)
	{
	  send_by_ref (ref->next, i, src_index, single_token, NULL,
		       src, ds, sr, dst_kind, src_kind,
		       0, src_dim, 1, size, stat, dst_type);
	  return;
	}
      switch (ref->u.a.mode[dst_dim])
	{
	case CAF_ARR_REF_VECTOR:
	  array_offset_dst = 0;
	  src_index[src_dim] = 0;
	  for (size_t idx = 0; idx < ref->u.a.dim[dst_dim].v.nvec;
	       ++idx)
	    {
#define KINDCASE(kind, type) case kind: \
	     array_offset_dst = ((type *)ref->u.a.dim[dst_dim].v.vector)[idx]; \
	      break

	      switch (ref->u.a.dim[dst_dim].v.kind)
		{
		KINDCASE (1, GFC_INTEGER_1);
		KINDCASE (2, GFC_INTEGER_2);
		KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		KINDCASE (16, GFC_INTEGER_16);
#endif
		default:
		  caf_runtime_error (unreachable);
		  return;
		}
#undef KINDCASE

	      send_by_ref (ref, i, src_index, single_token, NULL, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      src_index[src_dim]
		  += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	    }
	  return;
	case CAF_ARR_REF_FULL:
	  src_index[src_dim] = 0;
	  for (array_offset_dst = 0 ;
	       array_offset_dst <= ref->u.a.dim[dst_dim].s.end;
	       array_offset_dst += ref->u.a.dim[dst_dim].s.stride)
	    {
	      send_by_ref (ref, i, src_index, single_token, NULL, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	    }
	  return;
	case CAF_ARR_REF_RANGE:
	  COMPUTE_NUM_ITEMS (extent_dst,
			     ref->u.a.dim[dst_dim].s.stride,
			     ref->u.a.dim[dst_dim].s.start,
			     ref->u.a.dim[dst_dim].s.end);
	  array_offset_dst = ref->u.a.dim[dst_dim].s.start;
	  src_index[src_dim] = 0;
	  for (index_type idx = 0; idx < extent_dst; ++idx)
	    {
	      send_by_ref (ref, i, src_index, single_token, NULL, src,
			   ds + array_offset_dst * ref->item_size, sr,
			   dst_kind, src_kind, dst_dim + 1, src_dim + 1,
			   1, size, stat, dst_type);
	      if (src_rank > 0)
		src_index[src_dim]
		    += GFC_DIMENSION_STRIDE (src->dim[src_dim]);
	      array_offset_dst += ref->u.a.dim[dst_dim].s.stride;
	    }
	  return;
	case CAF_ARR_REF_SINGLE:
	  array_offset_dst = ref->u.a.dim[dst_dim].s.start;
	  send_by_ref (ref, i, src_index, single_token, NULL, src,
		       ds + array_offset_dst * ref->item_size, sr,
		       dst_kind, src_kind, dst_dim + 1, src_dim, 1,
		       size, stat, dst_type);
	  return;
	/* The OPEN_* are mapped to a RANGE and therefore cannot occur.  */
	case CAF_ARR_REF_OPEN_END:
	case CAF_ARR_REF_OPEN_START:
	default:
	  caf_runtime_error (unreachable);
	}
      return;
    default:
      caf_runtime_error (unreachable);
    }
}


void
_gfortran_caf_send_by_ref (caf_token_t token,
			   int image_index __attribute__ ((unused)),
			   gfc_descriptor_t *src, caf_reference_t *refs,
			   int dst_kind, int src_kind,
			   bool may_require_tmp __attribute__ ((unused)),
			   bool dst_reallocatable, int *stat, int dst_type)
{
  const char vecrefunknownkind[] = "libcaf_single::caf_get_by_ref(): "
				   "unknown kind in vector-ref.\n";
  const char unknownreftype[] = "libcaf_single::caf_send_by_ref(): "
				"unknown reference type.\n";
  const char unknownarrreftype[] = "libcaf_single::caf_send_by_ref(): "
				   "unknown array reference type.\n";
  const char rankoutofrange[] = "libcaf_single::caf_send_by_ref(): "
				"rank out of range.\n";
  const char realloconinnerref[] = "libcaf_single::caf_send_by_ref(): "
      "reallocation of array followed by component ref not allowed.\n";
  const char cannotallocdst[] = "libcaf_single::caf_send_by_ref(): "
				"cannot allocate memory.\n";
  const char nonallocextentmismatch[] = "libcaf_single::caf_send_by_ref(): "
      "extent of non-allocatable array mismatch.\n";
  const char innercompref[] = "libcaf_single::caf_send_by_ref(): "
      "inner unallocated component detected.\n";
  size_t size, i;
  size_t dst_index[GFC_MAX_DIMENSIONS];
  int src_rank = GFC_DESCRIPTOR_RANK (src);
  int src_cur_dim = 0;
  size_t src_size = 0;
  caf_single_token_t single_token = TOKEN (token);
  void *memptr = single_token->memptr;
  gfc_descriptor_t *dst = single_token->desc;
  caf_reference_t *riter = refs;
  long delta;
  bool extent_mismatch;
  /* Note that the component is not allocated yet.  */
  index_type new_component_idx = -1;

  if (stat)
    *stat = 0;

  /* Compute the size of the result.  In the beginning size just counts the
     number of elements.  */
  size = 1;
  while (riter)
    {
      switch (riter->type)
	{
	case CAF_REF_COMPONENT:
	  if (unlikely (new_component_idx != -1))
	    {
	      /* Allocating a component in the middle of a component ref is not
		 support.  We don't know the type to allocate.  */
	      caf_internal_error (innercompref, stat, NULL, 0);
	      return;
	    }
	  if (riter->u.c.caf_token_offset > 0)
	    {
	      /* Check whether the allocatable component is zero, then no
		 token is present, too.  The token's pointer is not cleared
		 when the structure is initialized.  */
	      if (*(void**)(memptr + riter->u.c.offset) == NULL)
		{
		  /* This component is not yet allocated.  Check that it is
		     allocatable here.  */
		  if (!dst_reallocatable)
		    {
		      caf_internal_error (cannotallocdst, stat, NULL, 0);
		      return;
		    }
		  single_token = NULL;
		  memptr = NULL;
		  dst = NULL;
		  break;
		}
	      single_token = *(caf_single_token_t*)
					 (memptr + riter->u.c.caf_token_offset);
	      memptr += riter->u.c.offset;
	      dst = single_token->desc;
	    }
	  else
	    {
	      /* Regular component.  */
	      memptr += riter->u.c.offset;
	      dst = (gfc_descriptor_t *)memptr;
	    }
	  break;
	case CAF_REF_ARRAY:
	  if (dst != NULL)
	    memptr = GFC_DESCRIPTOR_DATA (dst);
	  else
	    dst = src;
	  /* When the dst array needs to be allocated, then look at the
	     extent of the source array in the dimension dst_cur_dim.  */
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_VECTOR:
		  delta = riter->u.a.dim[i].v.nvec;
#define KINDCASE(kind, type) case kind: \
		    memptr += (((index_type) \
			((type *)riter->u.a.dim[i].v.vector)[0]) \
			- GFC_DIMENSION_LBOUND (dst->dim[i])) \
			* GFC_DIMENSION_STRIDE (dst->dim[i]) \
			* riter->item_size; \
		    break

		  switch (riter->u.a.dim[i].v.kind)
		    {
		    KINDCASE (1, GFC_INTEGER_1);
		    KINDCASE (2, GFC_INTEGER_2);
		    KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		    KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		    KINDCASE (16, GFC_INTEGER_16);
#endif
		    default:
		      caf_internal_error (vecrefunknownkind, stat, NULL, 0);
		      return;
		    }
#undef KINDCASE
		  break;
		case CAF_ARR_REF_FULL:
		  if (dst)
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				       GFC_DIMENSION_LBOUND (dst->dim[i]),
				       GFC_DIMENSION_UBOUND (dst->dim[i]));
		  else
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				   GFC_DIMENSION_LBOUND (src->dim[src_cur_dim]),
				  GFC_DIMENSION_UBOUND (src->dim[src_cur_dim]));
		  break;
		case CAF_ARR_REF_RANGE:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     riter->u.a.dim[i].s.start,
				     riter->u.a.dim[i].s.end);
		  memptr += (riter->u.a.dim[i].s.start
			     - dst->dim[i].lower_bound)
		      * GFC_DIMENSION_STRIDE (dst->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_SINGLE:
		  delta = 1;
		  memptr += (riter->u.a.dim[i].s.start
			     - dst->dim[i].lower_bound)
		      * GFC_DIMENSION_STRIDE (dst->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_END:
		  if (dst)
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				       riter->u.a.dim[i].s.start,
				       GFC_DIMENSION_UBOUND (dst->dim[i]));
		  else
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				       riter->u.a.dim[i].s.start,
				  GFC_DIMENSION_UBOUND (src->dim[src_cur_dim]));
		  memptr += (riter->u.a.dim[i].s.start
			     - dst->dim[i].lower_bound)
		      * GFC_DIMENSION_STRIDE (dst->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_START:
		  if (dst)
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				       GFC_DIMENSION_LBOUND (dst->dim[i]),
				       riter->u.a.dim[i].s.end);
		  else
		    COMPUTE_NUM_ITEMS (delta,
				       riter->u.a.dim[i].s.stride,
				   GFC_DIMENSION_LBOUND (src->dim[src_cur_dim]),
				       riter->u.a.dim[i].s.end);
		  /* The memptr stays unchanged when ref'ing the first element
		     in a dimension.  */
		  break;
		default:
		  caf_internal_error (unknownarrreftype, stat, NULL, 0);
		  return;
		}

	      if (delta <= 0)
		return;
	      /* Check the various properties of the source array.
		 When src is an array.  */
	      if (delta > 1 && src_rank > 0)
		{
		  /* Check that src_cur_dim is valid for src.  Can be
		     superceeded only by scalar data.  */
		  if (src_cur_dim >= src_rank)
		    {
		      caf_internal_error (rankoutofrange, stat, NULL, 0);
		      return;
		    }
		  /* Do further checks, when the source is not scalar.  */
		  else
		    {
		      /* When the realloc is required, then no extent may have
			 been set.  */
		      extent_mismatch = memptr == NULL
			  || (dst
			      && GFC_DESCRIPTOR_EXTENT (dst, src_cur_dim)
			      != delta);
		      /* When it already known, that a realloc is needed or
			 the extent does not match the needed one.  */
		      if (extent_mismatch)
			{
			  /* Check whether dst is reallocatable.  */
			  if (unlikely (!dst_reallocatable))
			    {
			      caf_internal_error (nonallocextentmismatch, stat,
						  NULL, 0, delta,
						  GFC_DESCRIPTOR_EXTENT (dst,
								  src_cur_dim));
			      return;
			    }
			  /* Report error on allocatable but missing inner
			     ref.  */
			  else if (riter->next != NULL)
			    {
			      caf_internal_error (realloconinnerref, stat, NULL,
						  0);
			      return;
			    }
			}
		      /* Only change the extent when it does not match.  This is
			 to prevent resetting given array bounds.  */
		      if (extent_mismatch)
			GFC_DIMENSION_SET (dst->dim[src_cur_dim], 1, delta,
					   size);
		    }
		  /* Increase the dim-counter of the src only when the extent
		     matches.  */
		  if (src_cur_dim < src_rank
		      && GFC_DESCRIPTOR_EXTENT (src, src_cur_dim) == delta)
		    ++src_cur_dim;
		}
	      size *= (index_type)delta;
	    }
	  break;
	case CAF_REF_STATIC_ARRAY:
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_VECTOR:
		  delta = riter->u.a.dim[i].v.nvec;
#define KINDCASE(kind, type) case kind: \
		    memptr += ((type *)riter->u.a.dim[i].v.vector)[0] \
			* riter->item_size; \
		    break

		  switch (riter->u.a.dim[i].v.kind)
		    {
		    KINDCASE (1, GFC_INTEGER_1);
		    KINDCASE (2, GFC_INTEGER_2);
		    KINDCASE (4, GFC_INTEGER_4);
#ifdef HAVE_GFC_INTEGER_8
		    KINDCASE (8, GFC_INTEGER_8);
#endif
#ifdef HAVE_GFC_INTEGER_16
		    KINDCASE (16, GFC_INTEGER_16);
#endif
		    default:
		      caf_internal_error (vecrefunknownkind, stat, NULL, 0);
		      return;
		    }
#undef KINDCASE
		  break;
		case CAF_ARR_REF_FULL:
		  delta = riter->u.a.dim[i].s.end / riter->u.a.dim[i].s.stride
		      + 1;
		  /* The memptr stays unchanged when ref'ing the first element
		     in a dimension.  */
		  break;
		case CAF_ARR_REF_RANGE:
		  COMPUTE_NUM_ITEMS (delta,
				     riter->u.a.dim[i].s.stride,
				     riter->u.a.dim[i].s.start,
				     riter->u.a.dim[i].s.end);
		  memptr += riter->u.a.dim[i].s.start
		      * riter->u.a.dim[i].s.stride
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_SINGLE:
		  delta = 1;
		  memptr += riter->u.a.dim[i].s.start
		      * riter->u.a.dim[i].s.stride
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_OPEN_END:
		  /* This and OPEN_START are mapped to a RANGE and therefore
		     cannot occur here.  */
		case CAF_ARR_REF_OPEN_START:
		default:
		  caf_internal_error (unknownarrreftype, stat, NULL, 0);
		  return;
		}
	      if (delta <= 0)
		return;
	      /* Check the various properties of the source array.
		 Only when the source array is not scalar examine its
		 properties.  */
	      if (delta > 1 && src_rank > 0)
		{
		  /* Check that src_cur_dim is valid for src.  Can be
		     superceeded only by scalar data.  */
		  if (src_cur_dim >= src_rank)
		    {
		      caf_internal_error (rankoutofrange, stat, NULL, 0);
		      return;
		    }
		  else
		    {
		      /* We will not be able to realloc the dst, because that's
			 a fixed size array.  */
		      extent_mismatch = GFC_DESCRIPTOR_EXTENT (src, src_cur_dim)
			      != delta;
		      /* When the extent does not match the needed one we can
			 only stop here.  */
		      if (extent_mismatch)
			{
			  caf_internal_error (nonallocextentmismatch, stat,
					      NULL, 0, delta,
					      GFC_DESCRIPTOR_EXTENT (src,
								  src_cur_dim));
			  return;
			}
		    }
		  ++src_cur_dim;
		}
	      size *= (index_type)delta;
	    }
	  break;
	default:
	  caf_internal_error (unknownreftype, stat, NULL, 0);
	  return;
	}
      src_size = riter->item_size;
      riter = riter->next;
    }
  if (size == 0 || src_size == 0)
    return;
  /* Postcondition:
     - size contains the number of elements to store in the destination array,
     - src_size gives the size in bytes of each item in the destination array.
  */

  /* Reset the token.  */
  single_token = TOKEN (token);
  memptr = single_token->memptr;
  dst = single_token->desc;
  memset (dst_index, 0, sizeof (dst_index));
  i = 0;
  send_by_ref (refs, &i, dst_index, single_token, dst, src,
	       memptr, GFC_DESCRIPTOR_DATA (src), dst_kind, src_kind, 0, 0,
	       1, size, stat, dst_type);
  assert (i == size);
}


void
_gfortran_caf_sendget_by_ref (caf_token_t dst_token, int dst_image_index,
			      caf_reference_t *dst_refs, caf_token_t src_token,
			      int src_image_index,
			      caf_reference_t *src_refs, int dst_kind,
			      int src_kind, bool may_require_tmp, int *dst_stat,
			      int *src_stat, int dst_type, int src_type)
{
  GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) temp;
  GFC_DESCRIPTOR_DATA (&temp) = NULL;
  GFC_DESCRIPTOR_RANK (&temp) = -1;
  GFC_DESCRIPTOR_TYPE (&temp) = dst_type;

  _gfortran_caf_get_by_ref (src_token, src_image_index,
			    (gfc_descriptor_t *) &temp, src_refs,
			    dst_kind, src_kind, may_require_tmp, true,
			    src_stat, src_type);

  if (src_stat && *src_stat != 0)
    return;

  _gfortran_caf_send_by_ref (dst_token, dst_image_index,
			     (gfc_descriptor_t *) &temp, dst_refs,
			     dst_kind, dst_kind, may_require_tmp, true,
			     dst_stat, dst_type);
  if (GFC_DESCRIPTOR_DATA (&temp))
    free (GFC_DESCRIPTOR_DATA (&temp));
}


void
_gfortran_caf_atomic_define (caf_token_t token, size_t offset,
			     int image_index __attribute__ ((unused)),
			     void *value, int *stat,
			     int type __attribute__ ((unused)), int kind)
{
  assert(kind == 4);

  uint32_t *atom = (uint32_t *) ((char *) MEMTOK (token) + offset);

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

  uint32_t *atom = (uint32_t *) ((char *) MEMTOK (token) + offset);

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

  uint32_t *atom = (uint32_t *) ((char *) MEMTOK (token) + offset);

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
  uint32_t *atom = (uint32_t *) ((char *) MEMTOK (token) + offset);

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
			  size_t errmsg_len __attribute__ ((unused)))
{
  uint32_t value = 1;
  uint32_t *event = (uint32_t *) ((char *) MEMTOK (token) + index
				  * sizeof (uint32_t));
  __atomic_fetch_add (event, (uint32_t) value, __ATOMIC_RELAXED);
  
  if(stat)
    *stat = 0;
}

void
_gfortran_caf_event_wait (caf_token_t token, size_t index, 
			  int until_count, int *stat,
			  char *errmsg __attribute__ ((unused)), 
			  size_t errmsg_len __attribute__ ((unused)))
{
  uint32_t *event = (uint32_t *) ((char *) MEMTOK (token) + index
				  * sizeof (uint32_t));
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
  uint32_t *event = (uint32_t *) ((char *) MEMTOK (token) + index
				  * sizeof (uint32_t));
  __atomic_load (event, (uint32_t *) count, __ATOMIC_RELAXED);
  
  if(stat)
    *stat = 0;
}

void
_gfortran_caf_lock (caf_token_t token, size_t index,
		    int image_index __attribute__ ((unused)),
		    int *acquired_lock, int *stat, char *errmsg,
		    size_t errmsg_len)
{
  const char *msg = "Already locked";
  bool *lock = &((bool *) MEMTOK (token))[index];

  if (!*lock)
    {
      *lock = true;
      if (acquired_lock)
	*acquired_lock = (int) true;
      if (stat)
	*stat = 0;
      return;
    }

  if (acquired_lock)
    {
      *acquired_lock = (int) false;
      if (stat)
	*stat = 0;
    return;
    }


  if (stat)
    {
      *stat = 1;
      if (errmsg_len > 0)
	{
	  size_t len = (sizeof (msg) > errmsg_len) ? errmsg_len
						      : sizeof (msg);
	  memcpy (errmsg, msg, len);
	  if (errmsg_len > len)
	    memset (&errmsg[len], ' ', errmsg_len-len);
	}
      return;
    }
  _gfortran_caf_error_stop_str (msg, strlen (msg), false);
}


void
_gfortran_caf_unlock (caf_token_t token, size_t index,
		      int image_index __attribute__ ((unused)),
		      int *stat, char *errmsg, size_t errmsg_len)
{
  const char *msg = "Variable is not locked";
  bool *lock = &((bool *) MEMTOK (token))[index];

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
	  size_t len = (sizeof (msg) > errmsg_len) ? errmsg_len
	    : sizeof (msg);
	  memcpy (errmsg, msg, len);
	  if (errmsg_len > len)
	    memset (&errmsg[len], ' ', errmsg_len-len);
	}
      return;
    }
  _gfortran_caf_error_stop_str (msg, strlen (msg), false);
}

int
_gfortran_caf_is_present (caf_token_t token,
			  int image_index __attribute__ ((unused)),
			  caf_reference_t *refs)
{
  const char arraddressingnotallowed[] = "libcaf_single::caf_is_present(): "
				   "only scalar indexes allowed.\n";
  const char unknownreftype[] = "libcaf_single::caf_get_by_ref(): "
				"unknown reference type.\n";
  const char unknownarrreftype[] = "libcaf_single::caf_get_by_ref(): "
				   "unknown array reference type.\n";
  size_t i;
  caf_single_token_t single_token = TOKEN (token);
  void *memptr = single_token->memptr;
  gfc_descriptor_t *src = single_token->desc;
  caf_reference_t *riter = refs;

  while (riter)
    {
      switch (riter->type)
	{
	case CAF_REF_COMPONENT:
	  if (riter->u.c.caf_token_offset)
	    {
	      single_token = *(caf_single_token_t*)
					 (memptr + riter->u.c.caf_token_offset);
	      memptr = single_token->memptr;
	      src = single_token->desc;
	    }
	  else
	    {
	      memptr += riter->u.c.offset;
	      src = (gfc_descriptor_t *)memptr;
	    }
	  break;
	case CAF_REF_ARRAY:
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_SINGLE:
		  memptr += (riter->u.a.dim[i].s.start
			     - GFC_DIMENSION_LBOUND (src->dim[i]))
		      * GFC_DIMENSION_STRIDE (src->dim[i])
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_FULL:
		  /* A full array ref is allowed on the last reference only.  */
		  if (riter->next == NULL)
		    break;
		  /* else fall through reporting an error.  */
		  /* FALLTHROUGH */
		case CAF_ARR_REF_VECTOR:
		case CAF_ARR_REF_RANGE:
		case CAF_ARR_REF_OPEN_END:
		case CAF_ARR_REF_OPEN_START:
		  caf_internal_error (arraddressingnotallowed, 0, NULL, 0);
		  return 0;
		default:
		  caf_internal_error (unknownarrreftype, 0, NULL, 0);
		  return 0;
		}
	    }
	  break;
	case CAF_REF_STATIC_ARRAY:
	  for (i = 0; riter->u.a.mode[i] != CAF_ARR_REF_NONE; ++i)
	    {
	      switch (riter->u.a.mode[i])
		{
		case CAF_ARR_REF_SINGLE:
		  memptr += riter->u.a.dim[i].s.start
		      * riter->u.a.dim[i].s.stride
		      * riter->item_size;
		  break;
		case CAF_ARR_REF_FULL:
		  /* A full array ref is allowed on the last reference only.  */
		  if (riter->next == NULL)
		    break;
		  /* else fall through reporting an error.  */
		  /* FALLTHROUGH */
		case CAF_ARR_REF_VECTOR:
		case CAF_ARR_REF_RANGE:
		case CAF_ARR_REF_OPEN_END:
		case CAF_ARR_REF_OPEN_START:
		  caf_internal_error (arraddressingnotallowed, 0, NULL, 0);
		  return 0;
		default:
		  caf_internal_error (unknownarrreftype, 0, NULL, 0);
		  return 0;
		}
	    }
	  break;
	default:
	  caf_internal_error (unknownreftype, 0, NULL, 0);
	  return 0;
	}
      riter = riter->next;
    }
  return memptr != NULL;
}

/* Reference the libraries implementation.  */
extern void _gfortran_random_init (int32_t, int32_t, int32_t);

void _gfortran_caf_random_init (bool repeatable, bool image_distinct)
{
  /* In a single image implementation always forward to the gfortran
     routine.  */
  _gfortran_random_init (repeatable, image_distinct, 1);
}
