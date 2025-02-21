/* Single-image implementation of GNU Fortran Coarray Library
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
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

typedef void (*getter_t) (void *, const int *, void **, int32_t *, void *,
			  caf_token_t, const size_t, size_t *, const size_t *);
typedef void (*is_present_t) (void *, const int *, int32_t *, void *,
			      caf_single_token_t, const size_t);
typedef void (*receiver_t) (void *, const int *, void *, const void *,
			    caf_token_t, const size_t, const size_t *,
			    const size_t *);
struct accessor_hash_t
{
  int hash;
  int pad;
  union
  {
    getter_t getter;
    is_present_t is_present;
    receiver_t receiver;
  } u;
};

static struct accessor_hash_t *accessor_hash_table = NULL;
static int aht_cap = 0;
static int aht_size = 0;
static enum {
  AHT_UNINITIALIZED,
  AHT_OPEN,
  AHT_PREPARED
} accessor_hash_table_state
  = AHT_UNINITIALIZED;

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
  free (accessor_hash_table);

  while (caf_static_list != NULL)
    {
      caf_static_t *tmp = caf_static_list->prev;
      free (((caf_single_token_t) caf_static_list->token)->memptr);
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
      free (local);
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

extern void _gfortran_report_exception (void);

void
_gfortran_caf_stop_numeric(int stop_code, bool quiet)
{
  if (!quiet)
    {
      _gfortran_report_exception ();
      fprintf (stderr, "STOP %d\n", stop_code);
    }
  exit (stop_code);
}


void
_gfortran_caf_stop_str(const char *string, size_t len, bool quiet)
{
  if (!quiet)
    {
      _gfortran_report_exception ();
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
      _gfortran_report_exception ();
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
    {
      _gfortran_report_exception ();
      fprintf (stderr, "ERROR STOP %d\n", error);
    }
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


void
_gfortran_caf_register_accessor (const int hash, getter_t accessor)
{
  if (accessor_hash_table_state == AHT_UNINITIALIZED)
    {
      aht_cap = 16;
      accessor_hash_table = calloc (aht_cap, sizeof (struct accessor_hash_t));
      accessor_hash_table_state = AHT_OPEN;
    }
  if (aht_size == aht_cap)
    {
      aht_cap += 16;
      accessor_hash_table = realloc (accessor_hash_table,
				     aht_cap * sizeof (struct accessor_hash_t));
    }
  if (accessor_hash_table_state == AHT_PREPARED)
    {
      accessor_hash_table_state = AHT_OPEN;
    }
  accessor_hash_table[aht_size].hash = hash;
  accessor_hash_table[aht_size].u.getter = accessor;
  ++aht_size;
}

static int
hash_compare (const struct accessor_hash_t *lhs,
	      const struct accessor_hash_t *rhs)
{
  return lhs->hash < rhs->hash ? -1 : (lhs->hash > rhs->hash ? 1 : 0);
}

void
_gfortran_caf_register_accessors_finish (void)
{
  if (accessor_hash_table_state == AHT_PREPARED
      || accessor_hash_table_state == AHT_UNINITIALIZED)
    return;

  qsort (accessor_hash_table, aht_size, sizeof (struct accessor_hash_t),
	 (int (*) (const void *, const void *)) hash_compare);
  accessor_hash_table_state = AHT_PREPARED;
}

int
_gfortran_caf_get_remote_function_index (const int hash)
{
  if (accessor_hash_table_state != AHT_PREPARED)
    {
      caf_runtime_error ("the accessor hash table is not prepared.");
    }

  struct accessor_hash_t cand;
  cand.hash = hash;
  struct accessor_hash_t *f
    = bsearch (&cand, accessor_hash_table, aht_size,
	       sizeof (struct accessor_hash_t),
	       (int (*) (const void *, const void *)) hash_compare);

  int index = f ? f - accessor_hash_table : -1;
  return index;
}

void
_gfortran_caf_get_from_remote (
  caf_token_t token, const gfc_descriptor_t *opt_src_desc,
  const size_t *opt_src_charlen, const int image_index,
  const size_t dst_size __attribute__ ((unused)), void **dst_data,
  size_t *opt_dst_charlen, gfc_descriptor_t *opt_dst_desc,
  const bool may_realloc_dst, const int getter_index, void *add_data,
  const size_t add_data_size __attribute__ ((unused)), int *stat,
  caf_team_t *team __attribute__ ((unused)),
  int *team_number __attribute__ ((unused)))
{
  caf_single_token_t single_token = TOKEN (token);
  void *src_ptr = opt_src_desc ? (void *) opt_src_desc : single_token->memptr;
  int32_t free_buffer;
  void *dst_ptr = opt_dst_desc ? (void *)opt_dst_desc : dst_data;
  void *old_dst_data_ptr = NULL;
  struct caf_single_token cb_token;
  cb_token.memptr = add_data;
  cb_token.desc = NULL;
  cb_token.owning_memory = false;

  if (stat)
    *stat = 0;

  if (opt_dst_desc && !may_realloc_dst)
    {
      old_dst_data_ptr = opt_dst_desc->base_addr;
      opt_dst_desc->base_addr = NULL;
    }

  accessor_hash_table[getter_index].u.getter (add_data, &image_index, dst_ptr,
					      &free_buffer, src_ptr, &cb_token,
					      0, opt_dst_charlen,
					      opt_src_charlen);
  if (opt_dst_desc && old_dst_data_ptr && !may_realloc_dst
      && opt_dst_desc->base_addr != old_dst_data_ptr)
    {
      size_t dsize = opt_dst_desc->span;
      for (int i = 0; i < GFC_DESCRIPTOR_RANK (opt_dst_desc); ++i)
	dsize *= GFC_DESCRIPTOR_EXTENT (opt_dst_desc, i);
      memcpy (old_dst_data_ptr, opt_dst_desc->base_addr, dsize);
      free (opt_dst_desc->base_addr);
      opt_dst_desc->base_addr = old_dst_data_ptr;
    }
}

int32_t
_gfortran_caf_is_present_on_remote (caf_token_t token, const int image_index,
				    const int present_index, void *add_data,
				    const size_t add_data_size
				    __attribute__ ((unused)))
{
  /* Unregistered tokens are always not present.  */
  if (!token)
    return 0;

  caf_single_token_t single_token = TOKEN (token);
  int32_t result;
  struct caf_single_token cb_token = {add_data, NULL, false};


  accessor_hash_table[present_index].u.is_present (add_data, &image_index,
						   &result,
						   single_token->memptr,
						   &cb_token, 0);

  return result;
}

void
_gfortran_caf_send_to_remote (
  caf_token_t token, gfc_descriptor_t *opt_dst_desc,
  const size_t *opt_dst_charlen, const int image_index,
  const size_t src_size __attribute__ ((unused)), const void *src_data,
  const size_t *opt_src_charlen, const gfc_descriptor_t *opt_src_desc,
  const int accessor_index, void *add_data,
  const size_t add_data_size __attribute__ ((unused)), int *stat,
  caf_team_t *team __attribute__ ((unused)),
  int *team_number __attribute__ ((unused)))
{
  caf_single_token_t single_token = TOKEN (token);
  void *dst_ptr = opt_dst_desc ? (void *) opt_dst_desc : single_token->memptr;
  const void *src_ptr = opt_src_desc ? (void *) opt_src_desc : src_data;
  struct caf_single_token cb_token;
  cb_token.memptr = add_data;
  cb_token.desc = NULL;
  cb_token.owning_memory = false;

  if (stat)
    *stat = 0;

  accessor_hash_table[accessor_index].u.receiver (add_data, &image_index,
						  dst_ptr, src_ptr, &cb_token,
						  0, opt_dst_charlen,
						  opt_src_charlen);
}

void
_gfortran_caf_transfer_between_remotes (
  caf_token_t dst_token, gfc_descriptor_t *opt_dst_desc,
  size_t *opt_dst_charlen, const int dst_image_index,
  const int dst_access_index, void *dst_add_data,
  const size_t dst_add_data_size __attribute__ ((unused)),
  caf_token_t src_token, const gfc_descriptor_t *opt_src_desc,
  const size_t *opt_src_charlen, const int src_image_index,
  const int src_access_index, void *src_add_data,
  const size_t src_add_data_size __attribute__ ((unused)),
  const size_t src_size, const bool scalar_transfer, int *dst_stat,
  int *src_stat, caf_team_t *dst_team __attribute__ ((unused)),
  int *dst_team_number __attribute__ ((unused)),
  caf_team_t *src_team __attribute__ ((unused)),
  int *src_team_number __attribute__ ((unused)))
{
  caf_single_token_t src_single_token = TOKEN (src_token),
		     dst_single_token = TOKEN (dst_token);
  void *src_ptr
    = opt_src_desc ? (void *) opt_src_desc : src_single_token->memptr;
  int32_t free_buffer;
  void *dst_ptr
    = opt_dst_desc ? (void *) opt_dst_desc : dst_single_token->memptr;
  void *transfer_ptr, *buffer;
  GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) *transfer_desc = NULL;
  struct caf_single_token cb_token;
  cb_token.memptr = src_add_data;
  cb_token.desc = NULL;
  cb_token.owning_memory = false;

  if (src_stat)
    *src_stat = 0;

  if (!scalar_transfer)
    {
      const size_t desc_size = sizeof (*transfer_desc);
      transfer_desc = __builtin_alloca (desc_size);
      memset (transfer_desc, 0, desc_size);
      transfer_ptr = transfer_desc;
    }
  else if (opt_dst_charlen)
    transfer_ptr = __builtin_alloca (*opt_dst_charlen * src_size);
  else
    {
      buffer = NULL;
      transfer_ptr = &buffer;
    }

  accessor_hash_table[src_access_index].u.getter (
    src_add_data, &src_image_index, transfer_ptr, &free_buffer, src_ptr,
    &cb_token, 0, opt_dst_charlen, opt_src_charlen);

  if (dst_stat)
    *dst_stat = 0;

  if (scalar_transfer)
    transfer_ptr = *(void **) transfer_ptr;

  cb_token.memptr = dst_add_data;
  accessor_hash_table[dst_access_index].u.receiver (dst_add_data,
						    &dst_image_index, dst_ptr,
						    transfer_ptr, &cb_token, 0,
						    opt_dst_charlen,
						    opt_src_charlen);

  if (free_buffer)
    free (transfer_desc ? transfer_desc->base_addr : transfer_ptr);
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


/* Reference the libraries implementation.  */
extern void _gfortran_random_init (int32_t, int32_t, int32_t);

void _gfortran_caf_random_init (bool repeatable, bool image_distinct)
{
  /* In a single image implementation always forward to the gfortran
     routine.  */
  _gfortran_random_init (repeatable, image_distinct, 1);
}
