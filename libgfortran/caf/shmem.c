/* Shared memory-multiple (process)-image implementation of GNU Fortran
   Coarray Library
   Copyright (C) 2011-2025 Free Software Foundation, Inc.
   Based on single.c contributed by Tobias Burnus <burnus@net-b.de>

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
#include "caf_error.h"

#include "shmem/counter_barrier.h"
#include "shmem/supervisor.h"
#include "shmem/teams_mgmt.h"
#include "shmem/thread_support.h"

#include <stdlib.h> /* For exit and malloc.  */
#include <string.h> /* For memcpy and memset.  */
#include <stdint.h>
#include <assert.h>
#include <errno.h>
#include <unistd.h>

/* Define GFC_CAF_CHECK to enable run-time checking.  */
/* #define GFC_CAF_CHECK  1  */

#define TOKEN(X) ((caf_shmem_token_t) (X))
#define MEMTOK(X) ((caf_shmem_token_t) (X))->memptr

/* Global variables.  */
static caf_static_t *caf_static_list = NULL;
memid next_memid = 0;

typedef void (*getter_t) (void *, const int *, void **, int32_t *, void *,
			  caf_token_t, const size_t, size_t *, const size_t *);
typedef void (*is_present_t) (void *, const int *, int32_t *, void *,
			      caf_shmem_token_t, const size_t);
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

void
_gfortran_caf_init (int *argc, char ***argv)
{
  int exit_code = 0;

  ensure_shmem_initialization ();

  if (shared_memory_get_env ())
    {
      /* This is the initialization of a worker.  */
      _gfortran_caf_sync_all (NULL, NULL, 0);
      return;
    }

  if (supervisor_main_loop (argc, argv, &exit_code))
    return;
  shared_memory_cleanup (&local->sm);

  /* Free pseudo tokens and memory to allow main process to survive caf_init.
   */
  while (caf_static_list != NULL)
    {
      caf_static_t *tmp = caf_static_list->prev;
      free (((caf_shmem_token_t) caf_static_list->token)->base);
      free (caf_static_list->token);
      free (caf_static_list);
      caf_static_list = tmp;
    }
  free (local);
  exit (exit_code);
}

static void
free_team_list (caf_shmem_team_t l)
{
  while (l != NULL)
    {
      caf_shmem_team_t p = l->parent;
      struct coarray_allocated *ca = l->allocated;
      while (ca)
	{
	  struct coarray_allocated *nca = ca->next;
	  free (ca);
	  ca = nca;
	}
      free (l);
      l = p;
    }
}

void
_gfortran_caf_finalize (void)
{
  free (accessor_hash_table);

  while (caf_static_list != NULL)
    {
      caf_static_t *tmp = caf_static_list->prev;
      alloc_free_memory_with_id (
	&local->ai,
	(memid) ((caf_shmem_token_t) caf_static_list->token)->token_id);
      free (caf_static_list->token);
      free (caf_static_list);
      caf_static_list = tmp;
    }

  free_team_list (caf_current_team);
  caf_initial_team = caf_current_team = NULL;
  free_team_list (caf_teams_formed);
  caf_teams_formed = NULL;

  free (local);
}

int
_gfortran_caf_this_image (caf_team_t team)
{
  return (team ? ((caf_shmem_team_t) team)->index : caf_current_team->index)
	 + 1;
}

int
_gfortran_caf_num_images (caf_team_t team, int32_t *team_number)
{
#define CHECK_TEAMS                                                            \
  while (cur)                                                                  \
    {                                                                          \
      if (cur->u.image_info->team_id == *team_number)                          \
	return counter_barrier_get_count (&cur->u.image_info->image_count);    \
      cur = cur->parent;                                                       \
    }

  if (team)
    return counter_barrier_get_count (
      &((caf_shmem_team_t) team)->u.image_info->image_count);

  if (team_number)
    {
      caf_shmem_team_t cur = caf_current_team;

      CHECK_TEAMS

      cur = caf_teams_formed;
      CHECK_TEAMS
    }

  return counter_barrier_get_count (
    &caf_current_team->u.image_info->image_count);
}


void
_gfortran_caf_register (size_t size, caf_register_t type, caf_token_t *token,
			gfc_descriptor_t *data, int *stat, char *errmsg,
			size_t errmsg_len)
{
  static bool inited = false;
  const char alloc_fail_msg[] = "Failed to allocate coarray";
  void *mem;
  caf_shmem_token_t shmem_token;

  /* When the master has not been initialized, we could either be in the
     control process or in the static initializer phase.  */
  if (unlikely (!inited))
    {
      if (local == NULL)
	{
	  if (shared_memory_get_env ())
	    {
	      /* This is the static initializer phase.  Register the static
		 coarrays or we are in trouble later.  */
	      ensure_shmem_initialization ();
	      inited = true;
	    }
	  else if (type == CAF_REGTYPE_COARRAY_STATIC)
	    {
	      /* This is the control process, but it also runs the static
		 initializers (the caf_init.N() procedures).  In these it may
		 want to assign to members (effectively NULL them) of derived
		 types.  Therefore the need to return valid memory blocks.
		 These are never used and do not participate in any coarray
		 routine.  They unfortunately just waste some memory.  */
	      mem = malloc (size);
	      GFC_DESCRIPTOR_DATA (data) = mem;
	      caf_static_t *tmp = malloc (sizeof (caf_static_t));
	      *token = malloc (sizeof (struct caf_shmem_token));
	      **(caf_shmem_token_t *) token
		= (struct caf_shmem_token) {mem, NULL, mem, size, ~0U, true};
	      *tmp = (caf_static_t) {*token, caf_static_list};
	      caf_static_list = tmp;
	      return;
	    }
	  else
	    return;
	}
    }

  /* Catch all special cases.  */
  switch (type)
    {
    /* When mapping, read from the old token.  */
    case CAF_REGTYPE_COARRAY_MAP_EXISTING:
      /* The mapping could involve an offset that is mangled into the array's
	 data ptr.  */
      mem
	= ((caf_shmem_token_t) *token)->base
	  + (GFC_DESCRIPTOR_DATA (data) - ((caf_shmem_token_t) *token)->memptr);
      size = ((caf_shmem_token_t) *token)->image_size;
      break;
    case CAF_REGTYPE_EVENT_ALLOC:
    case CAF_REGTYPE_EVENT_STATIC:
      size *= sizeof (void *);
      break;
    default:
      break;
    }

  if (type != CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY)
    *token = malloc (sizeof (struct caf_shmem_token));

  size = alignto (size, sizeof (ptrdiff_t));
  switch (type)
    {
    case CAF_REGTYPE_LOCK_STATIC:
    case CAF_REGTYPE_LOCK_ALLOC:
    case CAF_REGTYPE_CRITICAL:
      {
	lock_t *addr;
	bool created;

	allocator_lock (&local->ai.alloc);
	/* Allocate enough space for the metadata infront of the lock
	   array.  */
	addr
	  = alloc_get_memory_by_id_created (&local->ai, size * sizeof (lock_t),
					    next_memid, &created);

	if (created)
	  {
	    /* Initialize the mutex only, when the memory was allocated for the
	       first time.  */
	    for (size_t c = 0; c < size; ++c)
	      initialize_shared_errorcheck_mutex (&addr[c]);
	  }
	size *= sizeof (lock_t);

	allocator_unlock (&local->ai.alloc);
	mem = addr;
	break;
      }
    case CAF_REGTYPE_EVENT_STATIC:
    case CAF_REGTYPE_EVENT_ALLOC:
      {
	bool created;

	allocator_lock (&local->ai.alloc);
	mem = alloc_get_memory_by_id_created (
	  &local->ai, size * caf_current_team->u.image_info->image_count.count,
	  next_memid, &created);
	if (created)
	  memset (mem, 0,
		  size * caf_current_team->u.image_info->image_count.count);
	allocator_unlock (&local->ai.alloc);
      }
      break;
    case CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY:
      mem = NULL;
      break;
    case CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY:
      allocator_lock (&local->ai.alloc);
      mem = SHMPTR_AS (void *, allocator_shared_malloc (&local->ai.alloc, size),
		       &local->sm);
      allocator_unlock (&local->ai.alloc);
      break;
    case CAF_REGTYPE_COARRAY_MAP_EXISTING:
      /* Computing the mem ptr is done above before the new token is allocated.
       */
      break;
    default:
      mem = alloc_get_memory_by_id (
	&local->ai, size * caf_current_team->u.image_info->image_count.count,
	next_memid);
      break;
    }

  if (unlikely (
	*token == NULL
	|| (mem == NULL && type != CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY)))
    {
      /* Freeing the memory conditionally seems pointless, but
	 caf_internal_error () may return, when a stat is given and then the
	 memory may be lost.  */
      if (mem)
	alloc_free_memory_with_id (&local->ai, next_memid);
      free (*token);
      caf_internal_error (alloc_fail_msg, stat, errmsg, errmsg_len);
      return;
    }

  shmem_token = TOKEN (*token);
  switch (type)
    {
    case CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY:
      *shmem_token
	= (struct caf_shmem_token) {NULL, NULL, NULL, size, ~0U, false};
      break;
    case CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY:
      shmem_token->memptr = mem;
      shmem_token->base = mem;
      shmem_token->image_size = size;
      shmem_token->owning_memory = true;
      break;
    case CAF_REGTYPE_COARRAY_MAP_EXISTING:
      *shmem_token
	= (struct caf_shmem_token) {mem + size * this_image.image_num,
				    GFC_DESCRIPTOR_RANK (data) > 0 ? data
								   : NULL,
				    mem,
				    size,
				    next_memid++,
				    false};
      break;
    case CAF_REGTYPE_LOCK_STATIC:
    case CAF_REGTYPE_LOCK_ALLOC:
    case CAF_REGTYPE_CRITICAL:
      *shmem_token = (struct caf_shmem_token) {
	mem,	      GFC_DESCRIPTOR_RANK (data) > 0 ? data : NULL,
	mem,	      size,
	next_memid++, false};
      break;
    default:
      *shmem_token
	= (struct caf_shmem_token) {mem + size * this_image.image_num,
				    GFC_DESCRIPTOR_RANK (data) > 0 ? data
								   : NULL,
				    mem,
				    size,
				    next_memid++,
				    true};
      break;
    }

  if (stat)
    *stat = 0;

  if (type == CAF_REGTYPE_COARRAY_STATIC || type == CAF_REGTYPE_LOCK_STATIC
      || type == CAF_REGTYPE_CRITICAL || type == CAF_REGTYPE_EVENT_STATIC)
    {
      caf_static_t *tmp = malloc (sizeof (caf_static_t));
      *tmp = (caf_static_t) {*token, caf_static_list};
      caf_static_list = tmp;
    }
  else
    {
      struct coarray_allocated *ca = caf_current_team->allocated;
      for (; ca && ca->token != shmem_token; ca = ca->next)
	;
      if (!ca)
	{
	  ca = (struct coarray_allocated *) malloc (
	    sizeof (struct coarray_allocated));
	  *ca = (struct coarray_allocated) {caf_current_team->allocated,
					    shmem_token};
	  caf_current_team->allocated = ca;
	}
    }
  GFC_DESCRIPTOR_DATA (data) = shmem_token->memptr;
}

void
_gfortran_caf_deregister (caf_token_t *token, caf_deregister_t type, int *stat,
			  char *errmsg __attribute__ ((unused)),
			  size_t errmsg_len __attribute__ ((unused)))
{
  caf_shmem_token_t shmem_token = TOKEN (*token);

  if (shmem_token->owning_memory && shmem_token->memptr)
    {
      if (shmem_token->token_id != ~0U)
	alloc_free_memory_with_id (&local->ai, (memid) shmem_token->token_id);
      else
	{
	  allocator_lock (&local->ai.alloc);
	  allocator_shared_free (&local->ai.alloc,
				 AS_SHMPTR (shmem_token->base, local->sm),
				 shmem_token->image_size);
	  allocator_unlock (&local->ai.alloc);
	}

      if (shmem_token->desc)
	GFC_DESCRIPTOR_DATA (shmem_token->desc) = NULL;
    }

  if (type != CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY)
    {
      struct coarray_allocated *ca = caf_current_team->allocated;
      if (ca && caf_current_team->allocated->token == shmem_token)
	caf_current_team->allocated = ca->next;
      else
	{
	  struct coarray_allocated *pca = NULL;
	  for (; ca && ca->token != shmem_token; pca = ca, ca = ca->next)
	    ;
	  if (!ca)
	    caf_runtime_error (
	      "Coarray token to be freeed is not in current team %d", type);
	  /* Unhook found coarray_allocated node from list...  */
	  pca->next = ca->next;
	}
      /* ... and free.  */
      free (ca);
      free (TOKEN (*token));
      *token = NULL;
    }
  else
    {
      shmem_token->memptr = NULL;
      shmem_token->owning_memory = false;
    }

  if (stat)
    *stat = 0;
}

void
_gfortran_caf_sync_all (int *stat, char *errmsg, size_t errmsg_len)
{
  __asm__ __volatile__ ("":::"memory");
  HEALTH_CHECK (stat, errmsg, errmsg_len);
  CHECK_TEAM_INTEGRITY (caf_current_team);
  sync_all ();
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
_gfortran_caf_sync_images (int count, int images[], int *stat, char *errmsg,
			   size_t errmsg_len)
{
  int *mapped_images = images;

  CHECK_TEAM_INTEGRITY (caf_current_team);
  if (count > 0)
    {
      int *map = caf_current_team->u.image_info->image_map;
      int max_id = caf_current_team->u.image_info->image_map_size;

      mapped_images = __builtin_alloca (sizeof (int) * count);
      if (!mapped_images)
	{
	  caf_internal_error ("SYNC IMAGES: Can not reserve buffer for mapping "
			      "images to internal ids. Increase stack size!",
			      stat, errmsg, errmsg_len);
	  return;
	}
      for (int c = 0; c < count; ++c)
	{
	  if (images[c] > 0 && images[c] <= max_id)
	    {
	      mapped_images[c] = map[images[c] - 1];
	      switch (this_image.supervisor->images[mapped_images[c]].status)
		{
		case IMAGE_SUCCESS:
		  caf_internal_error ("SYNC IMAGES: Image %d is stopped", stat,
				      errmsg, errmsg_len, images[c]);
		  /* We can come here only, when stat is non-NULL.  */
		  *stat = CAF_STAT_STOPPED_IMAGE;
		  return;
		case IMAGE_FAILED:
		  caf_internal_error ("SYNC IMAGES: Image %d has failed", stat,
				      errmsg, errmsg_len, images[c]);
		  /* We can come here only, when stat is non-NULL.  */
		  *stat = CAF_STAT_FAILED_IMAGE;
		  return;
		default:
		  break;
		}
	      for (int i = 0; i < c; ++i)
		if (mapped_images[c] == mapped_images[i])
		  {
		    caf_internal_error ("SYNC IMAGES: Duplicate image %d in "
					"images at position %d and &d.",
					stat, errmsg, errmsg_len, images[c],
					i + 1, c + 1);
		    /* There is no official error code for this, but 3 is what
		       OpenCoarray uses.  */
		    *stat = 3;
		    return;
		  }
	    }
	  else
	    {
	      caf_internal_error ("Invalid image number %d in SYNC IMAGES",
				  stat, errmsg, errmsg_len, images[c]);
	      return;
	    }
	}
    }
  else
    HEALTH_CHECK (stat, errmsg, errmsg_len);

  __asm__ __volatile__ ("" ::: "memory");
  sync_table (&local->si, mapped_images, count);
  HEALTH_CHECK (stat, errmsg, errmsg_len);
}

extern void _gfortran_report_exception (void);

void
_gfortran_caf_stop_numeric (int stop_code, bool quiet)
{
  if (!quiet)
    {
      _gfortran_report_exception ();
      fprintf (stderr, "STOP %d\n", stop_code);
    }
  exit (stop_code);
}

void
_gfortran_caf_stop_str (const char *string, size_t len, bool quiet)
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

/* Report that the program terminated because of a fail image issued.  */

void
_gfortran_caf_fail_image (void)
{
  fputs ("IMAGE FAILED!\n", stderr);
  this_image.supervisor->images[this_image.image_num].status = IMAGE_FAILED;
  atomic_fetch_add (&this_image.supervisor->failed_images, 1);
  exit (0);
}

/* Get the status of image IMAGE.  */

int
_gfortran_caf_image_status (int image, caf_team_t *team)
{
  caf_shmem_team_t t = caf_current_team;
  int image_index;

  if (team)
    t = *(caf_shmem_team_t *) team;

  if (image > t->u.image_info->image_count.count)
    return CAF_STAT_STOPPED_IMAGE;

  image_index = t->u.image_info->image_map[image - 1];

  switch (this_image.supervisor->images[image_index].status)
    {
    case IMAGE_FAILED:
      return CAF_STAT_FAILED_IMAGE;
    case IMAGE_SUCCESS:
      return CAF_STAT_STOPPED_IMAGE;

    /* When image status is not known, return 0.  */
    case IMAGE_OK:
    case IMAGE_UNKNOWN:
    default:
      return 0;
    }
}

static void
stopped_or_failed_images (gfc_descriptor_t *array, caf_team_t *team, int *kind,
			  image_status img_stat, const char *function_name)
{
  int local_kind = kind != NULL ? *kind : 4;
  size_t sti = 0;
  caf_shmem_team_t t = caf_current_team;

  if (team)
    t = *(caf_shmem_team_t *) team;

  int sz = t->u.image_info->image_map_size;
  for (int i = 0; i < sz; ++i)
    if (this_image.supervisor->images[t->u.image_info->image_map[i]].status
	== img_stat)
      ++sti;

  if (sti)
    {
      array->base_addr = malloc (local_kind * sti);
      array->dtype.type = BT_INTEGER;
      array->dtype.elem_len = local_kind;
      array->dim[0].lower_bound = 1;
      array->dim[0]._ubound = sti;
      array->dim[0]._stride = 1;
      array->span = local_kind;
      array->offset = 0;
      sti = 0;
      for (int i = 0; i < sz; ++i)
	if (this_image.supervisor->images[t->u.image_info->image_map[i]].status
	    == img_stat)
	  switch (local_kind)
	    {
	    case 1:
	      ((int8_t *) array->base_addr)[sti++] = i + 1;
	      break;
	    case 2:
	      ((int16_t *) array->base_addr)[sti++] = i + 1;
	      break;
	    case 4:
	      ((int32_t *) array->base_addr)[sti++] = i + 1;
	      break;
	    case 8:
	      ((int64_t *) array->base_addr)[sti++] = i + 1;
	      break;
	    default:
	      caf_runtime_error ("Unsupported kind %d in %s.", local_kind,
				 function_name);
	    }
    }
  else
    {
      array->base_addr = NULL;
      array->dtype.type = BT_INTEGER;
      array->dtype.elem_len = local_kind;
      /* Setting lower_bound higher then upper_bound is what the compiler does
	 to indicate an empty array.  */
      array->dim[0].lower_bound = 0;
      array->dim[0]._ubound = -1;
      array->dim[0]._stride = 1;
      array->offset = 0;
    }
}

void
_gfortran_caf_failed_images (gfc_descriptor_t *array, caf_team_t *team,
			     int *kind)
{
  stopped_or_failed_images (array, team, kind, IMAGE_FAILED, "FAILED_IMAGES()");
}

void
_gfortran_caf_stopped_images (gfc_descriptor_t *array, caf_team_t *team,
			      int *kind)
{
  stopped_or_failed_images (array, team, kind, IMAGE_SUCCESS,
			    "STOPPED_IMAGES()");
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

static bool
check_get_team (caf_team_t *team, int *team_number, int *stat,
		caf_shmem_team_t *cur_team)
{
  if (team || team_number)
    {
      *cur_team = caf_current_team;

      if (team)
	{
	  caf_shmem_team_t cand_team = (caf_shmem_team_t) (*team);
	  while (*cur_team && *cur_team != cand_team)
	    *cur_team = (*cur_team)->parent;
	}
      else
	while (*cur_team && (*cur_team)->u.image_info->team_id != *team_number)
	  *cur_team = (*cur_team)->parent;

      if (!*cur_team)
	{
	  if (stat)
	    {
	      *stat = 1;
	      return false;
	    }
	  else
	    caf_runtime_error ("requested team not found");
	}
    }
  else
    *cur_team = caf_current_team;

  CHECK_TEAM_INTEGRITY ((*cur_team));
  return true;
}

static bool
check_map_team (int *remote_index, int *this_index, const int image_index,
		caf_team_t *team, int *team_number, int *stat)
{
  caf_shmem_team_t selected_team;
  const bool check = check_get_team (team, team_number, stat, &selected_team);

  if (!selected_team)
    return false;
#ifndef NDEBUG
  if (image_index < 1
      || image_index > selected_team->u.image_info->image_map_size)
    {
      if (stat)
	*stat = 1;
      return false;
    }
#endif

  *remote_index = selected_team->u.image_info->image_map[image_index - 1];

  *this_index = this_image.image_num;

  return check;
}

void
_gfortran_caf_co_broadcast (gfc_descriptor_t *desc, int source_image, int *stat,
			    char *errmsg __attribute__ ((unused)),
			    size_t errmsg_len __attribute__ ((unused)))
{
  int mapped_index, this_image_index;
  if (stat)
    *stat = 0;

  if (!check_map_team (&mapped_index, &this_image_index, source_image, NULL,
		       NULL, stat))
    return;

  collsub_broadcast_array (desc, mapped_index);
}

#define GEN_OP(name, op, type)                                                 \
  static type name##_##type (type *lhs, type *rhs) { return op (*lhs, *rhs); }

#define GEN_OP_SERIES(name, op)                                                \
  GEN_OP (name, op, uint8_t)                                                   \
  GEN_OP (name, op, uint16_t)                                                  \
  GEN_OP (name, op, uint32_t)                                                  \
  GEN_OP (name, op, uint64_t)                                                  \
  GEN_OP (name, op, int8_t)                                                    \
  GEN_OP (name, op, int16_t)                                                   \
  GEN_OP (name, op, int32_t)                                                   \
  GEN_OP (name, op, int64_t)                                                   \
  GEN_OP (name, op, float)                                                     \
  GEN_OP (name, op, double)

#define CO_ADD(l, r) ((l) + (r))
#define CO_MIN(l, r) ((l) < (r) ? (l) : (r))
#define CO_MAX(l, r) ((l) > (r) ? (l) : (r))
GEN_OP_SERIES (sum, CO_ADD)
GEN_OP_SERIES (min, CO_MIN)
GEN_OP_SERIES (max, CO_MAX)

// typedef void *(*opr_t) (void *, void *);
typedef void *opr_t;

#define GFC_DESCRIPTOR_KIND(desc) ((desc)->dtype.elem_len)

#define CASE_TYPE_KIND(name, type, ctype)                                      \
  case type:                                                                   \
    {                                                                          \
      switch (GFC_DESCRIPTOR_KIND (desc))                                      \
	{                                                                      \
	case 1:                                                                \
	  opr = (opr_t) name##_##ctype##8_t;                                   \
	  break;                                                               \
	case 2:                                                                \
	  opr = (opr_t) name##_##ctype##16_t;                                  \
	  break;                                                               \
	case 4:                                                                \
	  opr = (opr_t) name##_##ctype##32_t;                                  \
	  break;                                                               \
	case 8:                                                                \
	  opr = (opr_t) name##_##ctype##64_t;                                  \
	  break;                                                               \
	default:                                                               \
	  caf_runtime_error ("" #name                                          \
			     " not available for type/kind combination");      \
	}                                                                      \
      break;                                                                   \
    }

#define SWITCH_TYPE_KIND(name)                                                 \
  switch (GFC_DESCRIPTOR_TYPE (desc))                                          \
    {                                                                          \
      CASE_TYPE_KIND (name, BT_INTEGER, int)                                   \
      CASE_TYPE_KIND (name, BT_UNSIGNED, uint)                                 \
    case BT_REAL:                                                              \
      switch (GFC_DESCRIPTOR_KIND (desc))                                      \
	{                                                                      \
	case 4:                                                                \
	  opr = (opr_t) name##_float;                                          \
	  break;                                                               \
	case 8:                                                                \
	  opr = (opr_t) name##_double;                                         \
	  break;                                                               \
	default:                                                               \
	  caf_runtime_error ("" #name                                          \
			     " not available for type/kind combination");      \
	}                                                                      \
      break;                                                                   \
    default:                                                                   \
      caf_runtime_error ("" #name " not available for type/kind combination"); \
    }

void
_gfortran_caf_co_sum (gfc_descriptor_t *desc, int result_image, int *stat,
		      char *errmsg __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
{
  int mapped_index = -1, this_image_index;
  opr_t opr;

  if (stat)
    *stat = 0;

  /* If result_image == 0 then allreduce is wanted, i.e. mapped_index = -1.  */
  if (result_image
      && !check_map_team (&mapped_index, &this_image_index, result_image, NULL,
			  NULL, stat))
    return;

  SWITCH_TYPE_KIND (sum)

  collsub_reduce_array (desc, mapped_index, opr, 0, 0);
}

void
_gfortran_caf_co_min (gfc_descriptor_t *desc, int result_image, int *stat,
		      char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
{
  int mapped_index = -1, this_image_index;
  opr_t opr;

  if (stat)
    *stat = 0;
  /* If result_image == 0 then allreduce is wanted, i.e. mapped_index = -1.  */
  if (result_image
      && !check_map_team (&mapped_index, &this_image_index, result_image, NULL,
			  NULL, stat))
    return;

  SWITCH_TYPE_KIND (min)

  collsub_reduce_array (desc, mapped_index, opr, 0, 0);
}

void
_gfortran_caf_co_max (gfc_descriptor_t *desc, int result_image, int *stat,
		      char *errmsg __attribute__ ((unused)),
		      int a_len __attribute__ ((unused)),
		      size_t errmsg_len __attribute__ ((unused)))
{
  int mapped_index = -1, this_image_index;
  opr_t opr;

  if (stat)
    *stat = 0;
  /* If result_image == 0 then allreduce is wanted, i.e. mapped_index = -1.  */
  if (result_image
      && !check_map_team (&mapped_index, &this_image_index, result_image, NULL,
			  NULL, stat))
    return;

  SWITCH_TYPE_KIND (max)

  collsub_reduce_array (desc, mapped_index, opr, 0, 0);
}

void
_gfortran_caf_co_reduce (gfc_descriptor_t *desc, void *(*opr) (void *, void *),
			 int opr_flags, int result_image, int *stat,
			 char *errmsg __attribute__ ((unused)), int desc_len,
			 size_t errmsg_len __attribute__ ((unused)))
{
  int mapped_index = -1, this_image_index;

  if (stat)
    *stat = 0;

  /* If result_image == 0 then allreduce is wanted, i.e. mapped_index = -1.  */
  if (result_image
      && !check_map_team (&mapped_index, &this_image_index, result_image, NULL,
			  NULL, stat))
    return;

  collsub_reduce_array (desc, mapped_index, opr, opr_flags, desc_len);
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
  caf_team_t *team, int *team_number)
{
  caf_shmem_token_t shmem_token = TOKEN (token);
  void *src_ptr;
  int32_t free_buffer;
  int remote_image_index, this_image_index;
  void *dst_ptr = opt_dst_desc ? (void *)opt_dst_desc : dst_data;
  void *old_dst_data_ptr = NULL, *old_src_data_ptr = NULL;
  struct caf_shmem_token cb_token = {add_data, NULL, add_data, 0, ~0, false};

  if (stat)
    *stat = 0;

  if (!check_map_team (&remote_image_index, &this_image_index, image_index,
		       team, team_number, stat))
    return;

  /* Compute the address only after team's mapping has taken place.  */
  src_ptr = shmem_token->base + remote_image_index * shmem_token->image_size;
  if (opt_src_desc)
    {
      old_src_data_ptr = opt_src_desc->base_addr;
      ((gfc_descriptor_t *) opt_src_desc)->base_addr = src_ptr;
      src_ptr = (void *) opt_src_desc;
    }

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

  if (old_src_data_ptr)
    ((gfc_descriptor_t *) opt_src_desc)->base_addr = old_src_data_ptr;
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

  caf_shmem_token_t shmem_token = TOKEN (token);
  int32_t result;
  struct caf_shmem_token cb_token = {add_data, NULL, add_data, 0, ~0, false};
  void *src_ptr, *arg;
  int remote_image_index, this_image_index;
  GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) temp_desc;

  if (!check_map_team (&remote_image_index, &this_image_index, image_index,
		       NULL, NULL, NULL))
    return 0;

  src_ptr = shmem_token->base + remote_image_index * shmem_token->image_size;
  if (shmem_token->desc)
    {
      memcpy (&temp_desc, shmem_token->desc,
	      sizeof (gfc_descriptor_t)
		+ GFC_DESCRIPTOR_RANK (shmem_token->desc)
		    * sizeof (descriptor_dimension));
      temp_desc.base_addr = src_ptr;
      arg = &temp_desc;
    }
  else
    arg = &src_ptr;

  accessor_hash_table[present_index].u.is_present (add_data, &image_index,
						   &result, arg, &cb_token, 0);

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
  caf_team_t *team, int *team_number)
{
  caf_shmem_token_t shmem_token = TOKEN (token);
  void *dst_ptr, *dst_data_ptr, *old_dst_data_ptr = NULL;
  const void *src_ptr = opt_src_desc ? (void *) opt_src_desc : src_data;
  struct caf_shmem_token cb_token = {add_data, NULL, add_data, 0, ~0, false};
  int remote_image_index, this_image_index;
  GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) temp_src_desc;

  if (stat)
    *stat = 0;

  if (!check_map_team (&remote_image_index, &this_image_index, image_index,
		       team, team_number, stat))
    return;

  dst_data_ptr = dst_ptr
    = shmem_token->base + remote_image_index * shmem_token->image_size;
  if (opt_dst_desc)
    {
      old_dst_data_ptr = opt_dst_desc->base_addr;
      ((gfc_descriptor_t *) opt_dst_desc)->base_addr = dst_ptr;
      dst_ptr = (void *) opt_dst_desc;
    }

  /* Try to detect copy to self, with overlapping data segment.  */
  if (opt_src_desc && remote_image_index == this_image_index)
    {
      size_t src_data_span = GFC_DESCRIPTOR_SIZE (opt_src_desc);
      for (int d = 0; d < GFC_DESCRIPTOR_RANK (opt_src_desc); d++)
	src_data_span *= GFC_DESCRIPTOR_EXTENT (opt_src_desc, d);
      if (GFC_DESCRIPTOR_DATA (opt_src_desc) >= dst_data_ptr
	  && dst_data_ptr <= GFC_DESCRIPTOR_DATA (opt_src_desc) + src_data_span)
	{
	  src_ptr = __builtin_alloca (src_data_span);
	  if (!src_ptr)
	    {
	      caf_internal_error ("Out of stack in coarray send (dst[...] = "
				  "...) expression. Increase stacksize!",
				  stat, NULL, 0);
	      return;
	    }
	  memcpy ((void *) src_ptr, GFC_DESCRIPTOR_DATA (opt_src_desc),
		  src_data_span);
	  memcpy (&temp_src_desc, opt_src_desc,
		  sizeof (gfc_descriptor_t)
		    + sizeof (descriptor_dimension)
			* GFC_DESCRIPTOR_RANK (opt_src_desc));
	  temp_src_desc.base_addr = (void *) src_ptr;
	  src_ptr = (void *) &temp_src_desc;
	}
    }

  accessor_hash_table[accessor_index].u.receiver (add_data, &image_index,
						  dst_ptr, src_ptr, &cb_token,
						  0, opt_dst_charlen,
						  opt_src_charlen);

  if (old_dst_data_ptr)
    ((gfc_descriptor_t *) opt_dst_desc)->base_addr = old_dst_data_ptr;
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
  int *src_stat, caf_team_t *dst_team, int *dst_team_number,
  caf_team_t *src_team, int *src_team_number)
{
  static const char *out_of_stack_errmsg
    = "Out of stack in coarray transfer between remotes (dst[...] = "
      "src[...]) expression. Increase stacksize!";
  caf_shmem_token_t src_shmem_token = TOKEN (src_token),
		    dst_shmem_token = TOKEN (dst_token);
  void *src_ptr, *old_src_data_ptr = NULL;
  int32_t free_buffer;
  void *dst_ptr, *old_dst_data_ptr = NULL;
  void *transfer_ptr, *buffer;
  GFC_FULL_ARRAY_DESCRIPTOR (GFC_MAX_DIMENSIONS, void) *transfer_desc = NULL;
  struct caf_shmem_token cb_token
    = {src_add_data, NULL, src_add_data, 0, ~0, false};
  int remote_image_index, this_image_index;

  if (src_stat)
    *src_stat = 0;

  if (!check_map_team (&remote_image_index, &this_image_index, src_image_index,
		       src_team, src_team_number, src_stat))
    return;

  if (!scalar_transfer)
    {
      const size_t desc_size = sizeof (*transfer_desc);
      transfer_desc = __builtin_alloca (desc_size);
      if (!transfer_desc)
	{
	  caf_internal_error (out_of_stack_errmsg, src_stat, NULL, 0);
	  return;
	}
      memset (transfer_desc, 0, desc_size);
      transfer_ptr = transfer_desc;
    }
  else if (opt_dst_charlen)
    {
      transfer_ptr = __builtin_alloca (*opt_dst_charlen * src_size);
      if (!transfer_ptr)
	{
	  caf_internal_error (out_of_stack_errmsg, src_stat, NULL, 0);
	  return;
	}
    }
  else
    {
      buffer = NULL;
      transfer_ptr = &buffer;
    }

  src_ptr
    = src_shmem_token->base + remote_image_index * src_shmem_token->image_size;
  if (opt_src_desc)
    {
      old_src_data_ptr = opt_src_desc->base_addr;
      ((gfc_descriptor_t *) opt_src_desc)->base_addr = src_ptr;
      src_ptr = (void *) opt_src_desc;
    }

  accessor_hash_table[src_access_index].u.getter (
    src_add_data, &src_image_index, transfer_ptr, &free_buffer, src_ptr,
    &cb_token, 0, opt_dst_charlen, opt_src_charlen);

  if (old_src_data_ptr)
    ((gfc_descriptor_t *) opt_src_desc)->base_addr = old_src_data_ptr;

  if (dst_stat)
    *dst_stat = 0;

  if (!check_map_team (&remote_image_index, &this_image_index, dst_image_index,
		       dst_team, dst_team_number, dst_stat))
    return;

  if (scalar_transfer)
    transfer_ptr = *(void **) transfer_ptr;

  dst_ptr
    = dst_shmem_token->base + remote_image_index * dst_shmem_token->image_size;
  if (opt_dst_desc)
    {
      old_dst_data_ptr = opt_dst_desc->base_addr;
      ((gfc_descriptor_t *) opt_dst_desc)->base_addr = dst_ptr;
      dst_ptr = (void *) opt_dst_desc;
    }

  cb_token.memptr = cb_token.base = dst_add_data;
  accessor_hash_table[dst_access_index].u.receiver (dst_add_data,
						    &dst_image_index, dst_ptr,
						    transfer_ptr, &cb_token, 0,
						    opt_dst_charlen,
						    opt_src_charlen);

  if (old_dst_data_ptr)
    ((gfc_descriptor_t *) opt_dst_desc)->base_addr = old_dst_data_ptr;

  if (free_buffer)
    free (transfer_desc ? transfer_desc->base_addr : transfer_ptr);
}

#define GET_ATOM                                                               \
  caf_shmem_token_t shmem_token = TOKEN (token);                               \
  int remote_image_index, this_image_index;                                    \
  if (stat)                                                                    \
    *stat = 0;                                                                 \
  if (!image_index)                                                            \
    image_index = this_image.image_num + 1;                                    \
  if (!check_map_team (&remote_image_index, &this_image_index, image_index,    \
		       NULL, NULL, stat))                                      \
    return;                                                                    \
  assert (kind == 4);                                                          \
  uint32_t *atom                                                               \
    = (uint32_t *) (shmem_token->base                                          \
		    + remote_image_index * shmem_token->image_size + offset)

void
_gfortran_caf_atomic_define (caf_token_t token, size_t offset, int image_index,
			     void *value, int *stat,
			     int type __attribute__ ((unused)), int kind)
{
  GET_ATOM;

  __atomic_store (atom, (uint32_t *) value, __ATOMIC_SEQ_CST);
}

void
_gfortran_caf_atomic_ref (caf_token_t token, size_t offset, int image_index,
			  void *value, int *stat,
			  int type __attribute__ ((unused)), int kind)
{
  GET_ATOM;

  __atomic_load (atom, (uint32_t *) value, __ATOMIC_SEQ_CST);
}

void
_gfortran_caf_atomic_cas (caf_token_t token, size_t offset, int image_index,
			  void *old, void *compare, void *new_val, int *stat,
			  int type __attribute__ ((unused)), int kind)
{
  GET_ATOM;

  *(uint32_t *) old = *(uint32_t *) compare;
  (void) __atomic_compare_exchange_n (atom, (uint32_t *) old,
				      *(uint32_t *) new_val, false,
				      __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
}

void
_gfortran_caf_atomic_op (int op, caf_token_t token, size_t offset,
			 int image_index, void *value, void *old, int *stat,
			 int type __attribute__ ((unused)), int kind)
{
  GET_ATOM;

  uint32_t res;

  switch (op)
    {
    case GFC_CAF_ATOMIC_ADD:
      res = __atomic_fetch_add (atom, *(uint32_t *) value, __ATOMIC_SEQ_CST);
      break;
    case GFC_CAF_ATOMIC_AND:
      res = __atomic_fetch_and (atom, *(uint32_t *) value, __ATOMIC_SEQ_CST);
      break;
    case GFC_CAF_ATOMIC_OR:
      res = __atomic_fetch_or (atom, *(uint32_t *) value, __ATOMIC_SEQ_CST);
      break;
    case GFC_CAF_ATOMIC_XOR:
      res = __atomic_fetch_xor (atom, *(uint32_t *) value, __ATOMIC_SEQ_CST);
      break;
    default:
      __builtin_unreachable ();
    }

  if (old)
    *(uint32_t *) old = res;
}

#define GET_EVENT(token_, index_, image_index_)                                \
  ((event_t *) (((caf_shmem_token_t) token_)->base                             \
		+ ((caf_shmem_token_t) token_)->image_size * image_index_      \
		+ sizeof (event_t) * index_))

void
_gfortran_caf_event_post (caf_token_t token, size_t index, int image_index,
			  int *stat, char *errmsg __attribute__ ((unused)),
			  size_t errmsg_len __attribute__ ((unused)))
{
  int remote_image_index, this_image_index;

  if (stat)
    *stat = 0;

  /* When image_index is zero, access this image's event.  */
  if (!image_index)
    image_index = this_image.image_num + 1;

  if (!check_map_team (&remote_image_index, &this_image_index, image_index,
		       NULL, NULL, stat))
    return;

  volatile event_t *event = GET_EVENT (token, index, remote_image_index);

  lock_event (&local->si);
  --(*event);
  event_post (&local->si);
  unlock_event (&local->si);
}

void
_gfortran_caf_event_wait (caf_token_t token, size_t index, int until_count,
			  int *stat, char *errmsg __attribute__ ((unused)),
			  size_t errmsg_len __attribute__ ((unused)))
{
  int remote_image_index, this_image_index;

  if (stat)
    *stat = 0;

  if (!check_map_team (&remote_image_index, &this_image_index, 1, NULL, NULL,
		       stat))
    return;

  volatile event_t *event = GET_EVENT (token, index, this_image_index);
  event_t val;

  lock_event (&local->si);
  val = (*event += until_count);
  if (val > 0) /* Move the invariant out of the loop.  */
    while (*event > 0)
      event_wait (&local->si);
  unlock_event (&local->si);

  if (stat)
    *stat = 0;
}

void
_gfortran_caf_event_query (caf_token_t token, size_t index, int image_index,
			   int *count, int *stat)
{
  int remote_image_index, this_image_index;

  if (stat)
    *stat = 0;

  /* When image_index is zero, access this image's event.  */
  if (!image_index)
    image_index = this_image.image_num + 1;

  if (!check_map_team (&remote_image_index, &this_image_index, image_index,
		       NULL, NULL, stat))
    return;

  volatile event_t *event = GET_EVENT (token, index, remote_image_index);

  lock_event (&local->si);
  *count = *event;
  unlock_event (&local->si);

  if (*count < 0)
    *count = -*count;
}

void
_gfortran_caf_lock (caf_token_t token, size_t index,
		    int image_index __attribute__ ((unused)),
		    int *acquired_lock, int *stat, char *errmsg,
		    size_t errmsg_len)
{
  const char *msg = "Already locked";
  lock_t *lock = &((lock_t *) MEMTOK (token))[index];
  int res;

  res
    = acquired_lock ? pthread_mutex_trylock (lock) : pthread_mutex_lock (lock);

  if (stat)
    *stat = res == EBUSY ? GFC_STAT_LOCKED : 0;

  if (acquired_lock)
    {
      *acquired_lock = (int) (res == 0);
      return;
    }

  if (!res)
    return;

  if (stat)
    {
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
  lock_t *lock = &((lock_t *) MEMTOK (token))[index];
  int res;

  res = pthread_mutex_unlock (lock);

  if (res == 0)
    {
      if (stat)
	*stat = 0;
      return;
    }

  if (stat && res == EPERM)
    {
      /* res == EPERM means that the lock is locked.  Now figure, if by us by
	 trying to lock it or by other image, which fails.  */
      res = pthread_mutex_trylock (lock);
      if (res == EBUSY)
	*stat = GFC_STAT_LOCKED_OTHER_IMAGE;
      else
	{
	  *stat = GFC_STAT_UNLOCKED;
	  pthread_mutex_unlock (lock);
	}

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
extern void _gfortran_random_seed_i4 (int32_t *size, gfc_array_i4 *put,
				      gfc_array_i4 *get);

void _gfortran_caf_random_init (bool repeatable, bool image_distinct)
{
  static struct
  {
    int32_t *base_addr;
    size_t offset;
    dtype_type dtype;
    index_type span;
    descriptor_dimension dim[1];
  } rand_seed;
  static bool rep_needs_init = true, arr_needs_init = true;
  static int32_t seed_size;

  if (arr_needs_init)
    {
      _gfortran_random_seed_i4 (&seed_size, NULL, NULL);
      memset (&rand_seed, 0,
	      sizeof (gfc_array_i4) + sizeof (descriptor_dimension));
      rand_seed.base_addr
	= malloc (seed_size * sizeof (int32_t)); // because using seed_i4
      rand_seed.offset = -1;
      rand_seed.dtype.elem_len = sizeof (int32_t);
      rand_seed.dtype.rank = 1;
      rand_seed.dtype.type = BT_INTEGER;
      rand_seed.span = 0;
      rand_seed.dim[0].lower_bound = 1;
      rand_seed.dim[0]._ubound = seed_size;
      rand_seed.dim[0]._stride = 1;

      arr_needs_init = false;
    }

  if (repeatable)
    {
      if (rep_needs_init)
	{
	  int32_t lcg_seed = 57911963;
	  if (image_distinct)
	    {
	      lcg_seed *= this_image.image_num;
	    }
	  int32_t *curr = rand_seed.base_addr;
	  for (int i = 0; i < seed_size; ++i)
	    {
	      const int32_t a = 16087;
	      const int32_t m = INT32_MAX;
	      const int32_t q = 127773;
	      const int32_t r = 2836;
	      lcg_seed = a * (lcg_seed % q) - r * (lcg_seed / q);
	      if (lcg_seed <= 0)
		lcg_seed += m;
	      *curr = lcg_seed;
	      ++curr;
	    }
	  rep_needs_init = false;
	}
      _gfortran_random_seed_i4 (NULL, (gfc_array_i4 *) &rand_seed, NULL);
    }
  else if (image_distinct)
    {
      _gfortran_random_seed_i4 (NULL, NULL, NULL);
    }
  else
    {
      if (this_image.image_num == 0)
	{
	  _gfortran_random_seed_i4 (NULL, NULL, (gfc_array_i4 *) &rand_seed);
	  collsub_broadcast_array ((gfc_descriptor_t *) &rand_seed, 0);
	}
      else
	{
	  collsub_broadcast_array ((gfc_descriptor_t *) &rand_seed, 0);
	  _gfortran_random_seed_i4 (NULL, (gfc_array_i4 *) &rand_seed, NULL);
	}
    }
}

void
_gfortran_caf_form_team (int team_no, caf_team_t *team, int *new_index,
			 int *stat, char *errmsg, size_t errmsg_len)
{
  const char new_index_out_of_range[]
    = "The NEW_INDEX in a FORM TEAM has to in (0, num_images()].";
  const char team_no_negativ[]
    = "The team number in FORM TEAM has to be positive.";
  const char alloc_fail_msg[] = "Failed to allocate team";
  const char non_unique_image_ids[]
    = "The NEW_INDEX of FORM TEAMs has to be unique.";
  const char cannot_assign_index[]
    = "Can not assign new image index in FORM TEAM.";
  static int image_size_shift = -1;
  static int teams_count = 0;
  caf_shmem_team_t t;
  bool created;
  memid tmemid;

  if (image_size_shift < 0)
    image_size_shift = (int) round (log2 (local->total_num_images));
  if (stat)
    *stat = 0;

  CHECK_TEAM_INTEGRITY (caf_current_team);

  if (new_index
      && (*new_index <= 0
	  || *new_index > caf_current_team->u.image_info->image_count.count))
    {
      caf_internal_error (new_index_out_of_range, stat, errmsg, errmsg_len);
      return;
    }
  if (team_no <= 0)
    {
      caf_internal_error (team_no_negativ, stat, errmsg, errmsg_len);
      return;
    }

  *team = malloc (sizeof (struct caf_shmem_team));
  if (unlikely (*team == NULL))
    {
      caf_internal_error (alloc_fail_msg, stat, errmsg, errmsg_len);
      return;
    }
  t = *((caf_shmem_team_t *) team);

  allocator_lock (&local->ai.alloc);
  if (caf_current_team->team_no == -1)
    tmemid = team_no + teams_count;
  else
    tmemid = (caf_current_team->u.image_info->lastmemid << image_size_shift)
	     + team_no + teams_count;
  ++teams_count;
  *t = (struct caf_shmem_team) {
    caf_teams_formed,
    team_no,
    -1,
    0,
    NULL,
    {alloc_get_memory_by_id_created (
      &local->ai,
      sizeof (struct shmem_image_info)
	+ caf_current_team->u.image_info->image_count.count * sizeof (int),
      -tmemid, &created)}};

  if (created)
    {
      counter_barrier_init (&t->u.image_info->image_count, 0);
      collsub_init_supervisor (&t->u.image_info->collsub,
			       alloc_get_allocator (&local->ai), 0);
      t->u.image_info->team_parent_id = caf_current_team->team_no;
      t->u.image_info->team_id = team_no;
      t->u.image_info->image_map_size = 0;
      t->u.image_info->num_term_images = 0;
      t->u.image_info->lastmemid = tmemid;
      /* Initialize a freshly created image_map with -1.  */
      for (int i = 0; i < caf_current_team->u.image_info->image_count.count;
	   ++i)
	t->u.image_info->image_map[i] = -1;
    }
  counter_barrier_add (&t->u.image_info->image_count, 1);
  counter_barrier_add (&t->u.image_info->collsub.barrier, 1);
  allocator_unlock (&local->ai.alloc);

  if (new_index)
    {
      int old_id;

      t->index = *new_index - 1;
      old_id = __atomic_exchange_n (&t->u.image_info->image_map[t->index],
				    this_image.image_num, __ATOMIC_SEQ_CST);
      if (old_id != -1)
	{
	  caf_internal_error (non_unique_image_ids, stat, errmsg, errmsg_len);
	  return;
	}

      __atomic_fetch_add (&t->u.image_info->image_map_size, 1,
			  __ATOMIC_SEQ_CST);
    }
  else
    {
      int im;
      int exp = -1;

      __atomic_fetch_add (&t->u.image_info->image_map_size, 1,
			  __ATOMIC_SEQ_CST);
      sync_team (caf_current_team);

      im = caf_current_team->index * t->u.image_info->image_map_size
	   / caf_current_team->u.image_info->image_count.count;
      /* Map our old index into the domain of the new team's size.  */
      if (__atomic_compare_exchange_n (&t->u.image_info->image_map[im], &exp,
				       this_image.image_num, false,
				       __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST))
	t->index = im;
      else
	{
	  caf_internal_error (cannot_assign_index, stat, errmsg, errmsg_len);
	  return;
	}
    }
  sync_team (caf_current_team);

  caf_teams_formed = t;
}

void
_gfortran_caf_change_team (caf_team_t team, int *stat,
			   char *errmsg __attribute__ ((unused)),
			   size_t errmsg_len __attribute__ ((unused)))
{
  caf_shmem_team_t t = (caf_shmem_team_t) team;

  if (stat)
    *stat = 0;

  if (t == caf_teams_formed)
    caf_teams_formed = t->parent;
  else
    for (caf_shmem_team_t p = caf_teams_formed; p; p = p->parent)
      if (p->parent == t)
	{
	  p->parent = t->parent;
	  break;
	}

  t->parent = caf_current_team;
  t->parent_teams_last_active_memid = next_memid;
  next_memid = (t->u.image_info->team_parent_id != -1
		  ? (((memid) t->u.image_info->team_parent_id) << 48)
		  : 0)
	       | (((memid) t->u.image_info->team_id) << 32) | 1;
  caf_current_team = t;
  sync_team (caf_current_team);
}

void
_gfortran_caf_end_team (int *stat, char *errmsg, size_t errmsg_len)
{
  caf_shmem_team_t t = caf_current_team;

  if (stat)
    *stat = 0;

  caf_current_team = caf_current_team->parent;
  next_memid = t->parent_teams_last_active_memid;
  sync_team (t);

  for (struct coarray_allocated *ca = t->allocated; ca;)
    {
      struct coarray_allocated *nca = ca->next;
      _gfortran_caf_deregister ((caf_token_t *) &ca->token,
				CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY, stat,
				errmsg, errmsg_len);
      free (ca);
      ca = nca;
    }
  t->allocated = NULL;
  t->parent = caf_teams_formed;
  caf_teams_formed = t;
}

void
_gfortran_caf_sync_team (caf_team_t team, int *stat, char *errmsg,
			 size_t errmsg_len)
{
  caf_shmem_team_t team_to_sync = (caf_shmem_team_t) team;
  caf_shmem_team_t active_team = caf_current_team;

  if (stat)
    *stat = 0;

  /* Check if team to sync is a child of the current team, aka not changed to
     yet.  */
  if (team_to_sync->u.image_info->team_parent_id != active_team->team_no)
    for (; active_team && active_team != team_to_sync;
	 active_team = active_team->parent)
      ;

  CHECK_TEAM_INTEGRITY (active_team);

  if (!active_team)
    {
      caf_internal_error ("SYNC TEAM: Called on team different from current, "
			  "or ancestor, or child",
			  stat, errmsg, errmsg_len);
      return;
    }

  sync_team (team_to_sync);
}

int
_gfortran_caf_team_number (caf_team_t team)
{
  return team ? ((caf_shmem_team_t) team)->u.image_info->team_id
	      : caf_current_team->u.image_info->team_id;
}

caf_team_t
_gfortran_caf_get_team (int32_t *level)
{
  if (!level)
    return caf_current_team;

  switch ((caf_team_level_t) *level)
    {
    case CAF_INITIAL_TEAM:
      return caf_initial_team;
    case CAF_PARENT_TEAM:
      return caf_current_team->parent ? caf_current_team->parent
				      : caf_current_team;
    case CAF_CURRENT_TEAM:
      return caf_current_team;
    default:
      caf_runtime_error ("Illegal value for GET_TEAM");
    }
  return NULL; /* To prevent any warnings.  */
}
