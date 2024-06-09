/* Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file contains wrappers for the system allocation routines.  Most
   places in the OpenMP API do not make any provision for failure, so in
   general we cannot allow memory allocation to fail.  */

#define _GNU_SOURCE
#include "libgomp.h"
#include <stdlib.h>
#include <string.h>
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
#include <dlfcn.h>
#endif

/* Keeping track whether a Fortran scalar allocatable/pointer has been
   allocated via 'omp allocators'/'omp allocate'.  */

struct fort_alloc_splay_tree_key_s {
  void *ptr;
};

typedef struct fort_alloc_splay_tree_node_s *fort_alloc_splay_tree_node;
typedef struct fort_alloc_splay_tree_s *fort_alloc_splay_tree;
typedef struct fort_alloc_splay_tree_key_s *fort_alloc_splay_tree_key;

static inline int
fort_alloc_splay_compare (fort_alloc_splay_tree_key x, fort_alloc_splay_tree_key y)
{
  if (x->ptr < y->ptr)
    return -1;
  if (x->ptr > y->ptr)
    return 1;
  return 0;
}
#define splay_tree_prefix fort_alloc
#define splay_tree_static
#include "splay-tree.h"

#define splay_tree_prefix fort_alloc
#define splay_tree_static
#define splay_tree_c
#include "splay-tree.h"

static struct fort_alloc_splay_tree_s fort_alloc_scalars;

/* Add pointer as being alloced by GOMP_alloc.  */
void
GOMP_add_alloc (void *ptr)
{
  if (ptr == NULL)
    return;
  fort_alloc_splay_tree_node item;
  item = gomp_malloc (sizeof (struct splay_tree_node_s));
  item->key.ptr = ptr;
  item->left = NULL;
  item->right = NULL;
  fort_alloc_splay_tree_insert (&fort_alloc_scalars, item);
}

/* Remove pointer, either called by FREE or by REALLOC,
   either of them can change the allocation status.  */
bool
GOMP_is_alloc (void *ptr)
{
  struct fort_alloc_splay_tree_key_s needle;
  fort_alloc_splay_tree_node n;
  needle.ptr = ptr;
  n = fort_alloc_splay_tree_lookup_node (&fort_alloc_scalars, &needle);
  if (n)
    {
      fort_alloc_splay_tree_remove (&fort_alloc_scalars, &n->key);
      free (n);
    }
  return n != NULL;
}


#define omp_max_predefined_alloc omp_thread_mem_alloc

/* These macros may be overridden in config/<target>/allocator.c.
   The defaults (no override) are to return NULL for pinned memory requests
   and pass through to the regular OS calls otherwise.
   The following definitions (ab)use comma operators to avoid unused
   variable errors.  */
#ifndef MEMSPACE_ALLOC
#define MEMSPACE_ALLOC(MEMSPACE, SIZE, PIN) \
  (PIN ? NULL : malloc (((void)(MEMSPACE), (SIZE))))
#endif
#ifndef MEMSPACE_CALLOC
#define MEMSPACE_CALLOC(MEMSPACE, SIZE, PIN) \
  (PIN ? NULL : calloc (1, (((void)(MEMSPACE), (SIZE)))))
#endif
#ifndef MEMSPACE_REALLOC
#define MEMSPACE_REALLOC(MEMSPACE, ADDR, OLDSIZE, SIZE, OLDPIN, PIN) \
   ((PIN) || (OLDPIN) ? NULL \
   : realloc (ADDR, (((void)(MEMSPACE), (void)(OLDSIZE), (SIZE)))))
#endif
#ifndef MEMSPACE_FREE
#define MEMSPACE_FREE(MEMSPACE, ADDR, SIZE, PIN) \
  if (PIN) free (((void)(MEMSPACE), (void)(SIZE), (ADDR)))
#endif
#ifndef MEMSPACE_VALIDATE
#define MEMSPACE_VALIDATE(MEMSPACE, ACCESS, PIN) \
  (PIN ? 0 : ((void)(MEMSPACE), (void)(ACCESS), 1))
#endif

/* Map the predefined allocators to the correct memory space.
   The index to this table is the omp_allocator_handle_t enum value.
   When the user calls omp_alloc with a predefined allocator this
   table determines what memory they get.  */
static const omp_memspace_handle_t predefined_alloc_mapping[] = {
  omp_default_mem_space,   /* omp_null_allocator doesn't actually use this. */
  omp_default_mem_space,   /* omp_default_mem_alloc. */
  omp_large_cap_mem_space, /* omp_large_cap_mem_alloc. */
  omp_const_mem_space,     /* omp_const_mem_alloc. */
  omp_high_bw_mem_space,   /* omp_high_bw_mem_alloc. */
  omp_low_lat_mem_space,   /* omp_low_lat_mem_alloc. */
  omp_low_lat_mem_space,   /* omp_cgroup_mem_alloc (implementation defined). */
  omp_low_lat_mem_space,   /* omp_pteam_mem_alloc (implementation defined). */
  omp_low_lat_mem_space,   /* omp_thread_mem_alloc (implementation defined). */
};

#define ARRAY_SIZE(A) (sizeof (A) / sizeof ((A)[0]))
_Static_assert (ARRAY_SIZE (predefined_alloc_mapping)
		== omp_max_predefined_alloc + 1,
		"predefined_alloc_mapping must match omp_memspace_handle_t");

enum gomp_numa_memkind_kind
{
  GOMP_MEMKIND_NONE = 0,
#define GOMP_MEMKIND_KINDS \
  GOMP_MEMKIND_KIND (HBW_INTERLEAVE),		\
  GOMP_MEMKIND_KIND (HBW_PREFERRED),		\
  GOMP_MEMKIND_KIND (DAX_KMEM_ALL),		\
  GOMP_MEMKIND_KIND (DAX_KMEM),			\
  GOMP_MEMKIND_KIND (INTERLEAVE),		\
  GOMP_MEMKIND_KIND (DEFAULT)
#define GOMP_MEMKIND_KIND(kind) GOMP_MEMKIND_##kind
  GOMP_MEMKIND_KINDS,
#undef GOMP_MEMKIND_KIND
  GOMP_MEMKIND_COUNT,
  GOMP_MEMKIND_LIBNUMA = GOMP_MEMKIND_COUNT
};

struct omp_allocator_data
{
  omp_memspace_handle_t memspace;
  omp_uintptr_t alignment;
  omp_uintptr_t pool_size;
  omp_uintptr_t used_pool_size;
  omp_allocator_handle_t fb_data;
  unsigned int sync_hint : 8;
  unsigned int access : 8;
  unsigned int fallback : 8;
  unsigned int pinned : 1;
  unsigned int partition : 7;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
  unsigned int memkind : 8;
#endif
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_t lock;
#endif
};

struct omp_mem_header
{
  void *ptr;
  size_t size;
  omp_allocator_handle_t allocator;
  void *pad;
};

struct gomp_libnuma_data
{
  void *numa_handle;
  void *(*numa_alloc_local) (size_t);
  void *(*numa_realloc) (void *, size_t, size_t);
  void (*numa_free) (void *, size_t);
};

struct gomp_memkind_data
{
  void *memkind_handle;
  void *(*memkind_malloc) (void *, size_t);
  void *(*memkind_calloc) (void *, size_t, size_t);
  void *(*memkind_realloc) (void *, void *, size_t);
  void (*memkind_free) (void *, void *);
  int (*memkind_check_available) (void *);
  void **kinds[GOMP_MEMKIND_COUNT];
};

#ifdef LIBGOMP_USE_LIBNUMA
static struct gomp_libnuma_data *libnuma_data;
static pthread_once_t libnuma_data_once = PTHREAD_ONCE_INIT;

static void
gomp_init_libnuma (void)
{
  void *handle = dlopen ("libnuma.so.1", RTLD_LAZY);
  struct gomp_libnuma_data *data;

  data = calloc (1, sizeof (struct gomp_libnuma_data));
  if (data == NULL)
    {
      if (handle)
	dlclose (handle);
      return;
    }
  if (handle)
    {
      int (*numa_available) (void);
      numa_available
	= (__typeof (numa_available)) dlsym (handle, "numa_available");
      if (!numa_available || numa_available () != 0)
	{
	  dlclose (handle);
	  handle = NULL;
	}
    }
  if (!handle)
    {
      __atomic_store_n (&libnuma_data, data, MEMMODEL_RELEASE);
      return;
    }
  data->numa_handle = handle;
  data->numa_alloc_local
    = (__typeof (data->numa_alloc_local)) dlsym (handle, "numa_alloc_local");
  data->numa_realloc
    = (__typeof (data->numa_realloc)) dlsym (handle, "numa_realloc");
  data->numa_free
    = (__typeof (data->numa_free)) dlsym (handle, "numa_free");
  __atomic_store_n (&libnuma_data, data, MEMMODEL_RELEASE);
}

static struct gomp_libnuma_data *
gomp_get_libnuma (void)
{
  struct gomp_libnuma_data *data
    = __atomic_load_n (&libnuma_data, MEMMODEL_ACQUIRE);
  if (data)
    return data;
  pthread_once (&libnuma_data_once, gomp_init_libnuma);
  return __atomic_load_n (&libnuma_data, MEMMODEL_ACQUIRE);
}
#endif

#ifdef LIBGOMP_USE_MEMKIND
static struct gomp_memkind_data *memkind_data;
static pthread_once_t memkind_data_once = PTHREAD_ONCE_INIT;

static void
gomp_init_memkind (void)
{
  void *handle = dlopen ("libmemkind.so.0", RTLD_LAZY);
  struct gomp_memkind_data *data;
  int i;
  static const char *kinds[] = {
    NULL,
#define GOMP_MEMKIND_KIND(kind) "MEMKIND_" #kind
    GOMP_MEMKIND_KINDS
#undef GOMP_MEMKIND_KIND
  };

  data = calloc (1, sizeof (struct gomp_memkind_data));
  if (data == NULL)
    {
      if (handle)
	dlclose (handle);
      return;
    }
  if (!handle)
    {
      __atomic_store_n (&memkind_data, data, MEMMODEL_RELEASE);
      return;
    }
  data->memkind_handle = handle;
  data->memkind_malloc
    = (__typeof (data->memkind_malloc)) dlsym (handle, "memkind_malloc");
  data->memkind_calloc
    = (__typeof (data->memkind_calloc)) dlsym (handle, "memkind_calloc");
  data->memkind_realloc
    = (__typeof (data->memkind_realloc)) dlsym (handle, "memkind_realloc");
  data->memkind_free
    = (__typeof (data->memkind_free)) dlsym (handle, "memkind_free");
  data->memkind_check_available
    = (__typeof (data->memkind_check_available))
      dlsym (handle, "memkind_check_available");
  if (data->memkind_malloc
      && data->memkind_calloc
      && data->memkind_realloc
      && data->memkind_free
      && data->memkind_check_available)
    for (i = 1; i < GOMP_MEMKIND_COUNT; ++i)
      {
	data->kinds[i] = (void **) dlsym (handle, kinds[i]);
	if (data->kinds[i] && data->memkind_check_available (*data->kinds[i]))
	  data->kinds[i] = NULL;
      }
  __atomic_store_n (&memkind_data, data, MEMMODEL_RELEASE);
}

static struct gomp_memkind_data *
gomp_get_memkind (void)
{
  struct gomp_memkind_data *data
    = __atomic_load_n (&memkind_data, MEMMODEL_ACQUIRE);
  if (data)
    return data;
  pthread_once (&memkind_data_once, gomp_init_memkind);
  return __atomic_load_n (&memkind_data, MEMMODEL_ACQUIRE);
}
#endif

omp_allocator_handle_t
omp_init_allocator (omp_memspace_handle_t memspace, int ntraits,
		    const omp_alloctrait_t traits[])
{
  struct omp_allocator_data data
    = { memspace, 1, ~(uintptr_t) 0, 0, 0, omp_atv_contended, omp_atv_all,
	omp_atv_default_mem_fb, omp_atv_false, omp_atv_environment,
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
	GOMP_MEMKIND_NONE
#endif
      };
  struct omp_allocator_data *ret;
  int i;

  if (memspace > omp_low_lat_mem_space)
    return omp_null_allocator;
  for (i = 0; i < ntraits; i++)
    switch (traits[i].key)
      {
      case omp_atk_sync_hint:
	switch (traits[i].value)
	  {
	  case omp_atv_default:
	    data.sync_hint = omp_atv_contended;
	    break;
	  case omp_atv_contended:
	  case omp_atv_uncontended:
	  case omp_atv_serialized:
	  case omp_atv_private:
	    data.sync_hint = traits[i].value;
	    break;
	  default:
	    return omp_null_allocator;
	  }
	break;
      case omp_atk_alignment:
        if (traits[i].value == omp_atv_default)
	  {
	    data.alignment = 1;
	    break;
	  }
	if ((traits[i].value & (traits[i].value - 1)) != 0
	    || !traits[i].value)
	  return omp_null_allocator;
	data.alignment = traits[i].value;
	break;
      case omp_atk_access:
	switch (traits[i].value)
	  {
	  case omp_atv_default:
	    data.access = omp_atv_all;
	    break;
	  case omp_atv_all:
	  case omp_atv_cgroup:
	  case omp_atv_pteam:
	  case omp_atv_thread:
	    data.access = traits[i].value;
	    break;
	  default:
	    return omp_null_allocator;
	  }
	break;
      case omp_atk_pool_size:
	if (traits[i].value == omp_atv_default)
	  data.pool_size = ~(uintptr_t) 0;
	else
	  data.pool_size = traits[i].value;
	break;
      case omp_atk_fallback:
	switch (traits[i].value)
	  {
	  case omp_atv_default:
	    data.fallback = omp_atv_default_mem_fb;
	    break;
	  case omp_atv_default_mem_fb:
	  case omp_atv_null_fb:
	  case omp_atv_abort_fb:
	  case omp_atv_allocator_fb:
	    data.fallback = traits[i].value;
	    break;
	  default:
	    return omp_null_allocator;
	  }
	break;
      case omp_atk_fb_data:
	data.fb_data = traits[i].value;
	break;
      case omp_atk_pinned:
	switch (traits[i].value)
	  {
	  case omp_atv_default:
	  case omp_atv_false:
	    data.pinned = omp_atv_false;
	    break;
	  case omp_atv_true:
	    data.pinned = omp_atv_true;
	    break;
	  default:
	    return omp_null_allocator;
	  }
	break;
      case omp_atk_partition:
	switch (traits[i].value)
	  {
	  case omp_atv_default:
	    data.partition = omp_atv_environment;
	    break;
	  case omp_atv_environment:
	  case omp_atv_nearest:
	  case omp_atv_blocked:
	  case omp_atv_interleaved:
	    data.partition = traits[i].value;
	    break;
	  default:
	    return omp_null_allocator;
	  }
	break;
      default:
	return omp_null_allocator;
      }

  if (data.alignment < sizeof (void *))
    data.alignment = sizeof (void *);

  switch (memspace)
    {
#ifdef LIBGOMP_USE_MEMKIND
    case omp_high_bw_mem_space:
      struct gomp_memkind_data *memkind_data;
      memkind_data = gomp_get_memkind ();
      if (data.partition == omp_atv_interleaved
	  && memkind_data->kinds[GOMP_MEMKIND_HBW_INTERLEAVE])
	{
	  data.memkind = GOMP_MEMKIND_HBW_INTERLEAVE;
	  break;
	}
      else if (memkind_data->kinds[GOMP_MEMKIND_HBW_PREFERRED])
	{
	  data.memkind = GOMP_MEMKIND_HBW_PREFERRED;
	  break;
	}
      break;
    case omp_large_cap_mem_space:
      memkind_data = gomp_get_memkind ();
      if (memkind_data->kinds[GOMP_MEMKIND_DAX_KMEM_ALL])
	data.memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      else if (memkind_data->kinds[GOMP_MEMKIND_DAX_KMEM])
	data.memkind = GOMP_MEMKIND_DAX_KMEM;
      break;
#endif
    default:
#ifdef LIBGOMP_USE_MEMKIND
      if (data.partition == omp_atv_interleaved)
	{
	  memkind_data = gomp_get_memkind ();
	  if (memkind_data->kinds[GOMP_MEMKIND_INTERLEAVE])
	    data.memkind = GOMP_MEMKIND_INTERLEAVE;
	}
#endif
      break;
    }

#ifdef LIBGOMP_USE_LIBNUMA
  if (data.memkind == GOMP_MEMKIND_NONE && data.partition == omp_atv_nearest)
    {
      libnuma_data = gomp_get_libnuma ();
      if (libnuma_data->numa_alloc_local != NULL)
	data.memkind = GOMP_MEMKIND_LIBNUMA;
    }
#endif

  /* Reject unsupported memory spaces.  */
  if (!MEMSPACE_VALIDATE (data.memspace, data.access, data.pinned))
    return omp_null_allocator;

  ret = gomp_malloc (sizeof (struct omp_allocator_data));
  *ret = data;
#ifndef HAVE_SYNC_BUILTINS
  gomp_mutex_init (&ret->lock);
#endif
  return (omp_allocator_handle_t) ret;
}

void
omp_destroy_allocator (omp_allocator_handle_t allocator)
{
  if (allocator != omp_null_allocator)
    {
#ifndef HAVE_SYNC_BUILTINS
      gomp_mutex_destroy (&((struct omp_allocator_data *) allocator)->lock);
#endif
      free ((void *) allocator);
    }
}

ialias (omp_init_allocator)
ialias (omp_destroy_allocator)

void *
omp_aligned_alloc (size_t alignment, size_t size,
		   omp_allocator_handle_t allocator)
{
  struct omp_allocator_data *allocator_data;
  size_t new_size, new_alignment;
  void *ptr, *ret;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
  enum gomp_numa_memkind_kind memkind;
#endif

  if (__builtin_expect (size == 0, 0))
    return NULL;

retry:
  new_alignment = alignment;
  if (allocator == omp_null_allocator)
    {
      struct gomp_thread *thr = gomp_thread ();
      if (thr->ts.def_allocator == omp_null_allocator)
	thr->ts.def_allocator = gomp_def_allocator;
      allocator = (omp_allocator_handle_t) thr->ts.def_allocator;
    }

  if (allocator > omp_max_predefined_alloc)
    {
      allocator_data = (struct omp_allocator_data *) allocator;
      if (new_alignment < allocator_data->alignment)
	new_alignment = allocator_data->alignment;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = allocator_data->memkind;
#endif
    }
  else
    {
      allocator_data = NULL;
      if (new_alignment < sizeof (void *))
	new_alignment = sizeof (void *);
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = GOMP_MEMKIND_NONE;
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (allocator == omp_high_bw_mem_alloc)
	memkind = GOMP_MEMKIND_HBW_PREFERRED;
      else if (allocator == omp_large_cap_mem_alloc)
	memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  if (!memkind_data->kinds[memkind])
	    memkind = GOMP_MEMKIND_NONE;
	}
#endif
    }

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_add_overflow (size, new_size, &new_size))
    goto fail;
#ifdef OMP_LOW_LAT_MEM_ALLOC_INVALID
  if (allocator == omp_low_lat_mem_alloc)
    goto fail;
#endif

  if (__builtin_expect (allocator_data
			&& allocator_data->pool_size < ~(uintptr_t) 0, 0))
    {
      uintptr_t used_pool_size;
      if (new_size > allocator_data->pool_size)
	goto fail;
#ifdef HAVE_SYNC_BUILTINS
      used_pool_size = __atomic_load_n (&allocator_data->used_pool_size,
					MEMMODEL_RELAXED);
      do
	{
	  uintptr_t new_pool_size;
	  if (__builtin_add_overflow (used_pool_size, new_size,
				      &new_pool_size)
	      || new_pool_size > allocator_data->pool_size)
	    goto fail;
	  if (__atomic_compare_exchange_n (&allocator_data->used_pool_size,
					   &used_pool_size, new_pool_size,
					   true, MEMMODEL_RELAXED,
					   MEMMODEL_RELAXED))
	    break;
	}
      while (1);
#else
      gomp_mutex_lock (&allocator_data->lock);
      if (__builtin_add_overflow (allocator_data->used_pool_size, new_size,
				  &used_pool_size)
	  || used_pool_size > allocator_data->pool_size)
	{
	  gomp_mutex_unlock (&allocator_data->lock);
	  goto fail;
	}
      allocator_data->used_pool_size = used_pool_size;
      gomp_mutex_unlock (&allocator_data->lock);
#endif
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	ptr = libnuma_data->numa_alloc_local (new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  ptr = memkind_data->memkind_malloc (kind, new_size);
	}
      else
#endif
	ptr = MEMSPACE_ALLOC (allocator_data->memspace, new_size,
			      allocator_data->pinned);
      if (ptr == NULL)
	{
#ifdef HAVE_SYNC_BUILTINS
	  __atomic_add_fetch (&allocator_data->used_pool_size, -new_size,
			      MEMMODEL_RELAXED);
#else
	  gomp_mutex_lock (&allocator_data->lock);
	  allocator_data->used_pool_size -= new_size;
	  gomp_mutex_unlock (&allocator_data->lock);
#endif
	  goto fail;
	}
    }
  else
    {
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	ptr = libnuma_data->numa_alloc_local (new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  ptr = memkind_data->memkind_malloc (kind, new_size);
	}
      else
#endif
	{
	  omp_memspace_handle_t memspace;
	  memspace = (allocator_data
		      ? allocator_data->memspace
		      : predefined_alloc_mapping[allocator]);
	  ptr = MEMSPACE_ALLOC (memspace, new_size,
				allocator_data && allocator_data->pinned);
	}
      if (ptr == NULL)
	goto fail;
    }

  if (new_alignment > sizeof (void *))
    ret = (void *) (((uintptr_t) ptr
		     + sizeof (struct omp_mem_header)
		     + new_alignment - sizeof (void *))
		    & ~(new_alignment - 1));
  else
    ret = (char *) ptr + sizeof (struct omp_mem_header);
  ((struct omp_mem_header *) ret)[-1].ptr = ptr;
  ((struct omp_mem_header *) ret)[-1].size = new_size;
  ((struct omp_mem_header *) ret)[-1].allocator = allocator;
  return ret;

fail:;
  int fallback = (allocator_data
		  ? allocator_data->fallback
		  : allocator == omp_default_mem_alloc
		  ? omp_atv_null_fb
		  : omp_atv_default_mem_fb);
  switch (fallback)
    {
    case omp_atv_default_mem_fb:
      allocator = omp_default_mem_alloc;
      goto retry;
    case omp_atv_null_fb:
      break;
    default:
    case omp_atv_abort_fb:
      gomp_fatal ("Out of memory allocating %lu bytes",
		  (unsigned long) size);
    case omp_atv_allocator_fb:
      allocator = allocator_data->fb_data;
      goto retry;
    }
  return NULL;
}

ialias (omp_aligned_alloc)

void *
omp_alloc (size_t size, omp_allocator_handle_t allocator)
{
  return ialias_call (omp_aligned_alloc) (1, size, allocator);
}

/* Like omp_aligned_alloc, but apply on top of that:
   "For allocations that arise from this ... the null_fb value of the
   fallback allocator trait behaves as if the abort_fb had been specified."  */

void *
GOMP_alloc (size_t alignment, size_t size, uintptr_t allocator)
{
  void *ret
    = ialias_call (omp_aligned_alloc) (alignment, size,
				       (omp_allocator_handle_t) allocator);
  if (__builtin_expect (ret == NULL, 0) && size)
    gomp_fatal ("Out of memory allocating %lu bytes",
		(unsigned long) size);
  return ret;
}

void
omp_free (void *ptr, omp_allocator_handle_t allocator)
{
  struct omp_mem_header *data;
  omp_memspace_handle_t memspace = omp_default_mem_space;
  int pinned = false;

  if (ptr == NULL)
    return;
  (void) allocator;
  data = &((struct omp_mem_header *) ptr)[-1];
  if (data->allocator > omp_max_predefined_alloc)
    {
      struct omp_allocator_data *allocator_data
	= (struct omp_allocator_data *) (data->allocator);
      if (allocator_data->pool_size < ~(uintptr_t) 0)
	{
#ifdef HAVE_SYNC_BUILTINS
	  __atomic_add_fetch (&allocator_data->used_pool_size, -data->size,
			      MEMMODEL_RELAXED);
#else
	  gomp_mutex_lock (&allocator_data->lock);
	  allocator_data->used_pool_size -= data->size;
	  gomp_mutex_unlock (&allocator_data->lock);
#endif
	}
#ifdef LIBGOMP_USE_LIBNUMA
      if (allocator_data->memkind == GOMP_MEMKIND_LIBNUMA)
	{
	  libnuma_data->numa_free (data->ptr, data->size);
	  return;
	}
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (allocator_data->memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[allocator_data->memkind];
	  memkind_data->memkind_free (kind, data->ptr);
	  return;
	}
#endif

      memspace = allocator_data->memspace;
      pinned = allocator_data->pinned;
    }
  else
    {
#ifdef LIBGOMP_USE_MEMKIND
      enum gomp_numa_memkind_kind memkind = GOMP_MEMKIND_NONE;
      if (data->allocator == omp_high_bw_mem_alloc)
	memkind = GOMP_MEMKIND_HBW_PREFERRED;
      else if (data->allocator == omp_large_cap_mem_alloc)
	memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  if (memkind_data->kinds[memkind])
	    {
	      void *kind = *memkind_data->kinds[memkind];
	      memkind_data->memkind_free (kind, data->ptr);
	      return;
	    }
	}
#endif

      memspace = predefined_alloc_mapping[data->allocator];
    }

  MEMSPACE_FREE (memspace, data->ptr, data->size, pinned);
}

ialias (omp_free)

void
GOMP_free (void *ptr, uintptr_t allocator)
{
  return ialias_call (omp_free) (ptr, (omp_allocator_handle_t) allocator);
}

void *
omp_aligned_calloc (size_t alignment, size_t nmemb, size_t size,
		    omp_allocator_handle_t allocator)
{
  struct omp_allocator_data *allocator_data;
  size_t new_size, size_temp, new_alignment;
  void *ptr, *ret;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
  enum gomp_numa_memkind_kind memkind;
#endif

  if (__builtin_expect (size == 0 || nmemb == 0, 0))
    return NULL;

retry:
  new_alignment = alignment;
  if (allocator == omp_null_allocator)
    {
      struct gomp_thread *thr = gomp_thread ();
      if (thr->ts.def_allocator == omp_null_allocator)
	thr->ts.def_allocator = gomp_def_allocator;
      allocator = (omp_allocator_handle_t) thr->ts.def_allocator;
    }

  if (allocator > omp_max_predefined_alloc)
    {
      allocator_data = (struct omp_allocator_data *) allocator;
      if (new_alignment < allocator_data->alignment)
	new_alignment = allocator_data->alignment;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = allocator_data->memkind;
#endif
    }
  else
    {
      allocator_data = NULL;
      if (new_alignment < sizeof (void *))
	new_alignment = sizeof (void *);
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = GOMP_MEMKIND_NONE;
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (allocator == omp_high_bw_mem_alloc)
	memkind = GOMP_MEMKIND_HBW_PREFERRED;
      else if (allocator == omp_large_cap_mem_alloc)
	memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  if (!memkind_data->kinds[memkind])
	    memkind = GOMP_MEMKIND_NONE;
	}
#endif
    }

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_mul_overflow (size, nmemb, &size_temp))
    goto fail;
  if (__builtin_add_overflow (size_temp, new_size, &new_size))
    goto fail;
#ifdef OMP_LOW_LAT_MEM_ALLOC_INVALID
  if (allocator == omp_low_lat_mem_alloc)
    goto fail;
#endif

  if (__builtin_expect (allocator_data
			&& allocator_data->pool_size < ~(uintptr_t) 0, 0))
    {
      uintptr_t used_pool_size;
      if (new_size > allocator_data->pool_size)
	goto fail;
#ifdef HAVE_SYNC_BUILTINS
      used_pool_size = __atomic_load_n (&allocator_data->used_pool_size,
					MEMMODEL_RELAXED);
      do
	{
	  uintptr_t new_pool_size;
	  if (__builtin_add_overflow (used_pool_size, new_size,
				      &new_pool_size)
	      || new_pool_size > allocator_data->pool_size)
	    goto fail;
	  if (__atomic_compare_exchange_n (&allocator_data->used_pool_size,
					   &used_pool_size, new_pool_size,
					   true, MEMMODEL_RELAXED,
					   MEMMODEL_RELAXED))
	    break;
	}
      while (1);
#else
      gomp_mutex_lock (&allocator_data->lock);
      if (__builtin_add_overflow (allocator_data->used_pool_size, new_size,
				  &used_pool_size)
	  || used_pool_size > allocator_data->pool_size)
	{
	  gomp_mutex_unlock (&allocator_data->lock);
	  goto fail;
	}
      allocator_data->used_pool_size = used_pool_size;
      gomp_mutex_unlock (&allocator_data->lock);
#endif
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	/* numa_alloc_local uses mmap with MAP_ANONYMOUS, returning
	   memory that is initialized to zero.  */
	ptr = libnuma_data->numa_alloc_local (new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  ptr = memkind_data->memkind_calloc (kind, 1, new_size);
	}
      else
#endif
	ptr = MEMSPACE_CALLOC (allocator_data->memspace, new_size,
			       allocator_data->pinned);
      if (ptr == NULL)
	{
#ifdef HAVE_SYNC_BUILTINS
	  __atomic_add_fetch (&allocator_data->used_pool_size, -new_size,
			      MEMMODEL_RELAXED);
#else
	  gomp_mutex_lock (&allocator_data->lock);
	  allocator_data->used_pool_size -= new_size;
	  gomp_mutex_unlock (&allocator_data->lock);
#endif
	  goto fail;
	}
    }
  else
    {
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	/* numa_alloc_local uses mmap with MAP_ANONYMOUS, returning
	   memory that is initialized to zero.  */
	ptr = libnuma_data->numa_alloc_local (new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  ptr = memkind_data->memkind_calloc (kind, 1, new_size);
	}
      else
#endif
	{
	  omp_memspace_handle_t memspace;
	  memspace = (allocator_data
		      ? allocator_data->memspace
		      : predefined_alloc_mapping[allocator]);
	  ptr = MEMSPACE_CALLOC (memspace, new_size,
				 allocator_data && allocator_data->pinned);
	}
      if (ptr == NULL)
	goto fail;
    }

  if (new_alignment > sizeof (void *))
    ret = (void *) (((uintptr_t) ptr
		     + sizeof (struct omp_mem_header)
		     + new_alignment - sizeof (void *))
		    & ~(new_alignment - 1));
  else
    ret = (char *) ptr + sizeof (struct omp_mem_header);
  ((struct omp_mem_header *) ret)[-1].ptr = ptr;
  ((struct omp_mem_header *) ret)[-1].size = new_size;
  ((struct omp_mem_header *) ret)[-1].allocator = allocator;
  return ret;

fail:;
  int fallback = (allocator_data
		  ? allocator_data->fallback
		  : allocator == omp_default_mem_alloc
		  ? omp_atv_null_fb
		  : omp_atv_default_mem_fb);
  switch (fallback)
    {
    case omp_atv_default_mem_fb:
      allocator = omp_default_mem_alloc;
      goto retry;
    case omp_atv_null_fb:
      break;
    default:
    case omp_atv_abort_fb:
      gomp_fatal ("Out of memory allocating %lu bytes",
		  (unsigned long) (size * nmemb));
    case omp_atv_allocator_fb:
      allocator = allocator_data->fb_data;
      goto retry;
    }
  return NULL;
}

ialias (omp_aligned_calloc)

void *
omp_calloc (size_t nmemb, size_t size, omp_allocator_handle_t allocator)
{
  return ialias_call (omp_aligned_calloc) (1, nmemb, size, allocator);
}

void *
omp_realloc (void *ptr, size_t size, omp_allocator_handle_t allocator,
	     omp_allocator_handle_t free_allocator)
{
  struct omp_allocator_data *allocator_data, *free_allocator_data;
  size_t new_size, old_size, new_alignment, old_alignment;
  void *new_ptr, *ret;
  struct omp_mem_header *data;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
  enum gomp_numa_memkind_kind memkind, free_memkind;
#endif

  if (__builtin_expect (ptr == NULL, 0))
    return ialias_call (omp_aligned_alloc) (1, size, allocator);

  if (__builtin_expect (size == 0, 0))
    {
      ialias_call (omp_free) (ptr, free_allocator);
      return NULL;
    }

  data = &((struct omp_mem_header *) ptr)[-1];
  free_allocator = data->allocator;

retry:
  new_alignment = sizeof (void *);
  if (allocator == omp_null_allocator)
    allocator = free_allocator;

  if (allocator > omp_max_predefined_alloc)
    {
      allocator_data = (struct omp_allocator_data *) allocator;
      if (new_alignment < allocator_data->alignment)
	new_alignment = allocator_data->alignment;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = allocator_data->memkind;
#endif
    }
  else
    {
      allocator_data = NULL;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      memkind = GOMP_MEMKIND_NONE;
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (allocator == omp_high_bw_mem_alloc)
	memkind = GOMP_MEMKIND_HBW_PREFERRED;
      else if (allocator == omp_large_cap_mem_alloc)
	memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  if (!memkind_data->kinds[memkind])
	    memkind = GOMP_MEMKIND_NONE;
	}
#endif
    }
  if (free_allocator > omp_max_predefined_alloc)
    {
      free_allocator_data = (struct omp_allocator_data *) free_allocator;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      free_memkind = free_allocator_data->memkind;
#endif
    }
  else
    {
      free_allocator_data = NULL;
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
      free_memkind = GOMP_MEMKIND_NONE;
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (free_allocator == omp_high_bw_mem_alloc)
	free_memkind = GOMP_MEMKIND_HBW_PREFERRED;
      else if (free_allocator == omp_large_cap_mem_alloc)
	free_memkind = GOMP_MEMKIND_DAX_KMEM_ALL;
      if (free_memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  if (!memkind_data->kinds[free_memkind])
	    free_memkind = GOMP_MEMKIND_NONE;
	}
#endif
    }
  old_alignment = (uintptr_t) ptr - (uintptr_t) (data->ptr);

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_add_overflow (size, new_size, &new_size))
    goto fail;
  old_size = data->size;
#ifdef OMP_LOW_LAT_MEM_ALLOC_INVALID
  if (allocator == omp_low_lat_mem_alloc)
    goto fail;
#endif

  if (__builtin_expect (allocator_data
			&& allocator_data->pool_size < ~(uintptr_t) 0, 0))
    {
      uintptr_t used_pool_size;
      size_t prev_size = 0;
      /* Check if we can use realloc.  Don't use it if extra alignment
	 was used previously or newly, because realloc might return a pointer
	 with different alignment and then we'd need to memmove the data
	 again.  */
      if (free_allocator_data
	  && free_allocator_data == allocator_data
	  && new_alignment == sizeof (void *)
	  && old_alignment == sizeof (struct omp_mem_header))
	prev_size = old_size;
      if (new_size > prev_size
	  && new_size - prev_size > allocator_data->pool_size)
	goto fail;
#ifdef HAVE_SYNC_BUILTINS
      used_pool_size = __atomic_load_n (&allocator_data->used_pool_size,
					MEMMODEL_RELAXED);
      do
	{
	  uintptr_t new_pool_size;
	  if (new_size > prev_size)
	    {
	      if (__builtin_add_overflow (used_pool_size, new_size - prev_size,
					  &new_pool_size)
		  || new_pool_size > allocator_data->pool_size)
		goto fail;
	    }
	  else
	    new_pool_size = used_pool_size + new_size - prev_size;
	  if (__atomic_compare_exchange_n (&allocator_data->used_pool_size,
					   &used_pool_size, new_pool_size,
					   true, MEMMODEL_RELAXED,
					   MEMMODEL_RELAXED))
	    break;
	}
      while (1);
#else
      gomp_mutex_lock (&allocator_data->lock);
      if (new_size > prev_size)
	{
	  if (__builtin_add_overflow (allocator_data->used_pool_size,
				      new_size - prev_size,
				      &used_pool_size)
	      || used_pool_size > allocator_data->pool_size)
	    {
	      gomp_mutex_unlock (&allocator_data->lock);
	      goto fail;
	    }
	}
      else
	used_pool_size = (allocator_data->used_pool_size
			  + new_size - prev_size);
      allocator_data->used_pool_size = used_pool_size;
      gomp_mutex_unlock (&allocator_data->lock);
#endif
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	{
	  if (prev_size)
	    new_ptr = libnuma_data->numa_realloc (data->ptr, data->size,
						  new_size);
	  else
	    new_ptr = libnuma_data->numa_alloc_local (new_size);
	}
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  if (prev_size)
	    new_ptr = memkind_data->memkind_realloc (kind, data->ptr,
						     new_size);
	  else
	    new_ptr = memkind_data->memkind_malloc (kind, new_size);
	}
      else
#endif
      if (prev_size)
	new_ptr = MEMSPACE_REALLOC (allocator_data->memspace, data->ptr,
				    data->size, new_size,
				    (free_allocator_data
				     && free_allocator_data->pinned),
				    allocator_data->pinned);
      else
	new_ptr = MEMSPACE_ALLOC (allocator_data->memspace, new_size,
				  allocator_data->pinned);
      if (new_ptr == NULL)
	{
#ifdef HAVE_SYNC_BUILTINS
	  __atomic_add_fetch (&allocator_data->used_pool_size,
			      prev_size - new_size,
			      MEMMODEL_RELAXED);
#else
	  gomp_mutex_lock (&allocator_data->lock);
	  allocator_data->used_pool_size -= new_size - prev_size;
	  gomp_mutex_unlock (&allocator_data->lock);
#endif
	  goto fail;
	}
      else if (prev_size)
	{
	  ret = (char *) new_ptr + sizeof (struct omp_mem_header);
	  ((struct omp_mem_header *) ret)[-1].ptr = new_ptr;
	  ((struct omp_mem_header *) ret)[-1].size = new_size;
	  ((struct omp_mem_header *) ret)[-1].allocator = allocator;
	  return ret;
	}
    }
  else if (new_alignment == sizeof (void *)
	   && old_alignment == sizeof (struct omp_mem_header)
#if defined(LIBGOMP_USE_MEMKIND) || defined(LIBGOMP_USE_LIBNUMA)
	   && memkind == free_memkind
#endif
	   && (free_allocator_data == NULL
	       || free_allocator_data->pool_size == ~(uintptr_t) 0))
    {
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	new_ptr = libnuma_data->numa_realloc (data->ptr, data->size, new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  new_ptr = memkind_data->memkind_realloc (kind, data->ptr,
						   new_size);
	}
      else
#endif
	{
	  omp_memspace_handle_t memspace;
	  memspace = (allocator_data
		      ? allocator_data->memspace
		      : predefined_alloc_mapping[allocator]);
	  new_ptr = MEMSPACE_REALLOC (memspace, data->ptr, data->size, new_size,
				      (free_allocator_data
				       && free_allocator_data->pinned),
				      allocator_data && allocator_data->pinned);
	}
      if (new_ptr == NULL)
	goto fail;

      ret = (char *) new_ptr + sizeof (struct omp_mem_header);
      ((struct omp_mem_header *) ret)[-1].ptr = new_ptr;
      ((struct omp_mem_header *) ret)[-1].size = new_size;
      ((struct omp_mem_header *) ret)[-1].allocator = allocator;
      return ret;
    }
  else
    {
#ifdef LIBGOMP_USE_LIBNUMA
      if (memkind == GOMP_MEMKIND_LIBNUMA)
	new_ptr = libnuma_data->numa_alloc_local (new_size);
# ifdef LIBGOMP_USE_MEMKIND
      else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
      if (memkind)
	{
	  struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
	  void *kind = *memkind_data->kinds[memkind];
	  new_ptr = memkind_data->memkind_malloc (kind, new_size);
	}
      else
#endif
	{
	  omp_memspace_handle_t memspace;
	  memspace = (allocator_data
		      ? allocator_data->memspace
		      : predefined_alloc_mapping[allocator]);
	  new_ptr = MEMSPACE_ALLOC (memspace, new_size,
				    allocator_data && allocator_data->pinned);
	}
      if (new_ptr == NULL)
	goto fail;
    }

  if (new_alignment > sizeof (void *))
    ret = (void *) (((uintptr_t) new_ptr
		     + sizeof (struct omp_mem_header)
		     + new_alignment - sizeof (void *))
		    & ~(new_alignment - 1));
  else
    ret = (char *) new_ptr + sizeof (struct omp_mem_header);
  ((struct omp_mem_header *) ret)[-1].ptr = new_ptr;
  ((struct omp_mem_header *) ret)[-1].size = new_size;
  ((struct omp_mem_header *) ret)[-1].allocator = allocator;
  if (old_size - old_alignment < size)
    size = old_size - old_alignment;
  memcpy (ret, ptr, size);
  if (__builtin_expect (free_allocator_data
			&& free_allocator_data->pool_size < ~(uintptr_t) 0, 0))
    {
#ifdef HAVE_SYNC_BUILTINS
      __atomic_add_fetch (&free_allocator_data->used_pool_size, -data->size,
			  MEMMODEL_RELAXED);
#else
      gomp_mutex_lock (&free_allocator_data->lock);
      free_allocator_data->used_pool_size -= data->size;
      gomp_mutex_unlock (&free_allocator_data->lock);
#endif
    }
#ifdef LIBGOMP_USE_LIBNUMA
  if (free_memkind == GOMP_MEMKIND_LIBNUMA)
    {
      libnuma_data->numa_free (data->ptr, data->size);
      return ret;
    }
# ifdef LIBGOMP_USE_MEMKIND
  else
# endif
#endif
#ifdef LIBGOMP_USE_MEMKIND
  if (free_memkind)
    {
      struct gomp_memkind_data *memkind_data = gomp_get_memkind ();
      void *kind = *memkind_data->kinds[free_memkind];
      memkind_data->memkind_free (kind, data->ptr);
      return ret;
    }
#endif
  {
    omp_memspace_handle_t was_memspace;
    was_memspace = (free_allocator_data
		    ? free_allocator_data->memspace
		    : predefined_alloc_mapping[free_allocator]);
    int was_pinned = (free_allocator_data && free_allocator_data->pinned);
    MEMSPACE_FREE (was_memspace, data->ptr, data->size, was_pinned);
  }
  return ret;

fail:;
  int fallback = (allocator_data
		  ? allocator_data->fallback
		  : allocator == omp_default_mem_alloc
		  ? omp_atv_null_fb
		  : omp_atv_default_mem_fb);
  switch (fallback)
    {
    case omp_atv_default_mem_fb:
      allocator = omp_default_mem_alloc;
      goto retry;
    case omp_atv_null_fb:
      break;
    default:
    case omp_atv_abort_fb:
      gomp_fatal ("Out of memory allocating %lu bytes",
		  (unsigned long) size);
    case omp_atv_allocator_fb:
      allocator = allocator_data->fb_data;
      goto retry;
    }
  return NULL;
}
