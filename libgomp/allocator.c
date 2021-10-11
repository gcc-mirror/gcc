/* Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#define omp_max_predefined_alloc omp_thread_mem_alloc

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

omp_allocator_handle_t
omp_init_allocator (omp_memspace_handle_t memspace, int ntraits,
		    const omp_alloctrait_t traits[])
{
  struct omp_allocator_data data
    = { memspace, 1, ~(uintptr_t) 0, 0, 0, omp_atv_contended, omp_atv_all,
	omp_atv_default_mem_fb, omp_atv_false, omp_atv_environment };
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

  /* No support for these so far (for hbw will use memkind).  */
  if (data.pinned || data.memspace == omp_high_bw_mem_space)
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
    }
  else
    {
      allocator_data = NULL;
      if (new_alignment < sizeof (void *))
	new_alignment = sizeof (void *);
    }

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_add_overflow (size, new_size, &new_size))
    goto fail;

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
      ptr = malloc (new_size);
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
      ptr = malloc (new_size);
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

fail:
  if (allocator_data)
    {
      switch (allocator_data->fallback)
	{
	case omp_atv_default_mem_fb:
	  if ((new_alignment > sizeof (void *) && new_alignment > alignment)
	      || (allocator_data
		  && allocator_data->pool_size < ~(uintptr_t) 0))
	    {
	      allocator = omp_default_mem_alloc;
	      goto retry;
	    }
	  /* Otherwise, we've already performed default mem allocation
	     and if that failed, it won't succeed again (unless it was
	     intermittent.  Return NULL then, as that is the fallback.  */
	  break;
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
    }
  free (data->ptr);
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
    }
  else
    {
      allocator_data = NULL;
      if (new_alignment < sizeof (void *))
	new_alignment = sizeof (void *);
    }

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_mul_overflow (size, nmemb, &size_temp))
    goto fail;
  if (__builtin_add_overflow (size_temp, new_size, &new_size))
    goto fail;

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
      ptr = calloc (1, new_size);
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
      ptr = calloc (1, new_size);
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

fail:
  if (allocator_data)
    {
      switch (allocator_data->fallback)
	{
	case omp_atv_default_mem_fb:
	  if ((new_alignment > sizeof (void *) && new_alignment > alignment)
	      || (allocator_data
		  && allocator_data->pool_size < ~(uintptr_t) 0))
	    {
	      allocator = omp_default_mem_alloc;
	      goto retry;
	    }
	  /* Otherwise, we've already performed default mem allocation
	     and if that failed, it won't succeed again (unless it was
	     intermittent.  Return NULL then, as that is the fallback.  */
	  break;
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
    }
  else
    allocator_data = NULL;
  if (free_allocator > omp_max_predefined_alloc)
    free_allocator_data = (struct omp_allocator_data *) free_allocator;
  else
    free_allocator_data = NULL;
  old_alignment = (uintptr_t) ptr - (uintptr_t) (data->ptr);

  new_size = sizeof (struct omp_mem_header);
  if (new_alignment > sizeof (void *))
    new_size += new_alignment - sizeof (void *);
  if (__builtin_add_overflow (size, new_size, &new_size))
    goto fail;
  old_size = data->size;

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
      if (prev_size)
	new_ptr = realloc (data->ptr, new_size);
      else
	new_ptr = malloc (new_size);
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
	   && (free_allocator_data == NULL
	       || free_allocator_data->pool_size == ~(uintptr_t) 0))
    {
      new_ptr = realloc (data->ptr, new_size);
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
      new_ptr = malloc (new_size);
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
  free (data->ptr);
  return ret;

fail:
  if (allocator_data)
    {
      switch (allocator_data->fallback)
	{
	case omp_atv_default_mem_fb:
	  if (new_alignment > sizeof (void *)
	      || (allocator_data
		  && allocator_data->pool_size < ~(uintptr_t) 0))
	    {
	      allocator = omp_default_mem_alloc;
	      goto retry;
	    }
	  /* Otherwise, we've already performed default mem allocation
	     and if that failed, it won't succeed again (unless it was
	     intermittent.  Return NULL then, as that is the fallback.  */
	  break;
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
    }
  return NULL;
}
