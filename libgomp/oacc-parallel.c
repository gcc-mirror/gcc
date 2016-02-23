/* Copyright (C) 2013-2016 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

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

/* This file handles OpenACC constructs.  */

#include "openacc.h"
#include "libgomp.h"
#include "libgomp_g.h"
#include "gomp-constants.h"
#include "oacc-int.h"
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIu64.  */
#endif
#include <string.h>
#include <stdarg.h>
#include <assert.h>

static int
find_pset (int pos, size_t mapnum, unsigned short *kinds)
{
  if (pos + 1 >= mapnum)
    return 0;

  unsigned char kind = kinds[pos+1] & 0xff;

  return kind == GOMP_MAP_TO_PSET;
}

static void goacc_wait (int async, int num_waits, va_list *ap);


/* Launch a possibly offloaded function on DEVICE.  FN is the host fn
   address.  MAPNUM, HOSTADDRS, SIZES & KINDS  describe the memory
   blocks to be copied to/from the device.  Varadic arguments are
   keyed optional parameters terminated with a zero.  */

void
GOACC_parallel_keyed (int device, void (*fn) (void *),
		      size_t mapnum, void **hostaddrs, size_t *sizes,
		      unsigned short *kinds, ...)
{
  bool host_fallback = device == GOMP_DEVICE_HOST_FALLBACK;
  va_list ap;
  struct goacc_thread *thr;
  struct gomp_device_descr *acc_dev;
  struct target_mem_desc *tgt;
  void **devaddrs;
  unsigned int i;
  struct splay_tree_key_s k;
  splay_tree_key tgt_fn_key;
  void (*tgt_fn);
  int async = GOMP_ASYNC_SYNC;
  unsigned dims[GOMP_DIM_MAX];
  unsigned tag;

#ifdef HAVE_INTTYPES_H
  gomp_debug (0, "%s: mapnum=%"PRIu64", hostaddrs=%p, size=%p, kinds=%p\n",
	      __FUNCTION__, (uint64_t) mapnum, hostaddrs, sizes, kinds);
#else
  gomp_debug (0, "%s: mapnum=%lu, hostaddrs=%p, sizes=%p, kinds=%p\n",
	      __FUNCTION__, (unsigned long) mapnum, hostaddrs, sizes, kinds);
#endif
  goacc_lazy_initialize ();

  thr = goacc_thread ();
  acc_dev = thr->dev;

  /* Host fallback if "if" clause is false or if the current device is set to
     the host.  */
  if (host_fallback)
    {
      goacc_save_and_set_bind (acc_device_host);
      fn (hostaddrs);
      goacc_restore_bind ();
      return;
    }
  else if (acc_device_type (acc_dev->type) == acc_device_host)
    {
      fn (hostaddrs);
      return;
    }

  /* Default: let the runtime choose.  */
  for (i = 0; i != GOMP_DIM_MAX; i++)
    dims[i] = 0;

  va_start (ap, kinds);
  /* TODO: This will need amending when device_type is implemented.  */
  while ((tag = va_arg (ap, unsigned)) != 0)
    {
      if (GOMP_LAUNCH_DEVICE (tag))
	gomp_fatal ("device_type '%d' offload parameters, libgomp is too old",
		    GOMP_LAUNCH_DEVICE (tag));

      switch (GOMP_LAUNCH_CODE (tag))
	{
	case GOMP_LAUNCH_DIM:
	  {
	    unsigned mask = GOMP_LAUNCH_OP (tag);

	    for (i = 0; i != GOMP_DIM_MAX; i++)
	      if (mask & GOMP_DIM_MASK (i))
		dims[i] = va_arg (ap, unsigned);
	  }
	  break;

	case GOMP_LAUNCH_ASYNC:
	  {
	    /* Small constant values are encoded in the operand.  */
	    async = GOMP_LAUNCH_OP (tag);

	    if (async == GOMP_LAUNCH_OP_MAX)
	      async = va_arg (ap, unsigned);
	    break;
	  }

	case GOMP_LAUNCH_WAIT:
	  {
	    unsigned num_waits = GOMP_LAUNCH_OP (tag);

	    if (num_waits)
	      goacc_wait (async, num_waits, &ap);
	    break;
	  }

	default:
	  gomp_fatal ("unrecognized offload code '%d',"
		      " libgomp is too old", GOMP_LAUNCH_CODE (tag));
	}
    }
  va_end (ap);
  
  acc_dev->openacc.async_set_async_func (async);

  if (!(acc_dev->capabilities & GOMP_OFFLOAD_CAP_NATIVE_EXEC))
    {
      k.host_start = (uintptr_t) fn;
      k.host_end = k.host_start + 1;
      gomp_mutex_lock (&acc_dev->lock);
      tgt_fn_key = splay_tree_lookup (&acc_dev->mem_map, &k);
      gomp_mutex_unlock (&acc_dev->lock);

      if (tgt_fn_key == NULL)
	gomp_fatal ("target function wasn't mapped");

      tgt_fn = (void (*)) tgt_fn_key->tgt_offset;
    }
  else
    tgt_fn = (void (*)) fn;

  tgt = gomp_map_vars (acc_dev, mapnum, hostaddrs, NULL, sizes, kinds, true,
		       GOMP_MAP_VARS_OPENACC);

  devaddrs = gomp_alloca (sizeof (void *) * mapnum);
  for (i = 0; i < mapnum; i++)
    devaddrs[i] = (void *) (tgt->list[i].key->tgt->tgt_start
			    + tgt->list[i].key->tgt_offset);

  acc_dev->openacc.exec_func (tgt_fn, mapnum, hostaddrs, devaddrs,
			      async, dims, tgt);

  /* If running synchronously, unmap immediately.  */
  if (async < acc_async_noval)
    gomp_unmap_vars (tgt, true);
  else
    {
      gomp_copy_from_async (tgt);
      acc_dev->openacc.register_async_cleanup_func (tgt);
    }

  acc_dev->openacc.async_set_async_func (acc_async_sync);
}

/* Legacy entry point, only provide host execution.  */

void
GOACC_parallel (int device, void (*fn) (void *),
		size_t mapnum, void **hostaddrs, size_t *sizes,
		unsigned short *kinds,
		int num_gangs, int num_workers, int vector_length,
		int async, int num_waits, ...)
{
  goacc_save_and_set_bind (acc_device_host);
  fn (hostaddrs);
  goacc_restore_bind ();
}

void
GOACC_data_start (int device, size_t mapnum,
		  void **hostaddrs, size_t *sizes, unsigned short *kinds)
{
  bool host_fallback = device == GOMP_DEVICE_HOST_FALLBACK;
  struct target_mem_desc *tgt;

#ifdef HAVE_INTTYPES_H
  gomp_debug (0, "%s: mapnum=%"PRIu64", hostaddrs=%p, size=%p, kinds=%p\n",
	      __FUNCTION__, (uint64_t) mapnum, hostaddrs, sizes, kinds);
#else
  gomp_debug (0, "%s: mapnum=%lu, hostaddrs=%p, sizes=%p, kinds=%p\n",
	      __FUNCTION__, (unsigned long) mapnum, hostaddrs, sizes, kinds);
#endif

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  /* Host fallback or 'do nothing'.  */
  if ((acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
      || host_fallback)
    {
      tgt = gomp_map_vars (NULL, 0, NULL, NULL, NULL, NULL, true,
			   GOMP_MAP_VARS_OPENACC);
      tgt->prev = thr->mapped_data;
      thr->mapped_data = tgt;

      return;
    }

  gomp_debug (0, "  %s: prepare mappings\n", __FUNCTION__);
  tgt = gomp_map_vars (acc_dev, mapnum, hostaddrs, NULL, sizes, kinds, true,
		       GOMP_MAP_VARS_OPENACC);
  gomp_debug (0, "  %s: mappings prepared\n", __FUNCTION__);
  tgt->prev = thr->mapped_data;
  thr->mapped_data = tgt;
}

void
GOACC_data_end (void)
{
  struct goacc_thread *thr = goacc_thread ();
  struct target_mem_desc *tgt = thr->mapped_data;

  gomp_debug (0, "  %s: restore mappings\n", __FUNCTION__);
  thr->mapped_data = tgt->prev;
  gomp_unmap_vars (tgt, true);
  gomp_debug (0, "  %s: mappings restored\n", __FUNCTION__);
}

void
GOACC_enter_exit_data (int device, size_t mapnum,
		       void **hostaddrs, size_t *sizes, unsigned short *kinds,
		       int async, int num_waits, ...)
{
  struct goacc_thread *thr;
  struct gomp_device_descr *acc_dev;
  bool host_fallback = device == GOMP_DEVICE_HOST_FALLBACK;
  bool data_enter = false;
  size_t i;

  goacc_lazy_initialize ();

  thr = goacc_thread ();
  acc_dev = thr->dev;

  if ((acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
      || host_fallback)
    return;

  if (num_waits)
    {
      va_list ap;

      va_start (ap, num_waits);
      goacc_wait (async, num_waits, &ap);
      va_end (ap);
    }

  acc_dev->openacc.async_set_async_func (async);

  /* Determine if this is an "acc enter data".  */
  for (i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;

      if (kind == GOMP_MAP_POINTER || kind == GOMP_MAP_TO_PSET)
	continue;

      if (kind == GOMP_MAP_FORCE_ALLOC
	  || kind == GOMP_MAP_FORCE_PRESENT
	  || kind == GOMP_MAP_FORCE_TO)
	{
	  data_enter = true;
	  break;
	}

      if (kind == GOMP_MAP_FORCE_DEALLOC
	  || kind == GOMP_MAP_FORCE_FROM)
	break;

      gomp_fatal (">>>> GOACC_enter_exit_data UNHANDLED kind 0x%.2x",
		      kind);
    }

  if (data_enter)
    {
      for (i = 0; i < mapnum; i++)
	{
	  unsigned char kind = kinds[i] & 0xff;

	  /* Scan for PSETs.  */
	  int psets = find_pset (i, mapnum, kinds);

	  if (!psets)
	    {
	      switch (kind)
		{
		case GOMP_MAP_POINTER:
		  gomp_acc_insert_pointer (1, &hostaddrs[i], &sizes[i],
					&kinds[i]);
		  break;
		case GOMP_MAP_FORCE_ALLOC:
		  acc_create (hostaddrs[i], sizes[i]);
		  break;
		case GOMP_MAP_FORCE_PRESENT:
		  acc_present_or_copyin (hostaddrs[i], sizes[i]);
		  break;
		case GOMP_MAP_FORCE_TO:
		  acc_present_or_copyin (hostaddrs[i], sizes[i]);
		  break;
		default:
		  gomp_fatal (">>>> GOACC_enter_exit_data UNHANDLED kind 0x%.2x",
			      kind);
		  break;
		}
	    }
	  else
	    {
	      gomp_acc_insert_pointer (3, &hostaddrs[i], &sizes[i], &kinds[i]);
	      /* Increment 'i' by two because OpenACC requires fortran
		 arrays to be contiguous, so each PSET is associated with
		 one of MAP_FORCE_ALLOC/MAP_FORCE_PRESET/MAP_FORCE_TO, and
		 one MAP_POINTER.  */
	      i += 2;
	    }
	}
    }
  else
    for (i = 0; i < mapnum; ++i)
      {
	unsigned char kind = kinds[i] & 0xff;

	int psets = find_pset (i, mapnum, kinds);

	if (!psets)
	  {
	    switch (kind)
	      {
	      case GOMP_MAP_POINTER:
		gomp_acc_remove_pointer (hostaddrs[i], (kinds[i] & 0xff)
					 == GOMP_MAP_FORCE_FROM,
					 async, 1);
		break;
	      case GOMP_MAP_FORCE_DEALLOC:
		acc_delete (hostaddrs[i], sizes[i]);
		break;
	      case GOMP_MAP_FORCE_FROM:
		acc_copyout (hostaddrs[i], sizes[i]);
		break;
	      default:
		gomp_fatal (">>>> GOACC_enter_exit_data UNHANDLED kind 0x%.2x",
			    kind);
		break;
	      }
	  }
	else
	  {
	    gomp_acc_remove_pointer (hostaddrs[i], (kinds[i] & 0xff)
				     == GOMP_MAP_FORCE_FROM, async, 3);
	    /* See the above comment.  */
	    i += 2;
	  }
      }

  acc_dev->openacc.async_set_async_func (acc_async_sync);
}

static void
goacc_wait (int async, int num_waits, va_list *ap)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  while (num_waits--)
    {
      int qid = va_arg (*ap, int);
      
      if (acc_async_test (qid))
	continue;

      if (async == acc_async_sync)
	acc_wait (qid);
      else if (qid == async)
	;/* If we're waiting on the same asynchronous queue as we're
	    launching on, the queue itself will order work as
	    required, so there's no need to wait explicitly.  */
      else
	acc_dev->openacc.async_wait_async_func (qid, async);
    }
}

void
GOACC_update (int device, size_t mapnum,
	      void **hostaddrs, size_t *sizes, unsigned short *kinds,
	      int async, int num_waits, ...)
{
  bool host_fallback = device == GOMP_DEVICE_HOST_FALLBACK;
  size_t i;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if ((acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
      || host_fallback)
    return;

  if (num_waits)
    {
      va_list ap;

      va_start (ap, num_waits);
      goacc_wait (async, num_waits, &ap);
      va_end (ap);
    }

  acc_dev->openacc.async_set_async_func (async);

  for (i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;

      switch (kind)
	{
	case GOMP_MAP_POINTER:
	case GOMP_MAP_TO_PSET:
	  break;

	case GOMP_MAP_FORCE_TO:
	  acc_update_device (hostaddrs[i], sizes[i]);
	  break;

	case GOMP_MAP_FORCE_FROM:
	  acc_update_self (hostaddrs[i], sizes[i]);
	  break;

	default:
	  gomp_fatal (">>>> GOACC_update UNHANDLED kind 0x%.2x", kind);
	  break;
	}
    }

  acc_dev->openacc.async_set_async_func (acc_async_sync);
}

void
GOACC_wait (int async, int num_waits, ...)
{
  if (num_waits)
    {
      va_list ap;

      va_start (ap, num_waits);
      goacc_wait (async, num_waits, &ap);
      va_end (ap);
    }
  else if (async == acc_async_sync)
    acc_wait_all ();
  else if (async == acc_async_noval)
    goacc_thread ()->dev->openacc.async_wait_all_async_func (acc_async_noval);
}

int
GOACC_get_num_threads (void)
{
  return 1;
}

int
GOACC_get_thread_num (void)
{
  return 0;
}

void
GOACC_declare (int device, size_t mapnum,
	       void **hostaddrs, size_t *sizes, unsigned short *kinds)
{
  int i;

  for (i = 0; i < mapnum; i++)
    {
      unsigned char kind = kinds[i] & 0xff;

      if (kind == GOMP_MAP_POINTER || kind == GOMP_MAP_TO_PSET)
	continue;

      switch (kind)
	{
	  case GOMP_MAP_FORCE_ALLOC:
	  case GOMP_MAP_FORCE_DEALLOC:
	  case GOMP_MAP_FORCE_FROM:
	  case GOMP_MAP_FORCE_TO:
	  case GOMP_MAP_POINTER:
	    GOACC_enter_exit_data (device, 1, &hostaddrs[i], &sizes[i],
				   &kinds[i], 0, 0);
	    break;

	  case GOMP_MAP_FORCE_DEVICEPTR:
	    break;

	  case GOMP_MAP_ALLOC:
	    if (!acc_is_present (hostaddrs[i], sizes[i]))
	      GOACC_enter_exit_data (device, 1, &hostaddrs[i], &sizes[i],
				     &kinds[i], 0, 0);
	    break;

	  case GOMP_MAP_TO:
	    GOACC_enter_exit_data (device, 1, &hostaddrs[i], &sizes[i],
				   &kinds[i], 0, 0);

	    break;

	  case GOMP_MAP_FROM:
	    kinds[i] = GOMP_MAP_FORCE_FROM;
	    GOACC_enter_exit_data (device, 1, &hostaddrs[i], &sizes[i],
				   &kinds[i], 0, 0);
	    break;

	  case GOMP_MAP_FORCE_PRESENT:
	    if (!acc_is_present (hostaddrs[i], sizes[i]))
	      gomp_fatal ("[%p,%ld] is not mapped", hostaddrs[i],
			  (unsigned long) sizes[i]);
	    break;

	  default:
	    assert (0);
	    break;
	}
    }
}
