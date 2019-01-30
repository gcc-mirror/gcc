/* OpenACC Runtime initialization routines

   Copyright (C) 2013-2019 Free Software Foundation, Inc.

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

#include "openacc.h"
#include "config.h"
#include "libgomp.h"
#include "gomp-constants.h"
#include "oacc-int.h"
#include <stdint.h>
#include <string.h>
#include <assert.h>

/* Return block containing [H->S), or NULL if not contained.  The device lock
   for DEV must be locked on entry, and remains locked on exit.  */

static splay_tree_key
lookup_host (struct gomp_device_descr *dev, void *h, size_t s)
{
  struct splay_tree_key_s node;
  splay_tree_key key;

  node.host_start = (uintptr_t) h;
  node.host_end = (uintptr_t) h + s;

  key = splay_tree_lookup (&dev->mem_map, &node);

  return key;
}

/* Helper for lookup_dev.  Iterate over splay tree.  */

static splay_tree_key
lookup_dev_1 (splay_tree_node node, uintptr_t d, size_t s)
{
  splay_tree_key k = &node->key;
  struct target_mem_desc *t = k->tgt;

  if (d >= t->tgt_start && d + s <= t->tgt_end)
    return k;

  if (node->left)
    return lookup_dev_1 (node->left, d, s);
  if (node->right)
    return lookup_dev_1 (node->right, d, s);

  return NULL;
}

/* Return block containing [D->S), or NULL if not contained.
   The list isn't ordered by device address, so we have to iterate
   over the whole array.  This is not expected to be a common
   operation.  The device lock associated with TGT must be locked on entry, and
   remains locked on exit.  */

static splay_tree_key
lookup_dev (splay_tree mem_map, void *d, size_t s)
{
  if (!mem_map || !mem_map->root)
    return NULL;

  return lookup_dev_1 (mem_map->root, (uintptr_t) d, s);
}

/* OpenACC is silent on how memory exhaustion is indicated.  We return
   NULL.  */

void *
acc_malloc (size_t s)
{
  if (!s)
    return NULL;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();

  assert (thr->dev);

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return malloc (s);

  return thr->dev->alloc_func (thr->dev->target_id, s);
}

/* OpenACC 2.0a (3.2.16) doesn't specify what to do in the event
   the device address is mapped. We choose to check if it mapped,
   and if it is, to unmap it. */
void
acc_free (void *d)
{
  splay_tree_key k;

  if (!d)
    return;

  struct goacc_thread *thr = goacc_thread ();

  assert (thr && thr->dev);

  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return free (d);

  gomp_mutex_lock (&acc_dev->lock);

  /* We don't have to call lazy open here, as the ptr value must have
     been returned by acc_malloc.  It's not permitted to pass NULL in
     (unless you got that null from acc_malloc).  */
  if ((k = lookup_dev (&acc_dev->mem_map, d, 1)))
    {
      void *offset;

      offset = d - k->tgt->tgt_start + k->tgt_offset;

      gomp_mutex_unlock (&acc_dev->lock);

      acc_unmap_data ((void *)(k->host_start + offset));
    }
  else
    gomp_mutex_unlock (&acc_dev->lock);

  if (!acc_dev->free_func (acc_dev->target_id, d))
    gomp_fatal ("error in freeing device memory in %s", __FUNCTION__);
}

static void
memcpy_tofrom_device (bool from, void *d, void *h, size_t s, int async,
		      const char *libfnname)
{
  /* No need to call lazy open here, as the device pointer must have
     been obtained from a routine that did that.  */
  struct goacc_thread *thr = goacc_thread ();

  assert (thr && thr->dev);

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    {
      if (from)
	memmove (h, d, s);
      else
	memmove (d, h, s);
      return;
    }

  goacc_aq aq = get_goacc_asyncqueue (async);
  if (from)
    gomp_copy_dev2host (thr->dev, aq, h, d, s);
  else
    gomp_copy_host2dev (thr->dev, aq, d, h, s, /* TODO: cbuf? */ NULL);
}

void
acc_memcpy_to_device (void *d, void *h, size_t s)
{
  memcpy_tofrom_device (false, d, h, s, acc_async_sync, __FUNCTION__);
}

void
acc_memcpy_to_device_async (void *d, void *h, size_t s, int async)
{
  memcpy_tofrom_device (false, d, h, s, async, __FUNCTION__);
}

void
acc_memcpy_from_device (void *h, void *d, size_t s)
{
  memcpy_tofrom_device (true, d, h, s, acc_async_sync, __FUNCTION__);
}

void
acc_memcpy_from_device_async (void *h, void *d, size_t s, int async)
{
  memcpy_tofrom_device (true, d, h, s, async, __FUNCTION__);
}

/* Return the device pointer that corresponds to host data H.  Or NULL
   if no mapping.  */

void *
acc_deviceptr (void *h)
{
  splay_tree_key n;
  void *d;
  void *offset;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *dev = thr->dev;

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return h;

  gomp_mutex_lock (&dev->lock);

  n = lookup_host (dev, h, 1);

  if (!n)
    {
      gomp_mutex_unlock (&dev->lock);
      return NULL;
    }

  offset = h - n->host_start;

  d = n->tgt->tgt_start + n->tgt_offset + offset;

  gomp_mutex_unlock (&dev->lock);

  return d;
}

/* Return the host pointer that corresponds to device data D.  Or NULL
   if no mapping.  */

void *
acc_hostptr (void *d)
{
  splay_tree_key n;
  void *h;
  void *offset;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return d;

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_dev (&acc_dev->mem_map, d, 1);

  if (!n)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      return NULL;
    }

  offset = d - n->tgt->tgt_start + n->tgt_offset;

  h = n->host_start + offset;

  gomp_mutex_unlock (&acc_dev->lock);

  return h;
}

/* Return 1 if host data [H,+S] is present on the device.  */

int
acc_is_present (void *h, size_t s)
{
  splay_tree_key n;

  if (!s || !h)
    return 0;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return h != NULL;

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_host (acc_dev, h, s);

  if (n && ((uintptr_t)h < n->host_start
	    || (uintptr_t)h + s > n->host_end
	    || s > n->host_end - n->host_start))
    n = NULL;

  gomp_mutex_unlock (&acc_dev->lock);

  return n != NULL;
}

/* Create a mapping for host [H,+S] -> device [D,+S] */

void
acc_map_data (void *h, void *d, size_t s)
{
  struct target_mem_desc *tgt = NULL;
  size_t mapnum = 1;
  void *hostaddrs = h;
  void *devaddrs = d;
  size_t sizes = s;
  unsigned short kinds = GOMP_MAP_ALLOC;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    {
      if (d != h)
        gomp_fatal ("cannot map data on shared-memory system");
    }
  else
    {
      struct goacc_thread *thr = goacc_thread ();

      if (!d || !h || !s)
	gomp_fatal ("[%p,+%d]->[%p,+%d] is a bad map",
                    (void *)h, (int)s, (void *)d, (int)s);

      gomp_mutex_lock (&acc_dev->lock);

      if (lookup_host (acc_dev, h, s))
        {
	  gomp_mutex_unlock (&acc_dev->lock);
	  gomp_fatal ("host address [%p, +%d] is already mapped", (void *)h,
		      (int)s);
	}

      if (lookup_dev (&thr->dev->mem_map, d, s))
        {
	  gomp_mutex_unlock (&acc_dev->lock);
	  gomp_fatal ("device address [%p, +%d] is already mapped", (void *)d,
		      (int)s);
	}

      gomp_mutex_unlock (&acc_dev->lock);

      tgt = gomp_map_vars (acc_dev, mapnum, &hostaddrs, &devaddrs, &sizes,
			   &kinds, true, GOMP_MAP_VARS_OPENACC);
      tgt->list[0].key->refcount = REFCOUNT_INFINITY;
    }
}

void
acc_unmap_data (void *h)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;
  struct splay_tree_key_s cur_node;

  /* No need to call lazy open, as the address must have been mapped.  */

  /* This is a no-op on shared-memory targets.  */
  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  gomp_mutex_lock (&acc_dev->lock);

  cur_node.host_start = (uintptr_t) h;
  cur_node.host_end = cur_node.host_start + 1;
  splay_tree_key n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

  if (!n)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("%p is not a mapped block", (void *)h);
    }

  if (n->host_start != (uintptr_t) h)
    {
      size_t host_size = n->host_end - n->host_start;
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,%d] surrounds %p",
		  (void *) n->host_start, (int) host_size, (void *) h);
    }

  splay_tree_remove (&acc_dev->mem_map, n);

  struct target_mem_desc *tgt = n->tgt;

  if (tgt->refcount > 0)
    tgt->refcount--;
  else
    {
      free (tgt->array);
      free (tgt);
    }

  gomp_mutex_unlock (&acc_dev->lock);
}

#define FLAG_PRESENT (1 << 0)
#define FLAG_CREATE (1 << 1)
#define FLAG_COPY (1 << 2)

static void *
present_create_copy (unsigned f, void *h, size_t s, int async)
{
  void *d;
  splay_tree_key n;

  if (!h || !s)
    gomp_fatal ("[%p,+%d] is a bad range", (void *)h, (int)s);

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return h;

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_host (acc_dev, h, s);
  if (n)
    {
      /* Present. */
      d = (void *) (n->tgt->tgt_start + n->tgt_offset);

      if (!(f & FLAG_PRESENT))
        {
	  gomp_mutex_unlock (&acc_dev->lock);
          gomp_fatal ("[%p,+%d] already mapped to [%p,+%d]",
        	      (void *)h, (int)s, (void *)d, (int)s);
	}
      if ((h + s) > (void *)n->host_end)
	{
	  gomp_mutex_unlock (&acc_dev->lock);
	  gomp_fatal ("[%p,+%d] not mapped", (void *)h, (int)s);
	}

      assert (n->virtual_refcount != VREFCOUNT_LINK_KEY);

      if (n->refcount != REFCOUNT_INFINITY)
	{
	  n->refcount++;
	  n->virtual_refcount++;
	}

      gomp_mutex_unlock (&acc_dev->lock);
    }
  else if (!(f & FLAG_CREATE))
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,+%d] not mapped", (void *)h, (int)s);
    }
  else
    {
      size_t mapnum = 1;
      unsigned short kinds;
      void *hostaddrs = h;

      if (f & FLAG_COPY)
	kinds = GOMP_MAP_TO;
      else
	kinds = GOMP_MAP_ALLOC;

      gomp_mutex_unlock (&acc_dev->lock);

      goacc_aq aq = get_goacc_asyncqueue (async);

      gomp_map_vars_async (acc_dev, aq, mapnum, &hostaddrs, NULL, &s, &kinds,
			   true, GOMP_MAP_VARS_OPENACC_ENTER_DATA);

      gomp_mutex_lock (&acc_dev->lock);
      n = lookup_host (acc_dev, h, s);
      assert (n != NULL);
      d = (void *) (n->tgt->tgt_start + n->tgt_offset + (uintptr_t) h
		    - n->host_start);
      gomp_mutex_unlock (&acc_dev->lock);
    }

  return d;
}

void *
acc_create (void *h, size_t s)
{
  return present_create_copy (FLAG_PRESENT | FLAG_CREATE, h, s, acc_async_sync);
}

void
acc_create_async (void *h, size_t s, int async)
{
  present_create_copy (FLAG_PRESENT | FLAG_CREATE, h, s, async);
}

/* acc_present_or_create used to be what acc_create is now.  */
/* acc_pcreate is acc_present_or_create by a different name.  */
#ifdef HAVE_ATTRIBUTE_ALIAS
strong_alias (acc_create, acc_present_or_create)
strong_alias (acc_create, acc_pcreate)
#else
void *
acc_present_or_create (void *h, size_t s)
{
  return acc_create (h, s);
}

void *
acc_pcreate (void *h, size_t s)
{
  return acc_create (h, s);
}
#endif

void *
acc_copyin (void *h, size_t s)
{
  return present_create_copy (FLAG_PRESENT | FLAG_CREATE | FLAG_COPY, h, s,
			      acc_async_sync);
}

void
acc_copyin_async (void *h, size_t s, int async)
{
  present_create_copy (FLAG_PRESENT | FLAG_CREATE | FLAG_COPY, h, s, async);
}

/* acc_present_or_copyin used to be what acc_copyin is now.  */
/* acc_pcopyin is acc_present_or_copyin by a different name.  */
#ifdef HAVE_ATTRIBUTE_ALIAS
strong_alias (acc_copyin, acc_present_or_copyin)
strong_alias (acc_copyin, acc_pcopyin)
#else
void *
acc_present_or_copyin (void *h, size_t s)
{
  return acc_copyin (h, s);
}

void *
acc_pcopyin (void *h, size_t s)
{
  return acc_copyin (h, s);
}
#endif

#define FLAG_COPYOUT  (1 << 0)
#define FLAG_FINALIZE (1 << 1)

static void
delete_copyout (unsigned f, void *h, size_t s, int async, const char *libfnname)
{
  size_t host_size;
  splay_tree_key n;
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_host (acc_dev, h, s);

  /* No need to call lazy open, as the data must already have been
     mapped.  */

  if (!n)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,%d] is not mapped", (void *)h, (int)s);
    }

  assert (n->virtual_refcount != VREFCOUNT_LINK_KEY);

  host_size = n->host_end - n->host_start;

  if (n->host_start != (uintptr_t) h || host_size != s)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,%d] surrounds2 [%p,+%d]",
		  (void *) n->host_start, (int) host_size, (void *) h, (int) s);
    }

  if (n->refcount == REFCOUNT_INFINITY)
    {
      n->refcount = 0;
      n->virtual_refcount = 0;
    }

  if (f & FLAG_FINALIZE)
    {
      n->refcount -= n->virtual_refcount;
      n->virtual_refcount = 0;
    }

  if (n->virtual_refcount > 0)
    {
      n->refcount--;
      n->virtual_refcount--;
    }
  else if (n->refcount > 0)
    n->refcount--;

  if (n->refcount == 0)
    {
      goacc_aq aq = get_goacc_asyncqueue (async);

      if (f & FLAG_COPYOUT)
        {
	  void *d = (void *) (n->tgt->tgt_start + n->tgt_offset
			      + (uintptr_t) h - n->host_start);
	  gomp_copy_dev2host (acc_dev, aq, h, d, s);
	}
      gomp_remove_var_async (acc_dev, n, aq);
    }

  gomp_mutex_unlock (&acc_dev->lock);
}

void
acc_delete (void *h , size_t s)
{
  delete_copyout (0, h, s, acc_async_sync, __FUNCTION__);
}

void
acc_delete_async (void *h , size_t s, int async)
{
  delete_copyout (0, h, s, async, __FUNCTION__);
}

void
acc_delete_finalize (void *h , size_t s)
{
  delete_copyout (FLAG_FINALIZE, h, s, acc_async_sync, __FUNCTION__);
}

void
acc_delete_finalize_async (void *h , size_t s, int async)
{
  delete_copyout (FLAG_FINALIZE, h, s, async, __FUNCTION__);
}

void
acc_copyout (void *h, size_t s)
{
  delete_copyout (FLAG_COPYOUT, h, s, acc_async_sync, __FUNCTION__);
}

void
acc_copyout_async (void *h, size_t s, int async)
{
  delete_copyout (FLAG_COPYOUT, h, s, async, __FUNCTION__);
}

void
acc_copyout_finalize (void *h, size_t s)
{
  delete_copyout (FLAG_COPYOUT | FLAG_FINALIZE, h, s, acc_async_sync,
		  __FUNCTION__);
}

void
acc_copyout_finalize_async (void *h, size_t s, int async)
{
  delete_copyout (FLAG_COPYOUT | FLAG_FINALIZE, h, s, async, __FUNCTION__);
}

static void
update_dev_host (int is_dev, void *h, size_t s, int async)
{
  splay_tree_key n;
  void *d;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  /* Fortran optional arguments that are non-present result in a
     null host address here.  This can safely be ignored as it is
     not possible to 'update' a non-present optional argument.  */
  if (h == NULL)
    return;

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_host (acc_dev, h, s);

  if (!n)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,%d] is not mapped", h, (int)s);
    }

  d = (void *) (n->tgt->tgt_start + n->tgt_offset
		+ (uintptr_t) h - n->host_start);

  goacc_aq aq = get_goacc_asyncqueue (async);

  if (is_dev)
    gomp_copy_host2dev (acc_dev, aq, d, h, s, /* TODO: cbuf? */ NULL);
  else
    gomp_copy_dev2host (acc_dev, aq, h, d, s);

  gomp_mutex_unlock (&acc_dev->lock);
}

void
acc_update_device (void *h, size_t s)
{
  update_dev_host (1, h, s, acc_async_sync);
}

void
acc_update_device_async (void *h, size_t s, int async)
{
  update_dev_host (1, h, s, async);
}

void
acc_update_self (void *h, size_t s)
{
  update_dev_host (0, h, s, acc_async_sync);
}

void
acc_update_self_async (void *h, size_t s, int async)
{
  update_dev_host (0, h, s, async);
}

void
gomp_acc_declare_allocate (bool allocate, size_t mapnum, void **hostaddrs,
                          size_t *sizes, unsigned short *kinds)
{
  gomp_debug (0, "  %s: processing\n", __FUNCTION__);

  if (allocate)
    {
      assert (mapnum == 3);

      /* Allocate memory for the array data.  */
      uintptr_t data = (uintptr_t) acc_create (hostaddrs[0], sizes[0]);

      /* Update the PSET.  */
      acc_update_device (hostaddrs[1], sizes[1]);
      void *pset = acc_deviceptr (hostaddrs[1]);
      acc_memcpy_to_device (pset, &data, sizeof (uintptr_t));
    }
  else
    {
      /* Deallocate memory for the array data.  */
      void *data = acc_deviceptr (hostaddrs[0]);
      acc_free (data);
    }

  gomp_debug (0, "  %s: end\n", __FUNCTION__);
}

void
gomp_acc_remove_pointer (struct gomp_device_descr *acc_dev, void **hostaddrs,
			 size_t *sizes, unsigned short *kinds, int async,
			 bool finalize, int mapnum)
{
  struct splay_tree_key_s cur_node;
  splay_tree_key n;

  gomp_mutex_lock (&acc_dev->lock);

  for (int i = 0; i < mapnum; i++)
    {
      int kind = kinds[i] & 0xff;
      bool copyfrom = false;

      switch (kind)
        {
	case GOMP_MAP_FROM:
	case GOMP_MAP_FORCE_FROM:
	case GOMP_MAP_ALWAYS_FROM:
	  copyfrom = true;
	  /* Fallthrough.  */

	case GOMP_MAP_TO_PSET:
	case GOMP_MAP_POINTER:
	case GOMP_MAP_DELETE:
	case GOMP_MAP_RELEASE:
	case GOMP_MAP_DETACH:
	case GOMP_MAP_FORCE_DETACH:
	  cur_node.host_start = (uintptr_t) hostaddrs[i];
	  cur_node.host_end = cur_node.host_start
			      + ((kind == GOMP_MAP_DETACH
				  || kind == GOMP_MAP_FORCE_DETACH
				  || kind == GOMP_MAP_POINTER)
				 ? sizeof (void *) : sizes[i]);
	  n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

	  if (n == NULL)
	    continue;

	  assert (n->virtual_refcount != VREFCOUNT_LINK_KEY);

	  if (n->refcount == REFCOUNT_INFINITY)
	    {
	      n->refcount = 1;
	      n->virtual_refcount = 0;
	    }

	  if (finalize)
	    {
	      n->refcount -= n->virtual_refcount;
	      n->virtual_refcount = 0;
	    }

	  if (n->virtual_refcount > 0)
	    {
	      n->refcount--;
	      n->virtual_refcount--;
	    }
	  else if (n->refcount > 0)
	    n->refcount--;

	  if (copyfrom)
	    gomp_copy_dev2host (acc_dev, NULL, (void *) cur_node.host_start,
				(void *) (n->tgt->tgt_start + n->tgt_offset
					  + cur_node.host_start
					  - n->host_start),
				cur_node.host_end - cur_node.host_start);

	  if (n->refcount == 0)
	    gomp_remove_var (acc_dev, n);
	  break;

	default:
	  gomp_mutex_unlock (&acc_dev->lock);
	  gomp_fatal ("gomp_acc_remove_pointer unhandled kind 0x%.2x",
		      kind);
	}
    }

  gomp_mutex_unlock (&acc_dev->lock);
}

void
acc_attach_async (void **hostaddr, int async)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;
  goacc_aq aq = get_goacc_asyncqueue (async);

  struct splay_tree_key_s cur_node;
  splay_tree_key n;

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  cur_node.host_start = (uintptr_t) hostaddr;
  cur_node.host_end = cur_node.host_start + sizeof (void *);
  n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

  if (n == NULL)
    gomp_fatal ("struct not mapped for acc_attach");

  gomp_attach_pointer (acc_dev, aq, &acc_dev->mem_map, n, (uintptr_t) hostaddr,
		       0, NULL);
}

void
acc_attach (void **hostaddr)
{
  acc_attach_async (hostaddr, acc_async_sync);
}

static void
goacc_detach_internal (void **hostaddr, int async, bool finalize)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;
  struct splay_tree_key_s cur_node;
  splay_tree_key n;
  struct goacc_asyncqueue *aq = get_goacc_asyncqueue (async);

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  cur_node.host_start = (uintptr_t) hostaddr;
  cur_node.host_end = cur_node.host_start + sizeof (void *);
  n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

  if (n == NULL)
    gomp_fatal ("struct not mapped for acc_detach");

  gomp_detach_pointer (acc_dev, aq, n, (uintptr_t) hostaddr, finalize, NULL);
}

void
acc_detach (void **hostaddr)
{
  goacc_detach_internal (hostaddr, acc_async_sync, false);
}

void
acc_detach_async (void **hostaddr, int async)
{
  goacc_detach_internal (hostaddr, async, false);
}

void
acc_detach_finalize (void **hostaddr)
{
  goacc_detach_internal (hostaddr, acc_async_sync, true);
}

void
acc_detach_finalize_async (void **hostaddr, int async)
{
  goacc_detach_internal (hostaddr, async, true);
}
