/* OpenACC Runtime initialization routines

   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
#include "libgomp.h"
#include "gomp-constants.h"
#include "oacc-int.h"
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
  splay_tree_key key = &node->key;
  if (d >= key->tgt->tgt_start && d + s <= key->tgt->tgt_end)
    return key;

  key = NULL;
  if (node->left)
    key = lookup_dev_1 (node->left, d, s);
  if (!key && node->right)
    key = lookup_dev_1 (node->right, d, s);

  return key;
}

/* Return block containing [D->S), or NULL if not contained.

   This iterates over the splay tree.  This is not expected to be a common
   operation.

   The device lock associated with MEM_MAP must be locked on entry, and remains
   locked on exit.  */

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

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

  void *res = thr->dev->alloc_func (thr->dev->target_id, s);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  return res;
}

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

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

  gomp_mutex_lock (&acc_dev->lock);

  /* We don't have to call lazy open here, as the ptr value must have
     been returned by acc_malloc.  It's not permitted to pass NULL in
     (unless you got that null from acc_malloc).  */
  if ((k = lookup_dev (&acc_dev->mem_map, d, 1)))
    {
      void *offset = d - k->tgt->tgt_start + k->tgt_offset;
      void *h = k->host_start + offset;
      size_t h_size = k->host_end - k->host_start;
      gomp_mutex_unlock (&acc_dev->lock);
      /* PR92503 "[OpenACC] Behavior of 'acc_free' if the memory space is still
	 used in a mapping".  */
      gomp_fatal ("refusing to free device memory space at %p that is still"
		  " mapped at [%p,+%d]",
		  d, h, (int) h_size);
    }
  else
    gomp_mutex_unlock (&acc_dev->lock);

  if (!acc_dev->free_func (acc_dev->target_id, d))
    gomp_fatal ("error in freeing device memory in %s", __FUNCTION__);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
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

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  goacc_aq aq = get_goacc_asyncqueue (async);
  if (from)
    gomp_copy_dev2host (thr->dev, aq, h, d, s);
  else
    gomp_copy_host2dev (thr->dev, aq, d, h, s, /* TODO: cbuf? */ NULL);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
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

  /* In the following, no OpenACC Profiling Interface events can possibly be
     generated.  */

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

  /* In the following, no OpenACC Profiling Interface events can possibly be
     generated.  */

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

  /* In the following, no OpenACC Profiling Interface events can possibly be
     generated.  */

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

      acc_prof_info prof_info;
      acc_api_info api_info;
      bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

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

      struct target_mem_desc *tgt
	= gomp_map_vars (acc_dev, mapnum, &hostaddrs, &devaddrs, &sizes,
			 &kinds, true, GOMP_MAP_VARS_ENTER_DATA);
      assert (tgt);
      assert (tgt->list_count == 1);
      splay_tree_key n = tgt->list[0].key;
      assert (n);
      assert (n->refcount == 1);
      assert (n->dynamic_refcount == 0);
      /* Special reference counting behavior.  */
      n->refcount = REFCOUNT_INFINITY;

      if (profiling_p)
	{
	  thr->prof_info = NULL;
	  thr->api_info = NULL;
	}
    }
}

void
acc_unmap_data (void *h)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  /* No need to call lazy open, as the address must have been mapped.  */

  /* This is a no-op on shared-memory targets.  */
  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);

  gomp_mutex_lock (&acc_dev->lock);

  splay_tree_key n = lookup_host (acc_dev, h, 1);

  if (!n)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("%p is not a mapped block", (void *)h);
    }

  size_t host_size = n->host_end - n->host_start;

  if (n->host_start != (uintptr_t) h)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,%d] surrounds %p",
		  (void *) n->host_start, (int) host_size, (void *) h);
    }
  /* TODO This currently doesn't catch 'REFCOUNT_INFINITY' usage different from
     'acc_map_data'.  Maybe 'dynamic_refcount' can be used for disambiguating
     the different 'REFCOUNT_INFINITY' cases, or simply separate
     'REFCOUNT_INFINITY' values per different usage ('REFCOUNT_ACC_MAP_DATA'
     etc.)?  */
  else if (n->refcount != REFCOUNT_INFINITY)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("refusing to unmap block [%p,+%d] that has not been mapped"
		  " by 'acc_map_data'",
		  (void *) h, (int) host_size);
    }

  struct target_mem_desc *tgt = n->tgt;

  if (tgt->refcount == REFCOUNT_INFINITY)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("cannot unmap target block");
    }

  /* Above, we've verified that the mapping must have been set up by
     'acc_map_data'.  */
  assert (tgt->refcount == 1);

  /* Nullifying these fields prevents 'gomp_unmap_tgt' via 'gomp_remove_var'
     from freeing the target memory.  */
  tgt->tgt_end = 0;
  tgt->to_free = NULL;

  bool is_tgt_unmapped = gomp_remove_var (acc_dev, n);
  assert (is_tgt_unmapped);

  gomp_mutex_unlock (&acc_dev->lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}


/* Helper function to map a single dynamic data item, represented by a single
   mapping.  The acc_dev->lock should be held on entry, and remains locked on
   exit.  */

static void *
goacc_map_var_existing (struct gomp_device_descr *acc_dev, void *hostaddr,
			size_t size, splay_tree_key n)
{
  assert (n);

  /* Present. */
  void *d = (void *) (n->tgt->tgt_start + n->tgt_offset + hostaddr
	    - n->host_start);

  if (hostaddr + size > (void *) n->host_end)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,+%d] not mapped", hostaddr, (int) size);
    }

  assert (n->refcount != REFCOUNT_LINK);
  if (n->refcount != REFCOUNT_INFINITY)
    n->refcount++;
  n->dynamic_refcount++;

  return d;
}

/* Enter dynamic mapping for a single datum.  Return the device pointer.  */

static void *
goacc_enter_datum (void **hostaddrs, size_t *sizes, void *kinds, int async)
{
  void *d;
  splay_tree_key n;

  if (!hostaddrs[0] || !sizes[0])
    gomp_fatal ("[%p,+%d] is a bad range", hostaddrs[0], (int) sizes[0]);

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return hostaddrs[0];

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  gomp_mutex_lock (&acc_dev->lock);

  n = lookup_host (acc_dev, hostaddrs[0], sizes[0]);
  if (n)
    {
      d = goacc_map_var_existing (acc_dev, hostaddrs[0], sizes[0], n);
      gomp_mutex_unlock (&acc_dev->lock);
    }
  else
    {
      const size_t mapnum = 1;

      gomp_mutex_unlock (&acc_dev->lock);

      goacc_aq aq = get_goacc_asyncqueue (async);

      struct target_mem_desc *tgt
	= gomp_map_vars_async (acc_dev, aq, mapnum, hostaddrs, NULL, sizes,
			       kinds, true, GOMP_MAP_VARS_ENTER_DATA);
      assert (tgt);
      assert (tgt->list_count == 1);
      n = tgt->list[0].key;
      assert (n);
      assert (n->refcount == 1);
      assert (n->dynamic_refcount == 0);
      n->dynamic_refcount++;

      d = (void *) tgt->tgt_start;
    }

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }

  return d;
}

void *
acc_create (void *h, size_t s)
{
  unsigned short kinds[1] = { GOMP_MAP_ALLOC };
  return goacc_enter_datum (&h, &s, &kinds, acc_async_sync);
}

void
acc_create_async (void *h, size_t s, int async)
{
  unsigned short kinds[1] = { GOMP_MAP_ALLOC };
  goacc_enter_datum (&h, &s, &kinds, async);
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
  unsigned short kinds[1] = { GOMP_MAP_TO };
  return goacc_enter_datum (&h, &s, &kinds, acc_async_sync);
}

void
acc_copyin_async (void *h, size_t s, int async)
{
  unsigned short kinds[1] = { GOMP_MAP_TO };
  goacc_enter_datum (&h, &s, &kinds, async);
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


/* Helper function to unmap a single data item.  Device lock should be held on
   entry, and remains locked on exit.  */

static void
goacc_exit_datum_1 (struct gomp_device_descr *acc_dev, void *h, size_t s,
		    unsigned short kind, splay_tree_key n, goacc_aq aq)
{
  if ((uintptr_t) h < n->host_start || (uintptr_t) h + s > n->host_end)
    {
      size_t host_size = n->host_end - n->host_start;
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("[%p,+%d] outside mapped block [%p,+%d]",
		  (void *) h, (int) s, (void *) n->host_start, (int) host_size);
    }

  bool finalize = (kind == GOMP_MAP_FORCE_FROM
		   || kind == GOMP_MAP_DELETE
		   || kind == GOMP_MAP_FORCE_DETACH);

  assert (n->refcount != REFCOUNT_LINK);
  if (n->refcount != REFCOUNT_INFINITY
      && n->refcount < n->dynamic_refcount)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("Dynamic reference counting assert fail\n");
    }

  if (finalize)
    {
      if (n->refcount != REFCOUNT_INFINITY)
	n->refcount -= n->dynamic_refcount;
      n->dynamic_refcount = 0;
    }
  else if (n->dynamic_refcount)
    {
      if (n->refcount != REFCOUNT_INFINITY)
	n->refcount--;
      n->dynamic_refcount--;
    }

  if (n->refcount == 0)
    {
      bool copyout = (kind == GOMP_MAP_FROM
		      || kind == GOMP_MAP_FORCE_FROM);
      if (copyout)
	{
	  void *d = (void *) (n->tgt->tgt_start + n->tgt_offset
			      + (uintptr_t) h - n->host_start);
	  gomp_copy_dev2host (acc_dev, aq, h, d, s);
	}

      if (aq)
	/* TODO We can't do the 'is_tgt_unmapped' checking -- see the
	   'gomp_unref_tgt' comment in
	   <http://mid.mail-archive.com/878snl36eu.fsf@euler.schwinge.homeip.net>;
	   PR92881.  */
	gomp_remove_var_async (acc_dev, n, aq);
      else
	{
	  size_t num_mappings = 0;
	  /* If the target_mem_desc represents a single data mapping, we can
	     check that it is freed when this splay tree key's refcount reaches
	     zero.  Otherwise (e.g. for a 'GOMP_MAP_STRUCT' mapping with
	     multiple members), fall back to skipping the test.  */
	  for (size_t l_i = 0; l_i < n->tgt->list_count; ++l_i)
	    if (n->tgt->list[l_i].key)
	      ++num_mappings;
	  bool is_tgt_unmapped = gomp_remove_var (acc_dev, n);
	  assert (is_tgt_unmapped || num_mappings > 1);
	}
    }
}


/* Exit a dynamic mapping for a single variable.  */

static void
goacc_exit_datum (void *h, size_t s, unsigned short kind, int async)
{
  /* No need to call lazy open, as the data must already have been
     mapped.  */

  kind &= 0xff;

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  if (acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

  gomp_mutex_lock (&acc_dev->lock);

  splay_tree_key n = lookup_host (acc_dev, h, s);
  /* Non-present data is a no-op: PR92726, RP92970, PR92984.  */
  if (n)
    {
      goacc_aq aq = get_goacc_asyncqueue (async);
      goacc_exit_datum_1 (acc_dev, h, s, kind, n, aq);
    }

  gomp_mutex_unlock (&acc_dev->lock);

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

void
acc_delete (void *h , size_t s)
{
  goacc_exit_datum (h, s, GOMP_MAP_RELEASE, acc_async_sync);
}

void
acc_delete_async (void *h , size_t s, int async)
{
  goacc_exit_datum (h, s, GOMP_MAP_RELEASE, async);
}

void
acc_delete_finalize (void *h , size_t s)
{
  goacc_exit_datum (h, s, GOMP_MAP_DELETE, acc_async_sync);
}

void
acc_delete_finalize_async (void *h , size_t s, int async)
{
  goacc_exit_datum (h, s, GOMP_MAP_DELETE, async);
}

void
acc_copyout (void *h, size_t s)
{
  goacc_exit_datum (h, s, GOMP_MAP_FROM, acc_async_sync);
}

void
acc_copyout_async (void *h, size_t s, int async)
{
  goacc_exit_datum (h, s, GOMP_MAP_FROM, async);
}

void
acc_copyout_finalize (void *h, size_t s)
{
  goacc_exit_datum (h, s, GOMP_MAP_FORCE_FROM, acc_async_sync);
}

void
acc_copyout_finalize_async (void *h, size_t s, int async)
{
  goacc_exit_datum (h, s, GOMP_MAP_FORCE_FROM, async);
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
     NULL host address here.  This can safely be ignored as it is
     not possible to 'update' a non-present optional argument.  */
  if (h == NULL)
    return;

  acc_prof_info prof_info;
  acc_api_info api_info;
  bool profiling_p = GOACC_PROFILING_SETUP_P (thr, &prof_info, &api_info);
  if (profiling_p)
    {
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
    }

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

  if (profiling_p)
    {
      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
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
acc_attach_async (void **hostaddr, int async)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;
  goacc_aq aq = get_goacc_asyncqueue (async);

  struct splay_tree_key_s cur_node;
  splay_tree_key n;

  if (thr->dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  gomp_mutex_lock (&acc_dev->lock);

  cur_node.host_start = (uintptr_t) hostaddr;
  cur_node.host_end = cur_node.host_start + sizeof (void *);
  n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

  if (n == NULL)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("struct not mapped for acc_attach");
    }

  gomp_attach_pointer (acc_dev, aq, &acc_dev->mem_map, n, (uintptr_t) hostaddr,
		       0, NULL);

  gomp_mutex_unlock (&acc_dev->lock);
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

  gomp_mutex_lock (&acc_dev->lock);

  cur_node.host_start = (uintptr_t) hostaddr;
  cur_node.host_end = cur_node.host_start + sizeof (void *);
  n = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

  if (n == NULL)
    {
      gomp_mutex_unlock (&acc_dev->lock);
      gomp_fatal ("struct not mapped for acc_detach");
    }

  gomp_detach_pointer (acc_dev, aq, n, (uintptr_t) hostaddr, finalize, NULL);

  gomp_mutex_unlock (&acc_dev->lock);
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

/* Some types of (pointer) variables use several consecutive mappings, which
   must be treated as a group for enter/exit data directives.  This function
   returns the last mapping in such a group (inclusive), or POS for singleton
   mappings.  */

static int
find_group_last (int pos, size_t mapnum, size_t *sizes, unsigned short *kinds)
{
  unsigned char kind0 = kinds[pos] & 0xff;
  int first_pos = pos;

  switch (kind0)
    {
    case GOMP_MAP_TO_PSET:
      if (pos + 1 < mapnum
	  && (kinds[pos + 1] & 0xff) == GOMP_MAP_ATTACH)
	return pos + 1;

      while (pos + 1 < mapnum
	     && (kinds[pos + 1] & 0xff) == GOMP_MAP_POINTER)
	pos++;
      /* We expect at least one GOMP_MAP_POINTER (if not a single
	 GOMP_MAP_ATTACH) after a GOMP_MAP_TO_PSET.  */
      assert (pos > first_pos);
      break;

    case GOMP_MAP_STRUCT:
      pos += sizes[pos];
      break;

    case GOMP_MAP_POINTER:
    case GOMP_MAP_ALWAYS_POINTER:
      /* These mappings are only expected after some other mapping.  If we
	 see one by itself, something has gone wrong.  */
      gomp_fatal ("unexpected mapping");
      break;

    case GOMP_MAP_ATTACH:
      break;

    default:
      /* GOMP_MAP_ALWAYS_POINTER can only appear directly after some other
	 mapping.  */
      if (pos + 1 < mapnum)
	{
	  unsigned char kind1 = kinds[pos + 1] & 0xff;
	  if (kind1 == GOMP_MAP_ALWAYS_POINTER)
	    return pos + 1;
	}

      /* We can have a single GOMP_MAP_ATTACH mapping after a to/from
	 mapping.  */
      if (pos + 1 < mapnum
	  && (kinds[pos + 1] & 0xff) == GOMP_MAP_ATTACH)
	return pos + 1;

      /* We can have zero or more GOMP_MAP_POINTER mappings after a to/from
	 (etc.) mapping.  */
      while (pos + 1 < mapnum
	     && (kinds[pos + 1] & 0xff) == GOMP_MAP_POINTER)
	pos++;
    }

  return pos;
}

/* Map variables for OpenACC "enter data".  We can't just call
   gomp_map_vars_async once, because individual mapped variables might have
   "exit data" called for them at different times.  */

static void
goacc_enter_data_internal (struct gomp_device_descr *acc_dev, size_t mapnum,
			   void **hostaddrs, size_t *sizes,
			   unsigned short *kinds, goacc_aq aq)
{
  gomp_mutex_lock (&acc_dev->lock);

  for (size_t i = 0; i < mapnum; i++)
    {
      splay_tree_key n;
      size_t group_last = find_group_last (i, mapnum, sizes, kinds);
      bool struct_p = false;
      size_t size, groupnum = (group_last - i) + 1;

      switch (kinds[i] & 0xff)
	{
	case GOMP_MAP_STRUCT:
	  {
	    size = (uintptr_t) hostaddrs[group_last] + sizes[group_last]
		   - (uintptr_t) hostaddrs[i];
	    struct_p = true;
	  }
	  break;

	case GOMP_MAP_ATTACH:
	  size = sizeof (void *);
	  break;

	default:
	  size = sizes[i];
	}

      n = lookup_host (acc_dev, hostaddrs[i], size);

      if (n && struct_p)
	{
	  for (size_t j = i + 1; j <= group_last; j++)
	    {
	      struct splay_tree_key_s cur_node;
	      cur_node.host_start = (uintptr_t) hostaddrs[j];
	      cur_node.host_end = cur_node.host_start + sizes[j];
	      splay_tree_key n2
		= splay_tree_lookup (&acc_dev->mem_map, &cur_node);
	      if (!n2
		  || n2->tgt != n->tgt
		  || n2->host_start - n->host_start
		     != n2->tgt_offset - n->tgt_offset)
		{
		  gomp_mutex_unlock (&acc_dev->lock);
		  gomp_fatal ("Trying to map into device [%p..%p) structure "
			      "element when other mapped elements from the "
			      "same structure weren't mapped together with "
			      "it", (void *) cur_node.host_start,
			      (void *) cur_node.host_end);
		}
	    }
	  /* This is a special case because we must increment the refcount by
	     the number of mapped struct elements, rather than by one.  */
	  if (n->refcount != REFCOUNT_INFINITY)
	    n->refcount += groupnum - 1;
	  n->dynamic_refcount += groupnum - 1;
	}
      else if (n && groupnum == 1)
	{
	  void *h = hostaddrs[i];
	  size_t s = sizes[i];

	  /* A standalone attach clause.  */
	  if ((kinds[i] & 0xff) == GOMP_MAP_ATTACH)
	    gomp_attach_pointer (acc_dev, aq, &acc_dev->mem_map, n,
				 (uintptr_t) h, s, NULL);

	  goacc_map_var_existing (acc_dev, h, s, n);
	}
      else if (n && groupnum > 1)
	{
	  assert (n->refcount != REFCOUNT_INFINITY
		  && n->refcount != REFCOUNT_LINK);

	  for (size_t j = i + 1; j <= group_last; j++)
	    if ((kinds[j] & 0xff) == GOMP_MAP_ATTACH)
	      {
		splay_tree_key m
		  = lookup_host (acc_dev, hostaddrs[j], sizeof (void *));
		gomp_attach_pointer (acc_dev, aq, &acc_dev->mem_map, m,
				     (uintptr_t) hostaddrs[j], sizes[j], NULL);
	      }

	  bool processed = false;

	  struct target_mem_desc *tgt = n->tgt;
	  for (size_t j = 0; j < tgt->list_count; j++)
	    if (tgt->list[j].key == n)
	      {
		/* We are processing a group of mappings (e.g.
		   [GOMP_MAP_TO, GOMP_MAP_TO_PSET, GOMP_MAP_POINTER]).
		   Find the right group in the target_mem_desc's variable
		   list, and increment the refcounts for each item in that
		   group.  */
		for (size_t k = 0; k < groupnum; k++)
		  if (j + k < tgt->list_count && tgt->list[j + k].key)
		    {
		      tgt->list[j + k].key->refcount++;
		      tgt->list[j + k].key->dynamic_refcount++;
		    }
		processed = true;
		break;
	      }

	  if (!processed)
	    {
	      gomp_mutex_unlock (&acc_dev->lock);
	      gomp_fatal ("dynamic refcount incrementing failed for "
			  "pointer/pset");
	    }
	}
      else if (hostaddrs[i])
	{
	  /* The data is not mapped already.  Map it now, unless the first
	     member in the group has a NULL pointer (e.g. a non-present
	     optional parameter).  */
	  gomp_mutex_unlock (&acc_dev->lock);

	  struct target_mem_desc *tgt
	    = gomp_map_vars_async (acc_dev, aq, groupnum, &hostaddrs[i], NULL,
				   &sizes[i], &kinds[i], true,
				   GOMP_MAP_VARS_ENTER_DATA);
	  assert (tgt);

	  gomp_mutex_lock (&acc_dev->lock);

	  for (size_t j = 0; j < tgt->list_count; j++)
	    {
	      n = tgt->list[j].key;
	      if (n)
		n->dynamic_refcount++;
	    }
	}

      i = group_last;
    }

  gomp_mutex_unlock (&acc_dev->lock);
}

/* Unmap variables for OpenACC "exit data".  */

static void
goacc_exit_data_internal (struct gomp_device_descr *acc_dev, size_t mapnum,
			  void **hostaddrs, size_t *sizes,
			  unsigned short *kinds, goacc_aq aq)
{
  gomp_mutex_lock (&acc_dev->lock);

  /* Handle "detach" before copyback/deletion of mapped data.  */
  for (size_t i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;
      bool finalize = false;
      switch (kind)
	{
	case GOMP_MAP_FORCE_DETACH:
	  finalize = true;
	  /* Fallthrough.  */

	case GOMP_MAP_DETACH:
	  {
	    struct splay_tree_key_s cur_node;
	    uintptr_t hostaddr = (uintptr_t) hostaddrs[i];
	    cur_node.host_start = hostaddr;
	    cur_node.host_end = cur_node.host_start + sizeof (void *);
	    splay_tree_key n
	      = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

	    if (n == NULL)
	      {
		gomp_mutex_unlock (&acc_dev->lock);
		gomp_fatal ("struct not mapped for detach operation");
	      }

	    gomp_detach_pointer (acc_dev, aq, n, hostaddr, finalize, NULL);
	  }
	  break;
	default:
	  ;
	}
    }

  for (size_t i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;

      switch (kind)
	{
	case GOMP_MAP_FROM:
	case GOMP_MAP_FORCE_FROM:
	case GOMP_MAP_TO_PSET:
	case GOMP_MAP_POINTER:
	case GOMP_MAP_DELETE:
	case GOMP_MAP_RELEASE:
	case GOMP_MAP_DETACH:
	case GOMP_MAP_FORCE_DETACH:
	  {
	    struct splay_tree_key_s cur_node;
	    size_t size;
	    if (kind == GOMP_MAP_POINTER
		|| kind == GOMP_MAP_DETACH
		|| kind == GOMP_MAP_FORCE_DETACH)
	      size = sizeof (void *);
	    else
	      size = sizes[i];
	    cur_node.host_start = (uintptr_t) hostaddrs[i];
	    cur_node.host_end = cur_node.host_start + size;
	    splay_tree_key n
	      = splay_tree_lookup (&acc_dev->mem_map, &cur_node);

	    if (n == NULL)
	      continue;

	    goacc_exit_datum_1 (acc_dev, hostaddrs[i], size, kind, n, aq);
	  }
	  break;

	case GOMP_MAP_STRUCT:
	  /* Skip the 'GOMP_MAP_STRUCT' itself, and use the regular processing
	     for all its entries.  This special handling exists for GCC 10.1
	     compatibility; afterwards, we're not generating these no-op
	     'GOMP_MAP_STRUCT's anymore.  */
	  break;

	default:
	  gomp_fatal (">>>> goacc_exit_data_internal UNHANDLED kind 0x%.2x",
			  kind);
	}
    }

  gomp_mutex_unlock (&acc_dev->lock);
}

void
GOACC_enter_exit_data (int flags_m, size_t mapnum, void **hostaddrs,
		       size_t *sizes, unsigned short *kinds, int async,
		       int num_waits, ...)
{
  int flags = GOACC_FLAGS_UNMARSHAL (flags_m);

  struct goacc_thread *thr;
  struct gomp_device_descr *acc_dev;
  bool data_enter = false;
  size_t i;

  goacc_lazy_initialize ();

  thr = goacc_thread ();
  acc_dev = thr->dev;

  /* Determine if this is an "acc enter data".  */
  for (i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;

      if (kind == GOMP_MAP_POINTER
	  || kind == GOMP_MAP_TO_PSET
	  || kind == GOMP_MAP_STRUCT)
	continue;

      if (kind == GOMP_MAP_FORCE_ALLOC
	  || kind == GOMP_MAP_FORCE_PRESENT
	  || kind == GOMP_MAP_ATTACH
	  || kind == GOMP_MAP_FORCE_TO
	  || kind == GOMP_MAP_TO
	  || kind == GOMP_MAP_ALLOC)
	{
	  data_enter = true;
	  break;
	}

      if (kind == GOMP_MAP_RELEASE
	  || kind == GOMP_MAP_DELETE
	  || kind == GOMP_MAP_DETACH
	  || kind == GOMP_MAP_FORCE_DETACH
	  || kind == GOMP_MAP_FROM
	  || kind == GOMP_MAP_FORCE_FROM)
	break;

      gomp_fatal (">>>> GOACC_enter_exit_data UNHANDLED kind 0x%.2x",
		      kind);
    }

  bool profiling_p = GOACC_PROFILING_DISPATCH_P (true);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      thr->prof_info = &prof_info;

      prof_info.event_type
	= data_enter ? acc_ev_enter_data_start : acc_ev_exit_data_start;
      prof_info.valid_bytes = _ACC_PROF_INFO_VALID_BYTES;
      prof_info.version = _ACC_PROF_INFO_VERSION;
      prof_info.device_type = acc_device_type (acc_dev->type);
      prof_info.device_number = acc_dev->target_id;
      prof_info.thread_id = -1;
      prof_info.async = async;
      prof_info.async_queue = prof_info.async;
      prof_info.src_file = NULL;
      prof_info.func_name = NULL;
      prof_info.line_no = -1;
      prof_info.end_line_no = -1;
      prof_info.func_line_no = -1;
      prof_info.func_end_line_no = -1;
    }
  acc_event_info enter_exit_data_event_info;
  if (profiling_p)
    {
      enter_exit_data_event_info.other_event.event_type
	= prof_info.event_type;
      enter_exit_data_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      enter_exit_data_event_info.other_event.parent_construct
	= data_enter ? acc_construct_enter_data : acc_construct_exit_data;
      enter_exit_data_event_info.other_event.implicit = 0;
      enter_exit_data_event_info.other_event.tool_info = NULL;
    }
  acc_api_info api_info;
  if (profiling_p)
    {
      thr->api_info = &api_info;

      api_info.device_api = acc_device_api_none;
      api_info.valid_bytes = _ACC_API_INFO_VALID_BYTES;
      api_info.device_type = prof_info.device_type;
      api_info.vendor = -1;
      api_info.device_handle = NULL;
      api_info.context_handle = NULL;
      api_info.async_handle = NULL;
    }

  if (profiling_p)
    goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
			      &api_info);

  if ((acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
      || (flags & GOACC_FLAG_HOST_FALLBACK))
    {
      prof_info.device_type = acc_device_host;
      api_info.device_type = prof_info.device_type;

      goto out_prof;
    }

  if (num_waits)
    {
      va_list ap;

      va_start (ap, num_waits);
      goacc_wait (async, num_waits, &ap);
      va_end (ap);
    }

  goacc_aq aq = get_goacc_asyncqueue (async);

  if (data_enter)
    goacc_enter_data_internal (acc_dev, mapnum, hostaddrs, sizes, kinds, aq);
  else
    goacc_exit_data_internal (acc_dev, mapnum, hostaddrs, sizes, kinds, aq);

 out_prof:
  if (profiling_p)
    {
      prof_info.event_type
	= data_enter ? acc_ev_enter_data_end : acc_ev_exit_data_end;
      enter_exit_data_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
				&api_info);

      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}
