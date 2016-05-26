/* Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

/* This file contains the support of offloading.  */

#include "config.h"
#include "libgomp.h"
#include "oacc-plugin.h"
#include "oacc-int.h"
#include "gomp-constants.h"
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIu64.  */
#endif
#include <string.h>
#include <assert.h>
#include <errno.h>

#ifdef PLUGIN_SUPPORT
#include <dlfcn.h>
#include "plugin-suffix.h"
#endif

static void gomp_target_init (void);

/* The whole initialization code for offloading plugins is only run one.  */
static pthread_once_t gomp_is_initialized = PTHREAD_ONCE_INIT;

/* Mutex for offload image registration.  */
static gomp_mutex_t register_lock;

/* This structure describes an offload image.
   It contains type of the target device, pointer to host table descriptor, and
   pointer to target data.  */
struct offload_image_descr {
  unsigned version;
  enum offload_target_type type;
  const void *host_table;
  const void *target_data;
};

/* Array of descriptors of offload images.  */
static struct offload_image_descr *offload_images;

/* Total number of offload images.  */
static int num_offload_images;

/* Array of descriptors for all available devices.  */
static struct gomp_device_descr *devices;

/* Total number of available devices.  */
static int num_devices;

/* Number of GOMP_OFFLOAD_CAP_OPENMP_400 devices.  */
static int num_devices_openmp;

/* Similar to gomp_realloc, but release register_lock before gomp_fatal.  */

static void *
gomp_realloc_unlock (void *old, size_t size)
{
  void *ret = realloc (old, size);
  if (ret == NULL)
    {
      gomp_mutex_unlock (&register_lock);
      gomp_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
    }
  return ret;
}

attribute_hidden void
gomp_init_targets_once (void)
{
  (void) pthread_once (&gomp_is_initialized, gomp_target_init);
}

attribute_hidden int
gomp_get_num_devices (void)
{
  gomp_init_targets_once ();
  return num_devices_openmp;
}

static struct gomp_device_descr *
resolve_device (int device_id)
{
  if (device_id == GOMP_DEVICE_ICV)
    {
      struct gomp_task_icv *icv = gomp_icv (false);
      device_id = icv->default_device_var;
    }

  if (device_id < 0 || device_id >= gomp_get_num_devices ())
    return NULL;

  gomp_mutex_lock (&devices[device_id].lock);
  if (devices[device_id].state == GOMP_DEVICE_UNINITIALIZED)
    gomp_init_device (&devices[device_id]);
  else if (devices[device_id].state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devices[device_id].lock);
      return NULL;
    }
  gomp_mutex_unlock (&devices[device_id].lock);

  return &devices[device_id];
}


static inline splay_tree_key
gomp_map_lookup (splay_tree mem_map, splay_tree_key key)
{
  if (key->host_start != key->host_end)
    return splay_tree_lookup (mem_map, key);

  key->host_end++;
  splay_tree_key n = splay_tree_lookup (mem_map, key);
  key->host_end--;
  if (n)
    return n;
  key->host_start--;
  n = splay_tree_lookup (mem_map, key);
  key->host_start++;
  if (n)
    return n;
  return splay_tree_lookup (mem_map, key);
}

static inline splay_tree_key
gomp_map_0len_lookup (splay_tree mem_map, splay_tree_key key)
{
  if (key->host_start != key->host_end)
    return splay_tree_lookup (mem_map, key);

  key->host_end++;
  splay_tree_key n = splay_tree_lookup (mem_map, key);
  key->host_end--;
  return n;
}

static inline void
gomp_device_copy (struct gomp_device_descr *devicep,
		  bool (*copy_func) (int, void *, const void *, size_t),
		  const char *dst, void *dstaddr,
		  const char *src, const void *srcaddr,
		  size_t size)
{
  if (!copy_func (devicep->target_id, dstaddr, srcaddr, size))
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("Copying of %s object [%p..%p) to %s object [%p..%p) failed",
		  src, srcaddr, srcaddr + size, dst, dstaddr, dstaddr + size);
    }
}

static void
gomp_copy_host2dev (struct gomp_device_descr *devicep,
		    void *d, const void *h, size_t sz)
{
  gomp_device_copy (devicep, devicep->host2dev_func, "dev", d, "host", h, sz);
}

static void
gomp_copy_dev2host (struct gomp_device_descr *devicep,
		    void *h, const void *d, size_t sz)
{
  gomp_device_copy (devicep, devicep->dev2host_func, "host", h, "dev", d, sz);
}

static void
gomp_free_device_memory (struct gomp_device_descr *devicep, void *devptr)
{
  if (!devicep->free_func (devicep->target_id, devptr))
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("error in freeing device memory block at %p", devptr);
    }
}

/* Handle the case where gomp_map_lookup, splay_tree_lookup or
   gomp_map_0len_lookup found oldn for newn.
   Helper function of gomp_map_vars.  */

static inline void
gomp_map_vars_existing (struct gomp_device_descr *devicep, splay_tree_key oldn,
			splay_tree_key newn, struct target_var_desc *tgt_var,
			unsigned char kind)
{
  tgt_var->key = oldn;
  tgt_var->copy_from = GOMP_MAP_COPY_FROM_P (kind);
  tgt_var->always_copy_from = GOMP_MAP_ALWAYS_FROM_P (kind);
  tgt_var->offset = newn->host_start - oldn->host_start;
  tgt_var->length = newn->host_end - newn->host_start;

  if ((kind & GOMP_MAP_FLAG_FORCE)
      || oldn->host_start > newn->host_start
      || oldn->host_end < newn->host_end)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("Trying to map into device [%p..%p) object when "
		  "[%p..%p) is already mapped",
		  (void *) newn->host_start, (void *) newn->host_end,
		  (void *) oldn->host_start, (void *) oldn->host_end);
    }

  if (GOMP_MAP_ALWAYS_TO_P (kind))
    gomp_copy_host2dev (devicep,
			(void *) (oldn->tgt->tgt_start + oldn->tgt_offset
				  + newn->host_start - oldn->host_start),
			(void *) newn->host_start,
			newn->host_end - newn->host_start);

  if (oldn->refcount != REFCOUNT_INFINITY)
    oldn->refcount++;
}

static int
get_kind (bool short_mapkind, void *kinds, int idx)
{
  return short_mapkind ? ((unsigned short *) kinds)[idx]
		       : ((unsigned char *) kinds)[idx];
}

static void
gomp_map_pointer (struct target_mem_desc *tgt, uintptr_t host_ptr,
		  uintptr_t target_offset, uintptr_t bias)
{
  struct gomp_device_descr *devicep = tgt->device_descr;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;

  cur_node.host_start = host_ptr;
  if (cur_node.host_start == (uintptr_t) NULL)
    {
      cur_node.tgt_offset = (uintptr_t) NULL;
      /* FIXME: see comment about coalescing host/dev transfers below.  */
      gomp_copy_host2dev (devicep,
			  (void *) (tgt->tgt_start + target_offset),
			  (void *) &cur_node.tgt_offset,
			  sizeof (void *));
      return;
    }
  /* Add bias to the pointer value.  */
  cur_node.host_start += bias;
  cur_node.host_end = cur_node.host_start;
  splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
  if (n == NULL)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("Pointer target of array section wasn't mapped");
    }
  cur_node.host_start -= n->host_start;
  cur_node.tgt_offset
    = n->tgt->tgt_start + n->tgt_offset + cur_node.host_start;
  /* At this point tgt_offset is target address of the
     array section.  Now subtract bias to get what we want
     to initialize the pointer with.  */
  cur_node.tgt_offset -= bias;
  /* FIXME: see comment about coalescing host/dev transfers below.  */
  gomp_copy_host2dev (devicep, (void *) (tgt->tgt_start + target_offset),
		      (void *) &cur_node.tgt_offset, sizeof (void *));
}

static void
gomp_map_fields_existing (struct target_mem_desc *tgt, splay_tree_key n,
			  size_t first, size_t i, void **hostaddrs,
			  size_t *sizes, void *kinds)
{
  struct gomp_device_descr *devicep = tgt->device_descr;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  int kind;
  const bool short_mapkind = true;
  const int typemask = short_mapkind ? 0xff : 0x7;

  cur_node.host_start = (uintptr_t) hostaddrs[i];
  cur_node.host_end = cur_node.host_start + sizes[i];
  splay_tree_key n2 = splay_tree_lookup (mem_map, &cur_node);
  kind = get_kind (short_mapkind, kinds, i);
  if (n2
      && n2->tgt == n->tgt
      && n2->host_start - n->host_start == n2->tgt_offset - n->tgt_offset)
    {
      gomp_map_vars_existing (devicep, n2, &cur_node,
			      &tgt->list[i], kind & typemask);
      return;
    }
  if (sizes[i] == 0)
    {
      if (cur_node.host_start > (uintptr_t) hostaddrs[first - 1])
	{
	  cur_node.host_start--;
	  n2 = splay_tree_lookup (mem_map, &cur_node);
	  cur_node.host_start++;
	  if (n2
	      && n2->tgt == n->tgt
	      && n2->host_start - n->host_start
		 == n2->tgt_offset - n->tgt_offset)
	    {
	      gomp_map_vars_existing (devicep, n2, &cur_node, &tgt->list[i],
				      kind & typemask);
	      return;
	    }
	}
      cur_node.host_end++;
      n2 = splay_tree_lookup (mem_map, &cur_node);
      cur_node.host_end--;
      if (n2
	  && n2->tgt == n->tgt
	  && n2->host_start - n->host_start == n2->tgt_offset - n->tgt_offset)
	{
	  gomp_map_vars_existing (devicep, n2, &cur_node, &tgt->list[i],
				  kind & typemask);
	  return;
	}
    }
  gomp_mutex_unlock (&devicep->lock);
  gomp_fatal ("Trying to map into device [%p..%p) structure element when "
	      "other mapped elements from the same structure weren't mapped "
	      "together with it", (void *) cur_node.host_start,
	      (void *) cur_node.host_end);
}

static inline uintptr_t
gomp_map_val (struct target_mem_desc *tgt, void **hostaddrs, size_t i)
{
  if (tgt->list[i].key != NULL)
    return tgt->list[i].key->tgt->tgt_start
	   + tgt->list[i].key->tgt_offset
	   + tgt->list[i].offset;
  if (tgt->list[i].offset == ~(uintptr_t) 0)
    return (uintptr_t) hostaddrs[i];
  if (tgt->list[i].offset == ~(uintptr_t) 1)
    return 0;
  if (tgt->list[i].offset == ~(uintptr_t) 2)
    return tgt->list[i + 1].key->tgt->tgt_start
	   + tgt->list[i + 1].key->tgt_offset
	   + tgt->list[i + 1].offset
	   + (uintptr_t) hostaddrs[i]
	   - (uintptr_t) hostaddrs[i + 1];
  return tgt->tgt_start + tgt->list[i].offset;
}

attribute_hidden struct target_mem_desc *
gomp_map_vars (struct gomp_device_descr *devicep, size_t mapnum,
	       void **hostaddrs, void **devaddrs, size_t *sizes, void *kinds,
	       bool short_mapkind, enum gomp_map_vars_kind pragma_kind)
{
  size_t i, tgt_align, tgt_size, not_found_cnt = 0;
  bool has_firstprivate = false;
  const int rshift = short_mapkind ? 8 : 3;
  const int typemask = short_mapkind ? 0xff : 0x7;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  struct target_mem_desc *tgt
    = gomp_malloc (sizeof (*tgt) + sizeof (tgt->list[0]) * mapnum);
  tgt->list_count = mapnum;
  tgt->refcount = pragma_kind == GOMP_MAP_VARS_ENTER_DATA ? 0 : 1;
  tgt->device_descr = devicep;

  if (mapnum == 0)
    {
      tgt->tgt_start = 0;
      tgt->tgt_end = 0;
      return tgt;
    }

  tgt_align = sizeof (void *);
  tgt_size = 0;
  if (pragma_kind == GOMP_MAP_VARS_TARGET)
    {
      size_t align = 4 * sizeof (void *);
      tgt_align = align;
      tgt_size = mapnum * sizeof (void *);
    }

  gomp_mutex_lock (&devicep->lock);
  if (devicep->state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devicep->lock);
      free (tgt);
      return NULL;
    }

  for (i = 0; i < mapnum; i++)
    {
      int kind = get_kind (short_mapkind, kinds, i);
      if (hostaddrs[i] == NULL
	  || (kind & typemask) == GOMP_MAP_FIRSTPRIVATE_INT)
	{
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = ~(uintptr_t) 0;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_USE_DEVICE_PTR)
	{
	  cur_node.host_start = (uintptr_t) hostaddrs[i];
	  cur_node.host_end = cur_node.host_start;
	  splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
	  if (n == NULL)
	    {
	      gomp_mutex_unlock (&devicep->lock);
	      gomp_fatal ("use_device_ptr pointer wasn't mapped");
	    }
	  cur_node.host_start -= n->host_start;
	  hostaddrs[i]
	    = (void *) (n->tgt->tgt_start + n->tgt_offset
			+ cur_node.host_start);
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = ~(uintptr_t) 0;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_STRUCT)
	{
	  size_t first = i + 1;
	  size_t last = i + sizes[i];
	  cur_node.host_start = (uintptr_t) hostaddrs[i];
	  cur_node.host_end = (uintptr_t) hostaddrs[last]
			      + sizes[last];
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = ~(uintptr_t) 2;
	  splay_tree_key n = splay_tree_lookup (mem_map, &cur_node);
	  if (n == NULL)
	    {
	      size_t align = (size_t) 1 << (kind >> rshift);
	      if (tgt_align < align)
		tgt_align = align;
	      tgt_size -= (uintptr_t) hostaddrs[first]
			  - (uintptr_t) hostaddrs[i];
	      tgt_size = (tgt_size + align - 1) & ~(align - 1);
	      tgt_size += cur_node.host_end - (uintptr_t) hostaddrs[i];
	      not_found_cnt += last - i;
	      for (i = first; i <= last; i++)
		tgt->list[i].key = NULL;
	      i--;
	      continue;
	    }
	  for (i = first; i <= last; i++)
	    gomp_map_fields_existing (tgt, n, first, i, hostaddrs,
				      sizes, kinds);
	  i--;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_ALWAYS_POINTER)
	{
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = ~(uintptr_t) 1;
	  has_firstprivate = true;
	  continue;
	}
      cur_node.host_start = (uintptr_t) hostaddrs[i];
      if (!GOMP_MAP_POINTER_P (kind & typemask))
	cur_node.host_end = cur_node.host_start + sizes[i];
      else
	cur_node.host_end = cur_node.host_start + sizeof (void *);
      if ((kind & typemask) == GOMP_MAP_FIRSTPRIVATE)
	{
	  tgt->list[i].key = NULL;

	  size_t align = (size_t) 1 << (kind >> rshift);
	  if (tgt_align < align)
	    tgt_align = align;
	  tgt_size = (tgt_size + align - 1) & ~(align - 1);
	  tgt_size += cur_node.host_end - cur_node.host_start;
	  has_firstprivate = true;
	  continue;
	}
      splay_tree_key n;
      if ((kind & typemask) == GOMP_MAP_ZERO_LEN_ARRAY_SECTION)
	{
	  n = gomp_map_0len_lookup (mem_map, &cur_node);
	  if (!n)
	    {
	      tgt->list[i].key = NULL;
	      tgt->list[i].offset = ~(uintptr_t) 1;
	      continue;
	    }
	}
      else
	n = splay_tree_lookup (mem_map, &cur_node);
      if (n && n->refcount != REFCOUNT_LINK)
	gomp_map_vars_existing (devicep, n, &cur_node, &tgt->list[i],
				kind & typemask);
      else
	{
	  tgt->list[i].key = NULL;

	  size_t align = (size_t) 1 << (kind >> rshift);
	  not_found_cnt++;
	  if (tgt_align < align)
	    tgt_align = align;
	  tgt_size = (tgt_size + align - 1) & ~(align - 1);
	  tgt_size += cur_node.host_end - cur_node.host_start;
	  if ((kind & typemask) == GOMP_MAP_TO_PSET)
	    {
	      size_t j;
	      for (j = i + 1; j < mapnum; j++)
		if (!GOMP_MAP_POINTER_P (get_kind (short_mapkind, kinds, j)
					 & typemask))
		  break;
		else if ((uintptr_t) hostaddrs[j] < cur_node.host_start
			 || ((uintptr_t) hostaddrs[j] + sizeof (void *)
			     > cur_node.host_end))
		  break;
		else
		  {
		    tgt->list[j].key = NULL;
		    i++;
		  }
	    }
	}
    }

  if (devaddrs)
    {
      if (mapnum != 1)
	{
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("unexpected aggregation");
	}
      tgt->to_free = devaddrs[0];
      tgt->tgt_start = (uintptr_t) tgt->to_free;
      tgt->tgt_end = tgt->tgt_start + sizes[0];
    }
  else if (not_found_cnt || pragma_kind == GOMP_MAP_VARS_TARGET)
    {
      /* Allocate tgt_align aligned tgt_size block of memory.  */
      /* FIXME: Perhaps change interface to allocate properly aligned
	 memory.  */
      tgt->to_free = devicep->alloc_func (devicep->target_id,
					  tgt_size + tgt_align - 1);
      if (!tgt->to_free)
	{
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("device memory allocation fail");
	}

      tgt->tgt_start = (uintptr_t) tgt->to_free;
      tgt->tgt_start = (tgt->tgt_start + tgt_align - 1) & ~(tgt_align - 1);
      tgt->tgt_end = tgt->tgt_start + tgt_size;
    }
  else
    {
      tgt->to_free = NULL;
      tgt->tgt_start = 0;
      tgt->tgt_end = 0;
    }

  tgt_size = 0;
  if (pragma_kind == GOMP_MAP_VARS_TARGET)
    tgt_size = mapnum * sizeof (void *);

  tgt->array = NULL;
  if (not_found_cnt || has_firstprivate)
    {
      if (not_found_cnt)
	tgt->array = gomp_malloc (not_found_cnt * sizeof (*tgt->array));
      splay_tree_node array = tgt->array;
      size_t j, field_tgt_offset = 0, field_tgt_clear = ~(size_t) 0;
      uintptr_t field_tgt_base = 0;

      for (i = 0; i < mapnum; i++)
	if (tgt->list[i].key == NULL)
	  {
	    int kind = get_kind (short_mapkind, kinds, i);
	    if (hostaddrs[i] == NULL)
	      continue;
	    switch (kind & typemask)
	      {
		size_t align, len, first, last;
		splay_tree_key n;
	      case GOMP_MAP_FIRSTPRIVATE:
		align = (size_t) 1 << (kind >> rshift);
		tgt_size = (tgt_size + align - 1) & ~(align - 1);
		tgt->list[i].offset = tgt_size;
		len = sizes[i];
		gomp_copy_host2dev (devicep,
				    (void *) (tgt->tgt_start + tgt_size),
				    (void *) hostaddrs[i], len);
		tgt_size += len;
		continue;
	      case GOMP_MAP_FIRSTPRIVATE_INT:
	      case GOMP_MAP_USE_DEVICE_PTR:
	      case GOMP_MAP_ZERO_LEN_ARRAY_SECTION:
		continue;
	      case GOMP_MAP_STRUCT:
		first = i + 1;
		last = i + sizes[i];
		cur_node.host_start = (uintptr_t) hostaddrs[i];
		cur_node.host_end = (uintptr_t) hostaddrs[last]
				    + sizes[last];
		if (tgt->list[first].key != NULL)
		  continue;
		n = splay_tree_lookup (mem_map, &cur_node);
		if (n == NULL)
		  {
		    size_t align = (size_t) 1 << (kind >> rshift);
		    tgt_size -= (uintptr_t) hostaddrs[first]
				- (uintptr_t) hostaddrs[i];
		    tgt_size = (tgt_size + align - 1) & ~(align - 1);
		    tgt_size += (uintptr_t) hostaddrs[first]
				- (uintptr_t) hostaddrs[i];
		    field_tgt_base = (uintptr_t) hostaddrs[first];
		    field_tgt_offset = tgt_size;
		    field_tgt_clear = last;
		    tgt_size += cur_node.host_end
				- (uintptr_t) hostaddrs[first];
		    continue;
		  }
		for (i = first; i <= last; i++)
		  gomp_map_fields_existing (tgt, n, first, i, hostaddrs,
					    sizes, kinds);
		i--;
		continue;
	      case GOMP_MAP_ALWAYS_POINTER:
		cur_node.host_start = (uintptr_t) hostaddrs[i];
		cur_node.host_end = cur_node.host_start + sizeof (void *);
		n = splay_tree_lookup (mem_map, &cur_node);
		if (n == NULL
		    || n->host_start > cur_node.host_start
		    || n->host_end < cur_node.host_end)
		  {
		    gomp_mutex_unlock (&devicep->lock);
		    gomp_fatal ("always pointer not mapped");
		  }
		if ((get_kind (short_mapkind, kinds, i - 1) & typemask)
		    != GOMP_MAP_ALWAYS_POINTER)
		  cur_node.tgt_offset = gomp_map_val (tgt, hostaddrs, i - 1);
		if (cur_node.tgt_offset)
		  cur_node.tgt_offset -= sizes[i];
		gomp_copy_host2dev (devicep,
				    (void *) (n->tgt->tgt_start
					      + n->tgt_offset
					      + cur_node.host_start
					      - n->host_start),
				    (void *) &cur_node.tgt_offset,
				    sizeof (void *));
		cur_node.tgt_offset = n->tgt->tgt_start + n->tgt_offset
				      + cur_node.host_start - n->host_start;
		continue;
	      default:
		break;
	      }
	    splay_tree_key k = &array->key;
	    k->host_start = (uintptr_t) hostaddrs[i];
	    if (!GOMP_MAP_POINTER_P (kind & typemask))
	      k->host_end = k->host_start + sizes[i];
	    else
	      k->host_end = k->host_start + sizeof (void *);
	    splay_tree_key n = splay_tree_lookup (mem_map, k);
	    if (n && n->refcount != REFCOUNT_LINK)
	      gomp_map_vars_existing (devicep, n, k, &tgt->list[i],
				      kind & typemask);
	    else
	      {
		k->link_key = NULL;
		if (n && n->refcount == REFCOUNT_LINK)
		  {
		    /* Replace target address of the pointer with target address
		       of mapped object in the splay tree.  */
		    splay_tree_remove (mem_map, n);
		    k->link_key = n;
		  }
		size_t align = (size_t) 1 << (kind >> rshift);
		tgt->list[i].key = k;
		k->tgt = tgt;
		if (field_tgt_clear != ~(size_t) 0)
		  {
		    k->tgt_offset = k->host_start - field_tgt_base
				    + field_tgt_offset;
		    if (i == field_tgt_clear)
		      field_tgt_clear = ~(size_t) 0;
		  }
		else
		  {
		    tgt_size = (tgt_size + align - 1) & ~(align - 1);
		    k->tgt_offset = tgt_size;
		    tgt_size += k->host_end - k->host_start;
		  }
		tgt->list[i].copy_from = GOMP_MAP_COPY_FROM_P (kind & typemask);
		tgt->list[i].always_copy_from
		  = GOMP_MAP_ALWAYS_FROM_P (kind & typemask);
		tgt->list[i].offset = 0;
		tgt->list[i].length = k->host_end - k->host_start;
		k->refcount = 1;
		tgt->refcount++;
		array->left = NULL;
		array->right = NULL;
		splay_tree_insert (mem_map, array);
		switch (kind & typemask)
		  {
		  case GOMP_MAP_ALLOC:
		  case GOMP_MAP_FROM:
		  case GOMP_MAP_FORCE_ALLOC:
		  case GOMP_MAP_FORCE_FROM:
		  case GOMP_MAP_ALWAYS_FROM:
		    break;
		  case GOMP_MAP_TO:
		  case GOMP_MAP_TOFROM:
		  case GOMP_MAP_FORCE_TO:
		  case GOMP_MAP_FORCE_TOFROM:
		  case GOMP_MAP_ALWAYS_TO:
		  case GOMP_MAP_ALWAYS_TOFROM:
		    /* FIXME: Perhaps add some smarts, like if copying
		       several adjacent fields from host to target, use some
		       host buffer to avoid sending each var individually.  */
		    gomp_copy_host2dev (devicep,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					k->host_end - k->host_start);
		    break;
		  case GOMP_MAP_POINTER:
		    gomp_map_pointer (tgt, (uintptr_t) *(void **) k->host_start,
				      k->tgt_offset, sizes[i]);
		    break;
		  case GOMP_MAP_TO_PSET:
		    /* FIXME: see above FIXME comment.  */
		    gomp_copy_host2dev (devicep,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					k->host_end - k->host_start);

		    for (j = i + 1; j < mapnum; j++)
		      if (!GOMP_MAP_POINTER_P (get_kind (short_mapkind, kinds,
							 j)
					       & typemask))
			break;
		      else if ((uintptr_t) hostaddrs[j] < k->host_start
			       || ((uintptr_t) hostaddrs[j] + sizeof (void *)
				   > k->host_end))
			break;
		      else
			{
			  tgt->list[j].key = k;
			  tgt->list[j].copy_from = false;
			  tgt->list[j].always_copy_from = false;
			  if (k->refcount != REFCOUNT_INFINITY)
			    k->refcount++;
			  gomp_map_pointer (tgt,
					    (uintptr_t) *(void **) hostaddrs[j],
					    k->tgt_offset
					    + ((uintptr_t) hostaddrs[j]
					       - k->host_start),
					    sizes[j]);
			  i++;
			}
		    break;
		  case GOMP_MAP_FORCE_PRESENT:
		    {
		      /* We already looked up the memory region above and it
			 was missing.  */
		      size_t size = k->host_end - k->host_start;
		      gomp_mutex_unlock (&devicep->lock);
#ifdef HAVE_INTTYPES_H
		      gomp_fatal ("present clause: !acc_is_present (%p, "
				  "%"PRIu64" (0x%"PRIx64"))",
				  (void *) k->host_start,
				  (uint64_t) size, (uint64_t) size);
#else
		      gomp_fatal ("present clause: !acc_is_present (%p, "
				  "%lu (0x%lx))", (void *) k->host_start,
				  (unsigned long) size, (unsigned long) size);
#endif
		    }
		    break;
		  case GOMP_MAP_FORCE_DEVICEPTR:
		    assert (k->host_end - k->host_start == sizeof (void *));
		    gomp_copy_host2dev (devicep,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					sizeof (void *));
		    break;
		  default:
		    gomp_mutex_unlock (&devicep->lock);
		    gomp_fatal ("%s: unhandled kind 0x%.2x", __FUNCTION__,
				kind);
		  }

		if (k->link_key)
		  {
		    /* Set link pointer on target to the device address of the
		       mapped object.  */
		    void *tgt_addr = (void *) (tgt->tgt_start + k->tgt_offset);
		    devicep->host2dev_func (devicep->target_id,
					    (void *) n->tgt_offset,
					    &tgt_addr, sizeof (void *));
		  }
		array++;
	      }
	  }
    }

  if (pragma_kind == GOMP_MAP_VARS_TARGET)
    {
      for (i = 0; i < mapnum; i++)
	{
	  cur_node.tgt_offset = gomp_map_val (tgt, hostaddrs, i);
	  /* FIXME: see above FIXME comment.  */
	  gomp_copy_host2dev (devicep,
			      (void *) (tgt->tgt_start + i * sizeof (void *)),
			      (void *) &cur_node.tgt_offset, sizeof (void *));
	}
    }

  /* If the variable from "omp target enter data" map-list was already mapped,
     tgt is not needed.  Otherwise tgt will be freed by gomp_unmap_vars or
     gomp_exit_data.  */
  if (pragma_kind == GOMP_MAP_VARS_ENTER_DATA && tgt->refcount == 0)
    {
      free (tgt);
      tgt = NULL;
    }

  gomp_mutex_unlock (&devicep->lock);
  return tgt;
}

static void
gomp_unmap_tgt (struct target_mem_desc *tgt)
{
  /* Deallocate on target the tgt->tgt_start .. tgt->tgt_end region.  */
  if (tgt->tgt_end)
    gomp_free_device_memory (tgt->device_descr, tgt->to_free);

  free (tgt->array);
  free (tgt);
}

/* Unmap variables described by TGT.  If DO_COPYFROM is true, copy relevant
   variables back from device to host: if it is false, it is assumed that this
   has been done already.  */

attribute_hidden void
gomp_unmap_vars (struct target_mem_desc *tgt, bool do_copyfrom)
{
  struct gomp_device_descr *devicep = tgt->device_descr;

  if (tgt->list_count == 0)
    {
      free (tgt);
      return;
    }

  gomp_mutex_lock (&devicep->lock);
  if (devicep->state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devicep->lock);
      free (tgt->array);
      free (tgt);
      return;
    }

  size_t i;
  for (i = 0; i < tgt->list_count; i++)
    {
      splay_tree_key k = tgt->list[i].key;
      if (k == NULL)
	continue;

      bool do_unmap = false;
      if (k->refcount > 1 && k->refcount != REFCOUNT_INFINITY)
	k->refcount--;
      else if (k->refcount == 1)
	{
	  k->refcount--;
	  do_unmap = true;
	}

      if ((do_unmap && do_copyfrom && tgt->list[i].copy_from)
	  || tgt->list[i].always_copy_from)
	gomp_copy_dev2host (devicep,
			    (void *) (k->host_start + tgt->list[i].offset),
			    (void *) (k->tgt->tgt_start + k->tgt_offset
				      + tgt->list[i].offset),
			    tgt->list[i].length);
      if (do_unmap)
	{
	  splay_tree_remove (&devicep->mem_map, k);
	  if (k->link_key)
	    splay_tree_insert (&devicep->mem_map,
			       (splay_tree_node) k->link_key);
	  if (k->tgt->refcount > 1)
	    k->tgt->refcount--;
	  else
	    gomp_unmap_tgt (k->tgt);
	}
    }

  if (tgt->refcount > 1)
    tgt->refcount--;
  else
    gomp_unmap_tgt (tgt);

  gomp_mutex_unlock (&devicep->lock);
}

static void
gomp_update (struct gomp_device_descr *devicep, size_t mapnum, void **hostaddrs,
	     size_t *sizes, void *kinds, bool short_mapkind)
{
  size_t i;
  struct splay_tree_key_s cur_node;
  const int typemask = short_mapkind ? 0xff : 0x7;

  if (!devicep)
    return;

  if (mapnum == 0)
    return;

  gomp_mutex_lock (&devicep->lock);
  if (devicep->state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devicep->lock);
      return;
    }

  for (i = 0; i < mapnum; i++)
    if (sizes[i])
      {
	cur_node.host_start = (uintptr_t) hostaddrs[i];
	cur_node.host_end = cur_node.host_start + sizes[i];
	splay_tree_key n = splay_tree_lookup (&devicep->mem_map, &cur_node);
	if (n)
	  {
	    int kind = get_kind (short_mapkind, kinds, i);
	    if (n->host_start > cur_node.host_start
		|| n->host_end < cur_node.host_end)
	      {
		gomp_mutex_unlock (&devicep->lock);
		gomp_fatal ("Trying to update [%p..%p) object when "
			    "only [%p..%p) is mapped",
			    (void *) cur_node.host_start,
			    (void *) cur_node.host_end,
			    (void *) n->host_start,
			    (void *) n->host_end);
	      }


	    void *hostaddr = (void *) cur_node.host_start;
	    void *devaddr = (void *) (n->tgt->tgt_start + n->tgt_offset
				      + cur_node.host_start - n->host_start);
	    size_t size = cur_node.host_end - cur_node.host_start;

	    if (GOMP_MAP_COPY_TO_P (kind & typemask))
	      gomp_copy_host2dev (devicep, devaddr, hostaddr, size);
	    if (GOMP_MAP_COPY_FROM_P (kind & typemask))
	      gomp_copy_dev2host (devicep, hostaddr, devaddr, size);
	  }
      }
  gomp_mutex_unlock (&devicep->lock);
}

/* Load image pointed by TARGET_DATA to the device, specified by DEVICEP.
   And insert to splay tree the mapping between addresses from HOST_TABLE and
   from loaded target image.  We rely in the host and device compiler
   emitting variable and functions in the same order.  */

static void
gomp_load_image_to_device (struct gomp_device_descr *devicep, unsigned version,
			   const void *host_table, const void *target_data,
			   bool is_register_lock)
{
  void **host_func_table = ((void ***) host_table)[0];
  void **host_funcs_end  = ((void ***) host_table)[1];
  void **host_var_table  = ((void ***) host_table)[2];
  void **host_vars_end   = ((void ***) host_table)[3];

  /* The func table contains only addresses, the var table contains addresses
     and corresponding sizes.  */
  int num_funcs = host_funcs_end - host_func_table;
  int num_vars  = (host_vars_end - host_var_table) / 2;

  /* Load image to device and get target addresses for the image.  */
  struct addr_pair *target_table = NULL;
  int i, num_target_entries;

  num_target_entries
    = devicep->load_image_func (devicep->target_id, version,
				target_data, &target_table);

  if (num_target_entries != num_funcs + num_vars)
    {
      gomp_mutex_unlock (&devicep->lock);
      if (is_register_lock)
	gomp_mutex_unlock (&register_lock);
      gomp_fatal ("Cannot map target functions or variables"
		  " (expected %u, have %u)", num_funcs + num_vars,
		  num_target_entries);
    }

  /* Insert host-target address mapping into splay tree.  */
  struct target_mem_desc *tgt = gomp_malloc (sizeof (*tgt));
  tgt->array = gomp_malloc ((num_funcs + num_vars) * sizeof (*tgt->array));
  tgt->refcount = REFCOUNT_INFINITY;
  tgt->tgt_start = 0;
  tgt->tgt_end = 0;
  tgt->to_free = NULL;
  tgt->prev = NULL;
  tgt->list_count = 0;
  tgt->device_descr = devicep;
  splay_tree_node array = tgt->array;

  for (i = 0; i < num_funcs; i++)
    {
      splay_tree_key k = &array->key;
      k->host_start = (uintptr_t) host_func_table[i];
      k->host_end = k->host_start + 1;
      k->tgt = tgt;
      k->tgt_offset = target_table[i].start;
      k->refcount = REFCOUNT_INFINITY;
      k->link_key = NULL;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      array++;
    }

  /* Most significant bit of the size in host and target tables marks
     "omp declare target link" variables.  */
  const uintptr_t link_bit = 1ULL << (sizeof (uintptr_t) * __CHAR_BIT__ - 1);
  const uintptr_t size_mask = ~link_bit;

  for (i = 0; i < num_vars; i++)
    {
      struct addr_pair *target_var = &target_table[num_funcs + i];
      uintptr_t target_size = target_var->end - target_var->start;

      if ((uintptr_t) host_var_table[i * 2 + 1] != target_size)
	{
	  gomp_mutex_unlock (&devicep->lock);
	  if (is_register_lock)
	    gomp_mutex_unlock (&register_lock);
	  gomp_fatal ("Cannot map target variables (size mismatch)");
	}

      splay_tree_key k = &array->key;
      k->host_start = (uintptr_t) host_var_table[i * 2];
      k->host_end
	= k->host_start + (size_mask & (uintptr_t) host_var_table[i * 2 + 1]);
      k->tgt = tgt;
      k->tgt_offset = target_var->start;
      k->refcount = target_size & link_bit ? REFCOUNT_LINK : REFCOUNT_INFINITY;
      k->link_key = NULL;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      array++;
    }

  free (target_table);
}

/* Unload the mappings described by target_data from device DEVICE_P.
   The device must be locked.   */

static void
gomp_unload_image_from_device (struct gomp_device_descr *devicep,
			       unsigned version,
			       const void *host_table, const void *target_data)
{
  void **host_func_table = ((void ***) host_table)[0];
  void **host_funcs_end  = ((void ***) host_table)[1];
  void **host_var_table  = ((void ***) host_table)[2];
  void **host_vars_end   = ((void ***) host_table)[3];

  /* The func table contains only addresses, the var table contains addresses
     and corresponding sizes.  */
  int num_funcs = host_funcs_end - host_func_table;
  int num_vars  = (host_vars_end - host_var_table) / 2;

  struct splay_tree_key_s k;
  splay_tree_key node = NULL;

  /* Find mapping at start of node array */
  if (num_funcs || num_vars)
    {
      k.host_start = (num_funcs ? (uintptr_t) host_func_table[0]
		      : (uintptr_t) host_var_table[0]);
      k.host_end = k.host_start + 1;
      node = splay_tree_lookup (&devicep->mem_map, &k);
    }

  if (!devicep->unload_image_func (devicep->target_id, version, target_data))
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("image unload fail");
    }

  /* Remove mappings from splay tree.  */
  int i;
  for (i = 0; i < num_funcs; i++)
    {
      k.host_start = (uintptr_t) host_func_table[i];
      k.host_end = k.host_start + 1;
      splay_tree_remove (&devicep->mem_map, &k);
    }

  /* Most significant bit of the size in host and target tables marks
     "omp declare target link" variables.  */
  const uintptr_t link_bit = 1ULL << (sizeof (uintptr_t) * __CHAR_BIT__ - 1);
  const uintptr_t size_mask = ~link_bit;
  bool is_tgt_unmapped = false;

  for (i = 0; i < num_vars; i++)
    {
      k.host_start = (uintptr_t) host_var_table[i * 2];
      k.host_end
	= k.host_start + (size_mask & (uintptr_t) host_var_table[i * 2 + 1]);

      if (!(link_bit & (uintptr_t) host_var_table[i * 2 + 1]))
	splay_tree_remove (&devicep->mem_map, &k);
      else
	{
	  splay_tree_key n = splay_tree_lookup (&devicep->mem_map, &k);
	  splay_tree_remove (&devicep->mem_map, n);
	  if (n->link_key)
	    {
	      if (n->tgt->refcount > 1)
		n->tgt->refcount--;
	      else
		{
		  is_tgt_unmapped = true;
		  gomp_unmap_tgt (n->tgt);
		}
	    }
	}
    }

  if (node && !is_tgt_unmapped)
    {
      free (node->tgt);
      free (node);
    }
}

/* This function should be called from every offload image while loading.
   It gets the descriptor of the host func and var tables HOST_TABLE, TYPE of
   the target, and TARGET_DATA needed by target plugin.  */

void
GOMP_offload_register_ver (unsigned version, const void *host_table,
			   int target_type, const void *target_data)
{
  int i;

  if (GOMP_VERSION_LIB (version) > GOMP_VERSION)
    gomp_fatal ("Library too old for offload (version %u < %u)",
		GOMP_VERSION, GOMP_VERSION_LIB (version));
  
  gomp_mutex_lock (&register_lock);

  /* Load image to all initialized devices.  */
  for (i = 0; i < num_devices; i++)
    {
      struct gomp_device_descr *devicep = &devices[i];
      gomp_mutex_lock (&devicep->lock);
      if (devicep->type == target_type
	  && devicep->state == GOMP_DEVICE_INITIALIZED)
	gomp_load_image_to_device (devicep, version,
				   host_table, target_data, true);
      gomp_mutex_unlock (&devicep->lock);
    }

  /* Insert image to array of pending images.  */
  offload_images
    = gomp_realloc_unlock (offload_images,
			   (num_offload_images + 1)
			   * sizeof (struct offload_image_descr));
  offload_images[num_offload_images].version = version;
  offload_images[num_offload_images].type = target_type;
  offload_images[num_offload_images].host_table = host_table;
  offload_images[num_offload_images].target_data = target_data;

  num_offload_images++;
  gomp_mutex_unlock (&register_lock);
}

void
GOMP_offload_register (const void *host_table, int target_type,
		       const void *target_data)
{
  GOMP_offload_register_ver (0, host_table, target_type, target_data);
}

/* This function should be called from every offload image while unloading.
   It gets the descriptor of the host func and var tables HOST_TABLE, TYPE of
   the target, and TARGET_DATA needed by target plugin.  */

void
GOMP_offload_unregister_ver (unsigned version, const void *host_table,
			     int target_type, const void *target_data)
{
  int i;

  gomp_mutex_lock (&register_lock);

  /* Unload image from all initialized devices.  */
  for (i = 0; i < num_devices; i++)
    {
      struct gomp_device_descr *devicep = &devices[i];
      gomp_mutex_lock (&devicep->lock);
      if (devicep->type == target_type
	  && devicep->state == GOMP_DEVICE_INITIALIZED)
	gomp_unload_image_from_device (devicep, version,
				       host_table, target_data);
      gomp_mutex_unlock (&devicep->lock);
    }

  /* Remove image from array of pending images.  */
  for (i = 0; i < num_offload_images; i++)
    if (offload_images[i].target_data == target_data)
      {
	offload_images[i] = offload_images[--num_offload_images];
	break;
      }

  gomp_mutex_unlock (&register_lock);
}

void
GOMP_offload_unregister (const void *host_table, int target_type,
			 const void *target_data)
{
  GOMP_offload_unregister_ver (0, host_table, target_type, target_data);
}

/* This function initializes the target device, specified by DEVICEP.  DEVICEP
   must be locked on entry, and remains locked on return.  */

attribute_hidden void
gomp_init_device (struct gomp_device_descr *devicep)
{
  int i;
  if (!devicep->init_device_func (devicep->target_id))
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("device initialization failed");
    }

  /* Load to device all images registered by the moment.  */
  for (i = 0; i < num_offload_images; i++)
    {
      struct offload_image_descr *image = &offload_images[i];
      if (image->type == devicep->type)
	gomp_load_image_to_device (devicep, image->version,
				   image->host_table, image->target_data,
				   false);
    }

  devicep->state = GOMP_DEVICE_INITIALIZED;
}

attribute_hidden void
gomp_unload_device (struct gomp_device_descr *devicep)
{
  if (devicep->state == GOMP_DEVICE_INITIALIZED)
    {
      unsigned i;
      
      /* Unload from device all images registered at the moment.  */
      for (i = 0; i < num_offload_images; i++)
	{
	  struct offload_image_descr *image = &offload_images[i];
	  if (image->type == devicep->type)
	    gomp_unload_image_from_device (devicep, image->version,
					   image->host_table,
					   image->target_data);
	}
    }
}

/* Free address mapping tables.  MM must be locked on entry, and remains locked
   on return.  */

attribute_hidden void
gomp_free_memmap (struct splay_tree_s *mem_map)
{
  while (mem_map->root)
    {
      struct target_mem_desc *tgt = mem_map->root->key.tgt;

      splay_tree_remove (mem_map, &mem_map->root->key);
      free (tgt->array);
      free (tgt);
    }
}

/* Host fallback for GOMP_target{,_ext} routines.  */

static void
gomp_target_fallback (void (*fn) (void *), void **hostaddrs)
{
  struct gomp_thread old_thr, *thr = gomp_thread ();
  old_thr = *thr;
  memset (thr, '\0', sizeof (*thr));
  if (gomp_places_list)
    {
      thr->place = old_thr.place;
      thr->ts.place_partition_len = gomp_places_list_len;
    }
  fn (hostaddrs);
  gomp_free_thread (thr);
  *thr = old_thr;
}

/* Calculate alignment and size requirements of a private copy of data shared
   as GOMP_MAP_FIRSTPRIVATE and store them to TGT_ALIGN and TGT_SIZE.  */

static inline void
calculate_firstprivate_requirements (size_t mapnum, size_t *sizes,
				     unsigned short *kinds, size_t *tgt_align,
				     size_t *tgt_size)
{
  size_t i;
  for (i = 0; i < mapnum; i++)
    if ((kinds[i] & 0xff) == GOMP_MAP_FIRSTPRIVATE)
      {
	size_t align = (size_t) 1 << (kinds[i] >> 8);
	if (*tgt_align < align)
	  *tgt_align = align;
	*tgt_size = (*tgt_size + align - 1) & ~(align - 1);
	*tgt_size += sizes[i];
      }
}

/* Copy data shared as GOMP_MAP_FIRSTPRIVATE to DST.  */

static inline void
copy_firstprivate_data (char *tgt, size_t mapnum, void **hostaddrs,
			size_t *sizes, unsigned short *kinds, size_t tgt_align,
			size_t tgt_size)
{
  uintptr_t al = (uintptr_t) tgt & (tgt_align - 1);
  if (al)
    tgt += tgt_align - al;
  tgt_size = 0;
  size_t i;
  for (i = 0; i < mapnum; i++)
    if ((kinds[i] & 0xff) == GOMP_MAP_FIRSTPRIVATE)
      {
	size_t align = (size_t) 1 << (kinds[i] >> 8);
	tgt_size = (tgt_size + align - 1) & ~(align - 1);
	memcpy (tgt + tgt_size, hostaddrs[i], sizes[i]);
	hostaddrs[i] = tgt + tgt_size;
	tgt_size = tgt_size + sizes[i];
      }
}

/* Helper function of GOMP_target{,_ext} routines.  */

static void *
gomp_get_target_fn_addr (struct gomp_device_descr *devicep,
			 void (*host_fn) (void *))
{
  if (devicep->capabilities & GOMP_OFFLOAD_CAP_NATIVE_EXEC)
    return (void *) host_fn;
  else
    {
      gomp_mutex_lock (&devicep->lock);
      if (devicep->state == GOMP_DEVICE_FINALIZED)
	{
	  gomp_mutex_unlock (&devicep->lock);
	  return NULL;
	}

      struct splay_tree_key_s k;
      k.host_start = (uintptr_t) host_fn;
      k.host_end = k.host_start + 1;
      splay_tree_key tgt_fn = splay_tree_lookup (&devicep->mem_map, &k);
      gomp_mutex_unlock (&devicep->lock);
      if (tgt_fn == NULL)
	return NULL;

      return (void *) tgt_fn->tgt_offset;
    }
}

/* Called when encountering a target directive.  If DEVICE
   is GOMP_DEVICE_ICV, it means use device-var ICV.  If it is
   GOMP_DEVICE_HOST_FALLBACK (or any value
   larger than last available hw device), use host fallback.
   FN is address of host code, UNUSED is part of the current ABI, but
   we're not actually using it.  HOSTADDRS, SIZES and KINDS are arrays
   with MAPNUM entries, with addresses of the host objects,
   sizes of the host objects (resp. for pointer kind pointer bias
   and assumed sizeof (void *) size) and kinds.  */

void
GOMP_target (int device, void (*fn) (void *), const void *unused,
	     size_t mapnum, void **hostaddrs, size_t *sizes,
	     unsigned char *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  void *fn_addr;
  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      /* All shared memory devices should use the GOMP_target_ext function.  */
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM
      || !(fn_addr = gomp_get_target_fn_addr (devicep, fn)))
    return gomp_target_fallback (fn, hostaddrs);

  struct target_mem_desc *tgt_vars
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, false,
		     GOMP_MAP_VARS_TARGET);
  devicep->run_func (devicep->target_id, fn_addr, (void *) tgt_vars->tgt_start,
		     NULL);
  gomp_unmap_vars (tgt_vars, true);
}

/* Like GOMP_target, but KINDS is 16-bit, UNUSED is no longer present,
   and several arguments have been added:
   FLAGS is a bitmask, see GOMP_TARGET_FLAG_* in gomp-constants.h.
   DEPEND is array of dependencies, see GOMP_task for details.

   ARGS is a pointer to an array consisting of a variable number of both
   device-independent and device-specific arguments, which can take one two
   elements where the first specifies for which device it is intended, the type
   and optionally also the value.  If the value is not present in the first
   one, the whole second element the actual value.  The last element of the
   array is a single NULL.  Among the device independent can be for example
   NUM_TEAMS and THREAD_LIMIT.

   NUM_TEAMS is positive if GOMP_teams will be called in the body with
   that value, or 1 if teams construct is not present, or 0, if
   teams construct does not have num_teams clause and so the choice is
   implementation defined, and -1 if it can't be determined on the host
   what value will GOMP_teams have on the device.
   THREAD_LIMIT similarly is positive if GOMP_teams will be called in the
   body with that value, or 0, if teams construct does not have thread_limit
   clause or the teams construct is not present, or -1 if it can't be
   determined on the host what value will GOMP_teams have on the device.  */

void
GOMP_target_ext (int device, void (*fn) (void *), size_t mapnum,
		 void **hostaddrs, size_t *sizes, unsigned short *kinds,
		 unsigned int flags, void **depend, void **args)
{
  struct gomp_device_descr *devicep = resolve_device (device);
  size_t tgt_align = 0, tgt_size = 0;
  bool fpc_done = false;

  if (flags & GOMP_TARGET_FLAG_NOWAIT)
    {
      struct gomp_thread *thr = gomp_thread ();
      /* Create a team if we don't have any around, as nowait
	 target tasks make sense to run asynchronously even when
	 outside of any parallel.  */
      if (__builtin_expect (thr->ts.team == NULL, 0))
	{
	  struct gomp_team *team = gomp_new_team (1);
	  struct gomp_task *task = thr->task;
	  struct gomp_task_icv *icv = task ? &task->icv : &gomp_global_icv;
	  team->prev_ts = thr->ts;
	  thr->ts.team = team;
	  thr->ts.team_id = 0;
	  thr->ts.work_share = &team->work_shares[0];
	  thr->ts.last_work_share = NULL;
#ifdef HAVE_SYNC_BUILTINS
	  thr->ts.single_count = 0;
#endif
	  thr->ts.static_trip = 0;
	  thr->task = &team->implicit_task[0];
	  gomp_init_task (thr->task, NULL, icv);
	  if (task)
	    {
	      thr->task = task;
	      gomp_end_task ();
	      free (task);
	      thr->task = &team->implicit_task[0];
	    }
	  else
	    pthread_setspecific (gomp_thread_destructor, thr);
	}
      if (thr->ts.team
	  && !thr->task->final_task)
	{
	  gomp_create_target_task (devicep, fn, mapnum, hostaddrs,
				   sizes, kinds, flags, depend, args,
				   GOMP_TARGET_TASK_BEFORE_MAP);
	  return;
	}
    }

  /* If there are depend clauses, but nowait is not present
     (or we are in a final task), block the parent task until the
     dependencies are resolved and then just continue with the rest
     of the function as if it is a merged task.  */
  if (depend != NULL)
    {
      struct gomp_thread *thr = gomp_thread ();
      if (thr->task && thr->task->depend_hash)
	{
	  /* If we might need to wait, copy firstprivate now.  */
	  calculate_firstprivate_requirements (mapnum, sizes, kinds,
					       &tgt_align, &tgt_size);
	  if (tgt_align)
	    {
	      char *tgt = gomp_alloca (tgt_size + tgt_align - 1);
	      copy_firstprivate_data (tgt, mapnum, hostaddrs, sizes, kinds,
				      tgt_align, tgt_size);
	    }
	  fpc_done = true;
	  gomp_task_maybe_wait_for_dependencies (depend);
	}
    }

  void *fn_addr;
  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || !(fn_addr = gomp_get_target_fn_addr (devicep, fn))
      || (devicep->can_run_func && !devicep->can_run_func (fn_addr)))
    {
      if (!fpc_done)
	{
	  calculate_firstprivate_requirements (mapnum, sizes, kinds,
					       &tgt_align, &tgt_size);
	  if (tgt_align)
	    {
	      char *tgt = gomp_alloca (tgt_size + tgt_align - 1);
	      copy_firstprivate_data (tgt, mapnum, hostaddrs, sizes, kinds,
				      tgt_align, tgt_size);
	    }
	}
      gomp_target_fallback (fn, hostaddrs);
      return;
    }

  struct target_mem_desc *tgt_vars;
  if (devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    {
      if (!fpc_done)
	{
	  calculate_firstprivate_requirements (mapnum, sizes, kinds,
					       &tgt_align, &tgt_size);
	  if (tgt_align)
	    {
	      char *tgt = gomp_alloca (tgt_size + tgt_align - 1);
	      copy_firstprivate_data (tgt, mapnum, hostaddrs, sizes, kinds,
				      tgt_align, tgt_size);
	    }
	}
      tgt_vars = NULL;
    }
  else
    tgt_vars = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds,
			      true, GOMP_MAP_VARS_TARGET);
  devicep->run_func (devicep->target_id, fn_addr,
		     tgt_vars ? (void *) tgt_vars->tgt_start : hostaddrs,
		     args);
  if (tgt_vars)
    gomp_unmap_vars (tgt_vars, true);
}

/* Host fallback for GOMP_target_data{,_ext} routines.  */

static void
gomp_target_data_fallback (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  if (icv->target_data)
    {
      /* Even when doing a host fallback, if there are any active
         #pragma omp target data constructs, need to remember the
         new #pragma omp target data, otherwise GOMP_target_end_data
         would get out of sync.  */
      struct target_mem_desc *tgt
	= gomp_map_vars (NULL, 0, NULL, NULL, NULL, NULL, false,
			 GOMP_MAP_VARS_DATA);
      tgt->prev = icv->target_data;
      icv->target_data = tgt;
    }
}

void
GOMP_target_data (int device, const void *unused, size_t mapnum,
		  void **hostaddrs, size_t *sizes, unsigned char *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || (devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM))
    return gomp_target_data_fallback ();

  struct target_mem_desc *tgt
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, false,
		     GOMP_MAP_VARS_DATA);
  struct gomp_task_icv *icv = gomp_icv (true);
  tgt->prev = icv->target_data;
  icv->target_data = tgt;
}

void
GOMP_target_data_ext (int device, size_t mapnum, void **hostaddrs,
		      size_t *sizes, unsigned short *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return gomp_target_data_fallback ();

  struct target_mem_desc *tgt
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, true,
		     GOMP_MAP_VARS_DATA);
  struct gomp_task_icv *icv = gomp_icv (true);
  tgt->prev = icv->target_data;
  icv->target_data = tgt;
}

void
GOMP_target_end_data (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  if (icv->target_data)
    {
      struct target_mem_desc *tgt = icv->target_data;
      icv->target_data = tgt->prev;
      gomp_unmap_vars (tgt, true);
    }
}

void
GOMP_target_update (int device, const void *unused, size_t mapnum,
		    void **hostaddrs, size_t *sizes, unsigned char *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  gomp_update (devicep, mapnum, hostaddrs, sizes, kinds, false);
}

void
GOMP_target_update_ext (int device, size_t mapnum, void **hostaddrs,
			size_t *sizes, unsigned short *kinds,
			unsigned int flags, void **depend)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  /* If there are depend clauses, but nowait is not present,
     block the parent task until the dependencies are resolved
     and then just continue with the rest of the function as if it
     is a merged task.  Until we are able to schedule task during
     variable mapping or unmapping, ignore nowait if depend clauses
     are not present.  */
  if (depend != NULL)
    {
      struct gomp_thread *thr = gomp_thread ();
      if (thr->task && thr->task->depend_hash)
	{
	  if ((flags & GOMP_TARGET_FLAG_NOWAIT)
	      && thr->ts.team
	      && !thr->task->final_task)
	    {
	      if (gomp_create_target_task (devicep, (void (*) (void *)) NULL,
					   mapnum, hostaddrs, sizes, kinds,
					   flags | GOMP_TARGET_FLAG_UPDATE,
					   depend, NULL, GOMP_TARGET_TASK_DATA))
		return;
	    }
	  else
	    {
	      struct gomp_team *team = thr->ts.team;
	      /* If parallel or taskgroup has been cancelled, don't start new
		 tasks.  */
	      if (team
		  && (gomp_team_barrier_cancelled (&team->barrier)
		      || (thr->task->taskgroup
			  && thr->task->taskgroup->cancelled)))
		return;

	      gomp_task_maybe_wait_for_dependencies (depend);
	    }
	}
    }

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return;

  gomp_update (devicep, mapnum, hostaddrs, sizes, kinds, true);
}

static void
gomp_exit_data (struct gomp_device_descr *devicep, size_t mapnum,
		void **hostaddrs, size_t *sizes, unsigned short *kinds)
{
  const int typemask = 0xff;
  size_t i;
  gomp_mutex_lock (&devicep->lock);
  if (devicep->state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devicep->lock);
      return;
    }

  for (i = 0; i < mapnum; i++)
    {
      struct splay_tree_key_s cur_node;
      unsigned char kind = kinds[i] & typemask;
      switch (kind)
	{
	case GOMP_MAP_FROM:
	case GOMP_MAP_ALWAYS_FROM:
	case GOMP_MAP_DELETE:
	case GOMP_MAP_RELEASE:
	case GOMP_MAP_ZERO_LEN_ARRAY_SECTION:
	case GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION:
	  cur_node.host_start = (uintptr_t) hostaddrs[i];
	  cur_node.host_end = cur_node.host_start + sizes[i];
	  splay_tree_key k = (kind == GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION
			      || kind == GOMP_MAP_ZERO_LEN_ARRAY_SECTION)
	    ? gomp_map_0len_lookup (&devicep->mem_map, &cur_node)
	    : splay_tree_lookup (&devicep->mem_map, &cur_node);
	  if (!k)
	    continue;

	  if (k->refcount > 0 && k->refcount != REFCOUNT_INFINITY)
	    k->refcount--;
	  if ((kind == GOMP_MAP_DELETE
	       || kind == GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION)
	      && k->refcount != REFCOUNT_INFINITY)
	    k->refcount = 0;

	  if ((kind == GOMP_MAP_FROM && k->refcount == 0)
	      || kind == GOMP_MAP_ALWAYS_FROM)
	    gomp_copy_dev2host (devicep, (void *) cur_node.host_start,
				(void *) (k->tgt->tgt_start + k->tgt_offset
					  + cur_node.host_start
					  - k->host_start),
				cur_node.host_end - cur_node.host_start);
	  if (k->refcount == 0)
	    {
	      splay_tree_remove (&devicep->mem_map, k);
	      if (k->link_key)
		splay_tree_insert (&devicep->mem_map,
				   (splay_tree_node) k->link_key);
	      if (k->tgt->refcount > 1)
		k->tgt->refcount--;
	      else
		gomp_unmap_tgt (k->tgt);
	    }

	  break;
	default:
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("GOMP_target_enter_exit_data unhandled kind 0x%.2x",
		      kind);
	}
    }

  gomp_mutex_unlock (&devicep->lock);
}

void
GOMP_target_enter_exit_data (int device, size_t mapnum, void **hostaddrs,
			     size_t *sizes, unsigned short *kinds,
			     unsigned int flags, void **depend)
{
  struct gomp_device_descr *devicep = resolve_device (device);

  /* If there are depend clauses, but nowait is not present,
     block the parent task until the dependencies are resolved
     and then just continue with the rest of the function as if it
     is a merged task.  Until we are able to schedule task during
     variable mapping or unmapping, ignore nowait if depend clauses
     are not present.  */
  if (depend != NULL)
    {
      struct gomp_thread *thr = gomp_thread ();
      if (thr->task && thr->task->depend_hash)
	{
	  if ((flags & GOMP_TARGET_FLAG_NOWAIT)
	      && thr->ts.team
	      && !thr->task->final_task)
	    {
	      if (gomp_create_target_task (devicep, (void (*) (void *)) NULL,
					   mapnum, hostaddrs, sizes, kinds,
					   flags, depend, NULL,
					   GOMP_TARGET_TASK_DATA))
		return;
	    }
	  else
	    {
	      struct gomp_team *team = thr->ts.team;
	      /* If parallel or taskgroup has been cancelled, don't start new
		 tasks.  */
	      if (team
		  && (gomp_team_barrier_cancelled (&team->barrier)
		      || (thr->task->taskgroup
			  && thr->task->taskgroup->cancelled)))
		return;

	      gomp_task_maybe_wait_for_dependencies (depend);
	    }
	}
    }

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return;

  struct gomp_thread *thr = gomp_thread ();
  struct gomp_team *team = thr->ts.team;
  /* If parallel or taskgroup has been cancelled, don't start new tasks.  */
  if (team
      && (gomp_team_barrier_cancelled (&team->barrier)
	  || (thr->task->taskgroup && thr->task->taskgroup->cancelled)))
    return;

  size_t i;
  if ((flags & GOMP_TARGET_FLAG_EXIT_DATA) == 0)
    for (i = 0; i < mapnum; i++)
      if ((kinds[i] & 0xff) == GOMP_MAP_STRUCT)
	{
	  gomp_map_vars (devicep, sizes[i] + 1, &hostaddrs[i], NULL, &sizes[i],
			 &kinds[i], true, GOMP_MAP_VARS_ENTER_DATA);
	  i += sizes[i];
	}
      else
	gomp_map_vars (devicep, 1, &hostaddrs[i], NULL, &sizes[i], &kinds[i],
		       true, GOMP_MAP_VARS_ENTER_DATA);
  else
    gomp_exit_data (devicep, mapnum, hostaddrs, sizes, kinds);
}

bool
gomp_target_task_fn (void *data)
{
  struct gomp_target_task *ttask = (struct gomp_target_task *) data;
  struct gomp_device_descr *devicep = ttask->devicep;

  if (ttask->fn != NULL)
    {
      void *fn_addr;
      if (devicep == NULL
	  || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || !(fn_addr = gomp_get_target_fn_addr (devicep, ttask->fn))
	  || (devicep->can_run_func && !devicep->can_run_func (fn_addr)))
	{
	  ttask->state = GOMP_TARGET_TASK_FALLBACK;
	  gomp_target_fallback (ttask->fn, ttask->hostaddrs);
	  return false;
	}

      if (ttask->state == GOMP_TARGET_TASK_FINISHED)
	{
	  if (ttask->tgt)
	    gomp_unmap_vars (ttask->tgt, true);
	  return false;
	}

      void *actual_arguments;
      if (devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	{
	  ttask->tgt = NULL;
	  actual_arguments = ttask->hostaddrs;
	}
      else
	{
	  ttask->tgt = gomp_map_vars (devicep, ttask->mapnum, ttask->hostaddrs,
				      NULL, ttask->sizes, ttask->kinds, true,
				      GOMP_MAP_VARS_TARGET);
	  actual_arguments = (void *) ttask->tgt->tgt_start;
	}
      ttask->state = GOMP_TARGET_TASK_READY_TO_RUN;

      devicep->async_run_func (devicep->target_id, fn_addr, actual_arguments,
			       ttask->args, (void *) ttask);
      return true;
    }
  else if (devicep == NULL
	   || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	   || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return false;

  size_t i;
  if (ttask->flags & GOMP_TARGET_FLAG_UPDATE)
    gomp_update (devicep, ttask->mapnum, ttask->hostaddrs, ttask->sizes,
		 ttask->kinds, true);
  else if ((ttask->flags & GOMP_TARGET_FLAG_EXIT_DATA) == 0)
    for (i = 0; i < ttask->mapnum; i++)
      if ((ttask->kinds[i] & 0xff) == GOMP_MAP_STRUCT)
	{
	  gomp_map_vars (devicep, ttask->sizes[i] + 1, &ttask->hostaddrs[i],
			 NULL, &ttask->sizes[i], &ttask->kinds[i], true,
			 GOMP_MAP_VARS_ENTER_DATA);
	  i += ttask->sizes[i];
	}
      else
	gomp_map_vars (devicep, 1, &ttask->hostaddrs[i], NULL, &ttask->sizes[i],
		       &ttask->kinds[i], true, GOMP_MAP_VARS_ENTER_DATA);
  else
    gomp_exit_data (devicep, ttask->mapnum, ttask->hostaddrs, ttask->sizes,
		    ttask->kinds);
  return false;
}

void
GOMP_teams (unsigned int num_teams, unsigned int thread_limit)
{
  if (thread_limit)
    {
      struct gomp_task_icv *icv = gomp_icv (true);
      icv->thread_limit_var
	= thread_limit > INT_MAX ? UINT_MAX : thread_limit;
    }
  (void) num_teams;
}

void *
omp_target_alloc (size_t size, int device_num)
{
  if (device_num == GOMP_DEVICE_HOST_FALLBACK)
    return malloc (size);

  if (device_num < 0)
    return NULL;

  struct gomp_device_descr *devicep = resolve_device (device_num);
  if (devicep == NULL)
    return NULL;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return malloc (size);

  gomp_mutex_lock (&devicep->lock);
  void *ret = devicep->alloc_func (devicep->target_id, size);
  gomp_mutex_unlock (&devicep->lock);
  return ret;
}

void
omp_target_free (void *device_ptr, int device_num)
{
  if (device_ptr == NULL)
    return;

  if (device_num == GOMP_DEVICE_HOST_FALLBACK)
    {
      free (device_ptr);
      return;
    }

  if (device_num < 0)
    return;

  struct gomp_device_descr *devicep = resolve_device (device_num);
  if (devicep == NULL)
    return;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    {
      free (device_ptr);
      return;
    }

  gomp_mutex_lock (&devicep->lock);
  gomp_free_device_memory (devicep, device_ptr);
  gomp_mutex_unlock (&devicep->lock);
}

int
omp_target_is_present (void *ptr, int device_num)
{
  if (ptr == NULL)
    return 1;

  if (device_num == GOMP_DEVICE_HOST_FALLBACK)
    return 1;

  if (device_num < 0)
    return 0;

  struct gomp_device_descr *devicep = resolve_device (device_num);
  if (devicep == NULL)
    return 0;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return 1;

  gomp_mutex_lock (&devicep->lock);
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;

  cur_node.host_start = (uintptr_t) ptr;
  cur_node.host_end = cur_node.host_start;
  splay_tree_key n = gomp_map_0len_lookup (mem_map, &cur_node);
  int ret = n != NULL;
  gomp_mutex_unlock (&devicep->lock);
  return ret;
}

int
omp_target_memcpy (void *dst, void *src, size_t length, size_t dst_offset,
		   size_t src_offset, int dst_device_num, int src_device_num)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;
  bool ret;

  if (dst_device_num != GOMP_DEVICE_HOST_FALLBACK)
    {
      if (dst_device_num < 0)
	return EINVAL;

      dst_devicep = resolve_device (dst_device_num);
      if (dst_devicep == NULL)
	return EINVAL;

      if (!(dst_devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || dst_devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	dst_devicep = NULL;
    }
  if (src_device_num != GOMP_DEVICE_HOST_FALLBACK)
    {
      if (src_device_num < 0)
	return EINVAL;

      src_devicep = resolve_device (src_device_num);
      if (src_devicep == NULL)
	return EINVAL;

      if (!(src_devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || src_devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	src_devicep = NULL;
    }
  if (src_devicep == NULL && dst_devicep == NULL)
    {
      memcpy ((char *) dst + dst_offset, (char *) src + src_offset, length);
      return 0;
    }
  if (src_devicep == NULL)
    {
      gomp_mutex_lock (&dst_devicep->lock);
      ret = dst_devicep->host2dev_func (dst_devicep->target_id,
					(char *) dst + dst_offset,
					(char *) src + src_offset, length);
      gomp_mutex_unlock (&dst_devicep->lock);
      return (ret ? 0 : EINVAL);
    }
  if (dst_devicep == NULL)
    {
      gomp_mutex_lock (&src_devicep->lock);
      ret = src_devicep->dev2host_func (src_devicep->target_id,
					(char *) dst + dst_offset,
					(char *) src + src_offset, length);
      gomp_mutex_unlock (&src_devicep->lock);
      return (ret ? 0 : EINVAL);
    }
  if (src_devicep == dst_devicep)
    {
      gomp_mutex_lock (&src_devicep->lock);
      ret = src_devicep->dev2dev_func (src_devicep->target_id,
				       (char *) dst + dst_offset,
				       (char *) src + src_offset, length);
      gomp_mutex_unlock (&src_devicep->lock);
      return (ret ? 0 : EINVAL);
    }
  return EINVAL;
}

static int
omp_target_memcpy_rect_worker (void *dst, void *src, size_t element_size,
			       int num_dims, const size_t *volume,
			       const size_t *dst_offsets,
			       const size_t *src_offsets,
			       const size_t *dst_dimensions,
			       const size_t *src_dimensions,
			       struct gomp_device_descr *dst_devicep,
			       struct gomp_device_descr *src_devicep)
{
  size_t dst_slice = element_size;
  size_t src_slice = element_size;
  size_t j, dst_off, src_off, length;
  int i, ret;

  if (num_dims == 1)
    {
      if (__builtin_mul_overflow (element_size, volume[0], &length)
	  || __builtin_mul_overflow (element_size, dst_offsets[0], &dst_off)
	  || __builtin_mul_overflow (element_size, src_offsets[0], &src_off))
	return EINVAL;
      if (dst_devicep == NULL && src_devicep == NULL)
	{
	  memcpy ((char *) dst + dst_off, (char *) src + src_off, length);
	  ret = 1;
	}
      else if (src_devicep == NULL)
	ret = dst_devicep->host2dev_func (dst_devicep->target_id,
					  (char *) dst + dst_off,
					  (char *) src + src_off, length);
      else if (dst_devicep == NULL)
	ret = src_devicep->dev2host_func (src_devicep->target_id,
					  (char *) dst + dst_off,
					  (char *) src + src_off, length);
      else if (src_devicep == dst_devicep)
	ret = src_devicep->dev2dev_func (src_devicep->target_id,
					 (char *) dst + dst_off,
					 (char *) src + src_off, length);
      else
	ret = 0;
      return ret ? 0 : EINVAL;
    }

  /* FIXME: it would be nice to have some plugin function to handle
     num_dims == 2 and num_dims == 3 more efficiently.  Larger ones can
     be handled in the generic recursion below, and for host-host it
     should be used even for any num_dims >= 2.  */

  for (i = 1; i < num_dims; i++)
    if (__builtin_mul_overflow (dst_slice, dst_dimensions[i], &dst_slice)
	|| __builtin_mul_overflow (src_slice, src_dimensions[i], &src_slice))
      return EINVAL;
  if (__builtin_mul_overflow (dst_slice, dst_offsets[0], &dst_off)
      || __builtin_mul_overflow (src_slice, src_offsets[0], &src_off))
    return EINVAL;
  for (j = 0; j < volume[0]; j++)
    {
      ret = omp_target_memcpy_rect_worker ((char *) dst + dst_off,
					   (char *) src + src_off,
					   element_size, num_dims - 1,
					   volume + 1, dst_offsets + 1,
					   src_offsets + 1, dst_dimensions + 1,
					   src_dimensions + 1, dst_devicep,
					   src_devicep);
      if (ret)
	return ret;
      dst_off += dst_slice;
      src_off += src_slice;
    }
  return 0;
}

int
omp_target_memcpy_rect (void *dst, void *src, size_t element_size,
			int num_dims, const size_t *volume,
			const size_t *dst_offsets,
			const size_t *src_offsets,
			const size_t *dst_dimensions,
			const size_t *src_dimensions,
			int dst_device_num, int src_device_num)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;

  if (!dst && !src)
    return INT_MAX;

  if (dst_device_num != GOMP_DEVICE_HOST_FALLBACK)
    {
      if (dst_device_num < 0)
	return EINVAL;

      dst_devicep = resolve_device (dst_device_num);
      if (dst_devicep == NULL)
	return EINVAL;

      if (!(dst_devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || dst_devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	dst_devicep = NULL;
    }
  if (src_device_num != GOMP_DEVICE_HOST_FALLBACK)
    {
      if (src_device_num < 0)
	return EINVAL;

      src_devicep = resolve_device (src_device_num);
      if (src_devicep == NULL)
	return EINVAL;

      if (!(src_devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || src_devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	src_devicep = NULL;
    }

  if (src_devicep != NULL && dst_devicep != NULL && src_devicep != dst_devicep)
    return EINVAL;

  if (src_devicep)
    gomp_mutex_lock (&src_devicep->lock);
  else if (dst_devicep)
    gomp_mutex_lock (&dst_devicep->lock);
  int ret = omp_target_memcpy_rect_worker (dst, src, element_size, num_dims,
					   volume, dst_offsets, src_offsets,
					   dst_dimensions, src_dimensions,
					   dst_devicep, src_devicep);
  if (src_devicep)
    gomp_mutex_unlock (&src_devicep->lock);
  else if (dst_devicep)
    gomp_mutex_unlock (&dst_devicep->lock);
  return ret;
}

int
omp_target_associate_ptr (void *host_ptr, void *device_ptr, size_t size,
			  size_t device_offset, int device_num)
{
  if (device_num == GOMP_DEVICE_HOST_FALLBACK)
    return EINVAL;

  if (device_num < 0)
    return EINVAL;

  struct gomp_device_descr *devicep = resolve_device (device_num);
  if (devicep == NULL)
    return EINVAL;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return EINVAL;

  gomp_mutex_lock (&devicep->lock);

  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  int ret = EINVAL;

  cur_node.host_start = (uintptr_t) host_ptr;
  cur_node.host_end = cur_node.host_start + size;
  splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
  if (n)
    {
      if (n->tgt->tgt_start + n->tgt_offset
	  == (uintptr_t) device_ptr + device_offset
	  && n->host_start <= cur_node.host_start
	  && n->host_end >= cur_node.host_end)
	ret = 0;
    }
  else
    {
      struct target_mem_desc *tgt = gomp_malloc (sizeof (*tgt));
      tgt->array = gomp_malloc (sizeof (*tgt->array));
      tgt->refcount = 1;
      tgt->tgt_start = 0;
      tgt->tgt_end = 0;
      tgt->to_free = NULL;
      tgt->prev = NULL;
      tgt->list_count = 0;
      tgt->device_descr = devicep;
      splay_tree_node array = tgt->array;
      splay_tree_key k = &array->key;
      k->host_start = cur_node.host_start;
      k->host_end = cur_node.host_end;
      k->tgt = tgt;
      k->tgt_offset = (uintptr_t) device_ptr + device_offset;
      k->refcount = REFCOUNT_INFINITY;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      ret = 0;
    }
  gomp_mutex_unlock (&devicep->lock);
  return ret;
}

int
omp_target_disassociate_ptr (void *ptr, int device_num)
{
  if (device_num == GOMP_DEVICE_HOST_FALLBACK)
    return EINVAL;

  if (device_num < 0)
    return EINVAL;

  struct gomp_device_descr *devicep = resolve_device (device_num);
  if (devicep == NULL)
    return EINVAL;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400))
    return EINVAL;

  gomp_mutex_lock (&devicep->lock);

  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  int ret = EINVAL;

  cur_node.host_start = (uintptr_t) ptr;
  cur_node.host_end = cur_node.host_start;
  splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
  if (n
      && n->host_start == cur_node.host_start
      && n->refcount == REFCOUNT_INFINITY
      && n->tgt->tgt_start == 0
      && n->tgt->to_free == NULL
      && n->tgt->refcount == 1
      && n->tgt->list_count == 0)
    {
      splay_tree_remove (&devicep->mem_map, n);
      gomp_unmap_tgt (n->tgt);
      ret = 0;
    }

  gomp_mutex_unlock (&devicep->lock);
  return ret;
}

#ifdef PLUGIN_SUPPORT

/* This function tries to load a plugin for DEVICE.  Name of plugin is passed
   in PLUGIN_NAME.
   The handles of the found functions are stored in the corresponding fields
   of DEVICE.  The function returns TRUE on success and FALSE otherwise.  */

static bool
gomp_load_plugin_for_device (struct gomp_device_descr *device,
			     const char *plugin_name)
{
  const char *err = NULL, *last_missing = NULL;

  void *plugin_handle = dlopen (plugin_name, RTLD_LAZY);
  if (!plugin_handle)
    goto dl_fail;

  /* Check if all required functions are available in the plugin and store
     their handlers.  None of the symbols can legitimately be NULL,
     so we don't need to check dlerror all the time.  */
#define DLSYM(f)							\
  if (!(device->f##_func = dlsym (plugin_handle, "GOMP_OFFLOAD_" #f)))	\
    goto dl_fail
  /* Similar, but missing functions are not an error.  Return false if
     failed, true otherwise.  */
#define DLSYM_OPT(f, n)							\
  ((device->f##_func = dlsym (plugin_handle, "GOMP_OFFLOAD_" #n))	\
   || (last_missing = #n, 0))

  DLSYM (version);
  if (device->version_func () != GOMP_VERSION)
    {
      err = "plugin version mismatch";
      goto fail;
    }

  DLSYM (get_name);
  DLSYM (get_caps);
  DLSYM (get_type);
  DLSYM (get_num_devices);
  DLSYM (init_device);
  DLSYM (fini_device);
  DLSYM (load_image);
  DLSYM (unload_image);
  DLSYM (alloc);
  DLSYM (free);
  DLSYM (dev2host);
  DLSYM (host2dev);
  device->capabilities = device->get_caps_func ();
  if (device->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
    {
      DLSYM (run);
      DLSYM (async_run);
      DLSYM_OPT (can_run, can_run);
      DLSYM (dev2dev);
    }
  if (device->capabilities & GOMP_OFFLOAD_CAP_OPENACC_200)
    {
      if (!DLSYM_OPT (openacc.exec, openacc_parallel)
	  || !DLSYM_OPT (openacc.register_async_cleanup,
			 openacc_register_async_cleanup)
	  || !DLSYM_OPT (openacc.async_test, openacc_async_test)
	  || !DLSYM_OPT (openacc.async_test_all, openacc_async_test_all)
	  || !DLSYM_OPT (openacc.async_wait, openacc_async_wait)
	  || !DLSYM_OPT (openacc.async_wait_async, openacc_async_wait_async)
	  || !DLSYM_OPT (openacc.async_wait_all, openacc_async_wait_all)
	  || !DLSYM_OPT (openacc.async_wait_all_async,
			 openacc_async_wait_all_async)
	  || !DLSYM_OPT (openacc.async_set_async, openacc_async_set_async)
	  || !DLSYM_OPT (openacc.create_thread_data,
			 openacc_create_thread_data)
	  || !DLSYM_OPT (openacc.destroy_thread_data,
			 openacc_destroy_thread_data))
	{
	  /* Require all the OpenACC handlers if we have
	     GOMP_OFFLOAD_CAP_OPENACC_200.  */
	  err = "plugin missing OpenACC handler function";
	  goto fail;
	}

      unsigned cuda = 0;
      cuda += DLSYM_OPT (openacc.cuda.get_current_device,
			 openacc_get_current_cuda_device);
      cuda += DLSYM_OPT (openacc.cuda.get_current_context,
			 openacc_get_current_cuda_context);
      cuda += DLSYM_OPT (openacc.cuda.get_stream, openacc_get_cuda_stream);
      cuda += DLSYM_OPT (openacc.cuda.set_stream, openacc_set_cuda_stream);
      if (cuda && cuda != 4)
	{
	  /* Make sure all the CUDA functions are there if any of them are.  */
	  err = "plugin missing OpenACC CUDA handler function";
	  goto fail;
	}
    }
#undef DLSYM
#undef DLSYM_OPT

  return 1;

 dl_fail:
  err = dlerror ();
 fail:
  gomp_error ("while loading %s: %s", plugin_name, err);
  if (last_missing)
    gomp_error ("missing function was %s", last_missing);
  if (plugin_handle)
    dlclose (plugin_handle);

  return 0;
}

/* This function finalizes all initialized devices.  */

static void
gomp_target_fini (void)
{
  int i;
  for (i = 0; i < num_devices; i++)
    {
      bool ret = true;
      struct gomp_device_descr *devicep = &devices[i];
      gomp_mutex_lock (&devicep->lock);
      if (devicep->state == GOMP_DEVICE_INITIALIZED)
	{
	  ret = devicep->fini_device_func (devicep->target_id);
	  devicep->state = GOMP_DEVICE_FINALIZED;
	}
      gomp_mutex_unlock (&devicep->lock);
      if (!ret)
	gomp_fatal ("device finalization failed");
    }
}

/* This function initializes the runtime needed for offloading.
   It parses the list of offload targets and tries to load the plugins for
   these targets.  On return, the variables NUM_DEVICES and NUM_DEVICES_OPENMP
   will be set, and the array DEVICES initialized, containing descriptors for
   corresponding devices, first the GOMP_OFFLOAD_CAP_OPENMP_400 ones, follows
   by the others.  */

static void
gomp_target_init (void)
{
  const char *prefix ="libgomp-plugin-";
  const char *suffix = SONAME_SUFFIX (1);
  const char *cur, *next;
  char *plugin_name;
  int i, new_num_devices;

  num_devices = 0;
  devices = NULL;

  cur = OFFLOAD_TARGETS;
  if (*cur)
    do
      {
	struct gomp_device_descr current_device;

	next = strchr (cur, ',');

	plugin_name = (char *) malloc (1 + (next ? next - cur : strlen (cur))
				       + strlen (prefix) + strlen (suffix));
	if (!plugin_name)
	  {
	    num_devices = 0;
	    break;
	  }

	strcpy (plugin_name, prefix);
	strncat (plugin_name, cur, next ? next - cur : strlen (cur));
	strcat (plugin_name, suffix);

	if (gomp_load_plugin_for_device (&current_device, plugin_name))
	  {
	    new_num_devices = current_device.get_num_devices_func ();
	    if (new_num_devices >= 1)
	      {
		/* Augment DEVICES and NUM_DEVICES.  */

		devices = realloc (devices, (num_devices + new_num_devices)
				   * sizeof (struct gomp_device_descr));
		if (!devices)
		  {
		    num_devices = 0;
		    free (plugin_name);
		    break;
		  }

		current_device.name = current_device.get_name_func ();
		/* current_device.capabilities has already been set.  */
		current_device.type = current_device.get_type_func ();
		current_device.mem_map.root = NULL;
		current_device.state = GOMP_DEVICE_UNINITIALIZED;
		current_device.openacc.data_environ = NULL;
		for (i = 0; i < new_num_devices; i++)
		  {
		    current_device.target_id = i;
		    devices[num_devices] = current_device;
		    gomp_mutex_init (&devices[num_devices].lock);
		    num_devices++;
		  }
	      }
	  }

	free (plugin_name);
	cur = next + 1;
      }
    while (next);

  /* In DEVICES, sort the GOMP_OFFLOAD_CAP_OPENMP_400 ones first, and set
     NUM_DEVICES_OPENMP.  */
  struct gomp_device_descr *devices_s
    = malloc (num_devices * sizeof (struct gomp_device_descr));
  if (!devices_s)
    {
      num_devices = 0;
      free (devices);
      devices = NULL;
    }
  num_devices_openmp = 0;
  for (i = 0; i < num_devices; i++)
    if (devices[i].capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      devices_s[num_devices_openmp++] = devices[i];
  int num_devices_after_openmp = num_devices_openmp;
  for (i = 0; i < num_devices; i++)
    if (!(devices[i].capabilities & GOMP_OFFLOAD_CAP_OPENMP_400))
      devices_s[num_devices_after_openmp++] = devices[i];
  free (devices);
  devices = devices_s;

  for (i = 0; i < num_devices; i++)
    {
      /* The 'devices' array can be moved (by the realloc call) until we have
	 found all the plugins, so registering with the OpenACC runtime (which
	 takes a copy of the pointer argument) must be delayed until now.  */
      if (devices[i].capabilities & GOMP_OFFLOAD_CAP_OPENACC_200)
	goacc_register (&devices[i]);
    }

  if (atexit (gomp_target_fini) != 0)
    gomp_fatal ("atexit failed");
}

#else /* PLUGIN_SUPPORT */
/* If dlfcn.h is unavailable we always fallback to host execution.
   GOMP_target* routines are just stubs for this case.  */
static void
gomp_target_init (void)
{
}
#endif /* PLUGIN_SUPPORT */
