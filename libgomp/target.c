/* Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
#include <stdio.h>  /* For snprintf. */
#include <assert.h>
#include <errno.h>

#ifdef PLUGIN_SUPPORT
#include <dlfcn.h>
#include "plugin-suffix.h"
#endif

/* Define another splay tree instantiation - for reverse offload.  */
#define splay_tree_prefix reverse
#define splay_tree_c
#include "splay-tree.h"


typedef uintptr_t *hash_entry_type;
static inline void * htab_alloc (size_t size) { return gomp_malloc (size); }
static inline void htab_free (void *ptr) { free (ptr); }
#include "hashtab.h"

ialias_redirect (GOMP_task)

static inline hashval_t
htab_hash (hash_entry_type element)
{
  return hash_pointer ((void *) element);
}

static inline bool
htab_eq (hash_entry_type x, hash_entry_type y)
{
  return x == y;
}

#define FIELD_TGT_EMPTY (~(size_t) 0)

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

/* OpenMP requires mask.  */
static int omp_requires_mask;

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
resolve_device (int device_id, bool remapped)
{
  if (remapped && device_id == GOMP_DEVICE_ICV)
    {
      struct gomp_task_icv *icv = gomp_icv (false);
      device_id = icv->default_device_var;
      remapped = false;
    }

  if (device_id < 0)
    {
      if (device_id == (remapped ? GOMP_DEVICE_HOST_FALLBACK
				 : omp_initial_device))
	return NULL;
      if (device_id == omp_invalid_device)
	gomp_fatal ("omp_invalid_device encountered");
      else if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_MANDATORY)
	gomp_fatal ("OMP_TARGET_OFFLOAD is set to MANDATORY, "
		    "but device not found");

      return NULL;
    }
  else if (device_id >= gomp_get_num_devices ())
    {
      if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_MANDATORY
	  && device_id != num_devices_openmp)
	gomp_fatal ("OMP_TARGET_OFFLOAD is set to MANDATORY, "
		    "but device not found");

      return NULL;
    }

  gomp_mutex_lock (&devices[device_id].lock);
  if (devices[device_id].state == GOMP_DEVICE_UNINITIALIZED)
    gomp_init_device (&devices[device_id]);
  else if (devices[device_id].state == GOMP_DEVICE_FINALIZED)
    {
      gomp_mutex_unlock (&devices[device_id].lock);

      if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_MANDATORY)
	gomp_fatal ("OMP_TARGET_OFFLOAD is set to MANDATORY, "
		    "but device is finalized");

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

static inline reverse_splay_tree_key
gomp_map_lookup_rev (reverse_splay_tree mem_map_rev, reverse_splay_tree_key key)
{
  return reverse_splay_tree_lookup (mem_map_rev, key);
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

static inline void
goacc_device_copy_async (struct gomp_device_descr *devicep,
			 bool (*copy_func) (int, void *, const void *, size_t,
					    struct goacc_asyncqueue *),
			 const char *dst, void *dstaddr,
			 const char *src, const void *srcaddr,
			 const void *srcaddr_orig,
			 size_t size, struct goacc_asyncqueue *aq)
{
  if (!copy_func (devicep->target_id, dstaddr, srcaddr, size, aq))
    {
      gomp_mutex_unlock (&devicep->lock);
      if (srcaddr_orig && srcaddr_orig != srcaddr)
	gomp_fatal ("Copying of %s object [%p..%p)"
		    " via buffer %s object [%p..%p)"
		    " to %s object [%p..%p) failed",
		    src, srcaddr_orig, srcaddr_orig + size,
		    src, srcaddr, srcaddr + size,
		    dst, dstaddr, dstaddr + size);
      else
	gomp_fatal ("Copying of %s object [%p..%p)"
		    " to %s object [%p..%p) failed",
		    src, srcaddr, srcaddr + size,
		    dst, dstaddr, dstaddr + size);
    }
}

/* Infrastructure for coalescing adjacent or nearly adjacent (in device addresses)
   host to device memory transfers.  */

struct gomp_coalesce_chunk
{
  /* The starting and ending point of a coalesced chunk of memory.  */
  size_t start, end;
};

struct gomp_coalesce_buf
{
  /* Buffer into which gomp_copy_host2dev will memcpy data and from which
     it will be copied to the device.  */
  void *buf;
  struct target_mem_desc *tgt;
  /* Array with offsets, chunks[i].start is the starting offset and
     chunks[i].end ending offset relative to tgt->tgt_start device address
     of chunks which are to be copied to buf and later copied to device.  */
  struct gomp_coalesce_chunk *chunks;
  /* Number of chunks in chunks array, or -1 if coalesce buffering should not
     be performed.  */
  long chunk_cnt;
  /* During construction of chunks array, how many memory regions are within
     the last chunk.  If there is just one memory region for a chunk, we copy
     it directly to device rather than going through buf.  */
  long use_cnt;
};

/* Maximum size of memory region considered for coalescing.  Larger copies
   are performed directly.  */
#define MAX_COALESCE_BUF_SIZE	(32 * 1024)

/* Maximum size of a gap in between regions to consider them being copied
   within the same chunk.  All the device offsets considered are within
   newly allocated device memory, so it isn't fatal if we copy some padding
   in between from host to device.  The gaps come either from alignment
   padding or from memory regions which are not supposed to be copied from
   host to device (e.g. map(alloc:), map(from:) etc.).  */
#define MAX_COALESCE_BUF_GAP	(4 * 1024)

/* Add region with device tgt_start relative offset and length to CBUF.

   This must not be used for asynchronous copies, because the host data might
   not be computed yet (by an earlier asynchronous compute region, for
   example).  The exception is for EPHEMERAL data, that we know is available
   already "by construction".  */

static inline void
gomp_coalesce_buf_add (struct gomp_coalesce_buf *cbuf, size_t start, size_t len)
{
  if (len > MAX_COALESCE_BUF_SIZE || len == 0)
    return;
  if (cbuf->chunk_cnt)
    {
      if (cbuf->chunk_cnt < 0)
	return;
      if (start < cbuf->chunks[cbuf->chunk_cnt - 1].end)
	{
	  cbuf->chunk_cnt = -1;
	  return;
	}
      if (start < cbuf->chunks[cbuf->chunk_cnt - 1].end + MAX_COALESCE_BUF_GAP)
	{
	  cbuf->chunks[cbuf->chunk_cnt - 1].end = start + len;
	  cbuf->use_cnt++;
	  return;
	}
      /* If the last chunk is only used by one mapping, discard it,
	 as it will be one host to device copy anyway and
	 memcpying it around will only waste cycles.  */
      if (cbuf->use_cnt == 1)
	cbuf->chunk_cnt--;
    }
  cbuf->chunks[cbuf->chunk_cnt].start = start;
  cbuf->chunks[cbuf->chunk_cnt].end = start + len;
  cbuf->chunk_cnt++;
  cbuf->use_cnt = 1;
}

/* Return true for mapping kinds which need to copy data from the
   host to device for regions that weren't previously mapped.  */

static inline bool
gomp_to_device_kind_p (int kind)
{
  switch (kind)
    {
    case GOMP_MAP_ALLOC:
    case GOMP_MAP_FROM:
    case GOMP_MAP_FORCE_ALLOC:
    case GOMP_MAP_FORCE_FROM:
    case GOMP_MAP_ALWAYS_FROM:
      return false;
    default:
      return true;
    }
}

/* Copy host memory to an offload device.  In asynchronous mode (if AQ is
   non-NULL), when the source data is stack or may otherwise be deallocated
   before the asynchronous copy takes place, EPHEMERAL must be passed as
   TRUE.  */

attribute_hidden void
gomp_copy_host2dev (struct gomp_device_descr *devicep,
		    struct goacc_asyncqueue *aq,
		    void *d, const void *h, size_t sz,
		    bool ephemeral, struct gomp_coalesce_buf *cbuf)
{
  if (cbuf)
    {
      uintptr_t doff = (uintptr_t) d - cbuf->tgt->tgt_start;
      if (doff < cbuf->chunks[cbuf->chunk_cnt - 1].end)
	{
	  long first = 0;
	  long last = cbuf->chunk_cnt - 1;
	  while (first <= last)
	    {
	      long middle = (first + last) >> 1;
	      if (cbuf->chunks[middle].end <= doff)
		first = middle + 1;
	      else if (cbuf->chunks[middle].start <= doff)
		{
		  if (doff + sz > cbuf->chunks[middle].end)
		    {
		      gomp_mutex_unlock (&devicep->lock);
		      gomp_fatal ("internal libgomp cbuf error");
		    }

		  /* In an asynchronous context, verify that CBUF isn't used
		     with non-EPHEMERAL data; see 'gomp_coalesce_buf_add'.  */
		  if (__builtin_expect (aq != NULL, 0))
		    assert (ephemeral);

		  memcpy ((char *) cbuf->buf + (doff - cbuf->chunks[0].start),
			  h, sz);
		  return;
		}
	      else
		last = middle - 1;
	    }
	}
    }

  if (__builtin_expect (aq != NULL, 0))
    {
      void *h_buf = (void *) h;
      if (ephemeral)
	{
	  /* We're queueing up an asynchronous copy from data that may
	     disappear before the transfer takes place (i.e. because it is a
	     stack local in a function that is no longer executing).  As we've
	     not been able to use CBUF, make a copy of the data into a
	     temporary buffer.  */
	  h_buf = gomp_malloc (sz);
	  memcpy (h_buf, h, sz);
	}
      goacc_device_copy_async (devicep, devicep->openacc.async.host2dev_func,
			       "dev", d, "host", h_buf, h, sz, aq);
      if (ephemeral)
	/* Free once the transfer has completed.  */
	devicep->openacc.async.queue_callback_func (aq, free, h_buf);
    }
  else
    gomp_device_copy (devicep, devicep->host2dev_func,
		      "dev", d, "host", h, sz);
}

attribute_hidden void
gomp_copy_dev2host (struct gomp_device_descr *devicep,
		    struct goacc_asyncqueue *aq,
		    void *h, const void *d, size_t sz)
{
  if (__builtin_expect (aq != NULL, 0))
    goacc_device_copy_async (devicep, devicep->openacc.async.dev2host_func,
			     "host", h, "dev", d, NULL, sz, aq);
  else
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

/* Increment reference count of a splay_tree_key region K by 1.
   If REFCOUNT_SET != NULL, use it to track already seen refcounts, and only
   increment the value if refcount is not yet contained in the set (used for
   OpenMP 5.0, which specifies that a region's refcount is adjusted at most
   once for each construct).  */

static inline void
gomp_increment_refcount (splay_tree_key k, htab_t *refcount_set)
{
  if (k == NULL || k->refcount == REFCOUNT_INFINITY)
    return;

  uintptr_t *refcount_ptr = &k->refcount;

  if (REFCOUNT_STRUCTELEM_FIRST_P (k->refcount))
    refcount_ptr = &k->structelem_refcount;
  else if (REFCOUNT_STRUCTELEM_P (k->refcount))
    refcount_ptr = k->structelem_refcount_ptr;

  if (refcount_set)
    {
      if (htab_find (*refcount_set, refcount_ptr))
	return;
      uintptr_t **slot = htab_find_slot (refcount_set, refcount_ptr, INSERT);
      *slot = refcount_ptr;
    }

  *refcount_ptr += 1;
  return;
}

/* Decrement reference count of a splay_tree_key region K by 1, or if DELETE_P
   is true, set reference count to zero. If REFCOUNT_SET != NULL, use it to
   track already seen refcounts, and only adjust the value if refcount is not
   yet contained in the set (like gomp_increment_refcount).

   Return out-values: set *DO_COPY to true if we set the refcount to zero, or
   it is already zero and we know we decremented it earlier. This signals that
   associated maps should be copied back to host.

   *DO_REMOVE is set to true when we this is the first handling of this refcount
   and we are setting it to zero. This signals a removal of this key from the
   splay-tree map.

   Copy and removal are separated due to cases like handling of structure
   elements, e.g. each map of a structure element representing a possible copy
   out of a structure field has to be handled individually, but we only signal
   removal for one (the first encountered) sibing map.  */

static inline void
gomp_decrement_refcount (splay_tree_key k, htab_t *refcount_set, bool delete_p,
			 bool *do_copy, bool *do_remove)
{
  if (k == NULL || k->refcount == REFCOUNT_INFINITY)
    {
      *do_copy = *do_remove = false;
      return;
    }

  uintptr_t *refcount_ptr = &k->refcount;

  if (REFCOUNT_STRUCTELEM_FIRST_P (k->refcount))
    refcount_ptr = &k->structelem_refcount;
  else if (REFCOUNT_STRUCTELEM_P (k->refcount))
    refcount_ptr = k->structelem_refcount_ptr;

  bool new_encountered_refcount;
  bool set_to_zero = false;
  bool is_zero = false;

  uintptr_t orig_refcount = *refcount_ptr;

  if (refcount_set)
    {
      if (htab_find (*refcount_set, refcount_ptr))
	{
	  new_encountered_refcount = false;
	  goto end;
	}

      uintptr_t **slot = htab_find_slot (refcount_set, refcount_ptr, INSERT);
      *slot = refcount_ptr;
      new_encountered_refcount = true;
    }
  else
    /* If no refcount_set being used, assume all keys are being decremented
       for the first time.  */
    new_encountered_refcount = true;

  if (delete_p)
    *refcount_ptr = 0;
  else if (*refcount_ptr > 0)
    *refcount_ptr -= 1;

 end:
  if (*refcount_ptr == 0)
    {
      if (orig_refcount > 0)
	set_to_zero = true;

      is_zero = true;
    }

  *do_copy = (set_to_zero || (!new_encountered_refcount && is_zero));
  *do_remove = (new_encountered_refcount && set_to_zero);
}

/* Handle the case where gomp_map_lookup, splay_tree_lookup or
   gomp_map_0len_lookup found oldn for newn.
   Helper function of gomp_map_vars.  */

static inline void
gomp_map_vars_existing (struct gomp_device_descr *devicep,
			struct goacc_asyncqueue *aq, splay_tree_key oldn,
			splay_tree_key newn, struct target_var_desc *tgt_var,
			unsigned char kind, bool always_to_flag, bool implicit,
			struct gomp_coalesce_buf *cbuf,
			htab_t *refcount_set)
{
  assert (kind != GOMP_MAP_ATTACH
	  || kind != GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION);

  tgt_var->key = oldn;
  tgt_var->copy_from = GOMP_MAP_COPY_FROM_P (kind);
  tgt_var->always_copy_from = GOMP_MAP_ALWAYS_FROM_P (kind);
  tgt_var->is_attach = false;
  tgt_var->offset = newn->host_start - oldn->host_start;

  /* For implicit maps, old contained in new is valid.  */
  bool implicit_subset = (implicit
			  && newn->host_start <= oldn->host_start
			  && oldn->host_end <= newn->host_end);
  if (implicit_subset)
    tgt_var->length = oldn->host_end - oldn->host_start;
  else
    tgt_var->length = newn->host_end - newn->host_start;

  if ((kind & GOMP_MAP_FLAG_FORCE)
      /* For implicit maps, old contained in new is valid.  */
      || !(implicit_subset
	   /* Otherwise, new contained inside old is considered valid.  */
	   || (oldn->host_start <= newn->host_start
	       && newn->host_end <= oldn->host_end)))
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("Trying to map into device [%p..%p) object when "
		  "[%p..%p) is already mapped",
		  (void *) newn->host_start, (void *) newn->host_end,
		  (void *) oldn->host_start, (void *) oldn->host_end);
    }

  if (GOMP_MAP_ALWAYS_TO_P (kind) || always_to_flag)
    {
      /* Implicit + always should not happen. If this does occur, below
	 address/length adjustment is a TODO.  */
      assert (!implicit_subset);

      if (oldn->aux && oldn->aux->attach_count)
	{
	  /* We have to be careful not to overwrite still attached pointers
	     during the copyback to host.  */
	  uintptr_t addr = newn->host_start;
	  while (addr < newn->host_end)
	    {
	      size_t i = (addr - oldn->host_start) / sizeof (void *);
	      if (oldn->aux->attach_count[i] == 0)
		gomp_copy_host2dev (devicep, aq,
				    (void *) (oldn->tgt->tgt_start
					      + oldn->tgt_offset
					      + addr - oldn->host_start),
				    (void *) addr,
				    sizeof (void *), false, cbuf);
	      addr += sizeof (void *);
	    }
	}
      else
	gomp_copy_host2dev (devicep, aq,
			    (void *) (oldn->tgt->tgt_start + oldn->tgt_offset
				      + newn->host_start - oldn->host_start),
			    (void *) newn->host_start,
			    newn->host_end - newn->host_start, false, cbuf);
    }

  gomp_increment_refcount (oldn, refcount_set);
}

static int
get_kind (bool short_mapkind, void *kinds, int idx)
{
  if (!short_mapkind)
    return ((unsigned char *) kinds)[idx];

  int val = ((unsigned short *) kinds)[idx];
  if (GOMP_MAP_IMPLICIT_P (val))
    val &= ~GOMP_MAP_IMPLICIT;
  return val;
}


static bool
get_implicit (bool short_mapkind, void *kinds, int idx)
{
  if (!short_mapkind)
    return false;

  int val = ((unsigned short *) kinds)[idx];
  return GOMP_MAP_IMPLICIT_P (val);
}

static void
gomp_map_pointer (struct target_mem_desc *tgt, struct goacc_asyncqueue *aq,
		  uintptr_t host_ptr, uintptr_t target_offset, uintptr_t bias,
		  struct gomp_coalesce_buf *cbuf,
		  bool allow_zero_length_array_sections)
{
  struct gomp_device_descr *devicep = tgt->device_descr;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;

  cur_node.host_start = host_ptr;
  if (cur_node.host_start == (uintptr_t) NULL)
    {
      cur_node.tgt_offset = (uintptr_t) NULL;
      gomp_copy_host2dev (devicep, aq,
			  (void *) (tgt->tgt_start + target_offset),
			  (void *) &cur_node.tgt_offset, sizeof (void *),
			  true, cbuf);
      return;
    }
  /* Add bias to the pointer value.  */
  cur_node.host_start += bias;
  cur_node.host_end = cur_node.host_start;
  splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
  if (n == NULL)
    {
      if (allow_zero_length_array_sections)
	cur_node.tgt_offset = 0;
      else
	{
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("Pointer target of array section wasn't mapped");
	}
    }
  else
    {
      cur_node.host_start -= n->host_start;
      cur_node.tgt_offset
	= n->tgt->tgt_start + n->tgt_offset + cur_node.host_start;
      /* At this point tgt_offset is target address of the
	 array section.  Now subtract bias to get what we want
	 to initialize the pointer with.  */
      cur_node.tgt_offset -= bias;
    }
  gomp_copy_host2dev (devicep, aq, (void *) (tgt->tgt_start + target_offset),
		      (void *) &cur_node.tgt_offset, sizeof (void *),
		      true, cbuf);
}

static void
gomp_map_fields_existing (struct target_mem_desc *tgt,
			  struct goacc_asyncqueue *aq, splay_tree_key n,
			  size_t first, size_t i, void **hostaddrs,
			  size_t *sizes, void *kinds,
			  struct gomp_coalesce_buf *cbuf, htab_t *refcount_set)
{
  struct gomp_device_descr *devicep = tgt->device_descr;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  int kind;
  bool implicit;
  const bool short_mapkind = true;
  const int typemask = short_mapkind ? 0xff : 0x7;

  cur_node.host_start = (uintptr_t) hostaddrs[i];
  cur_node.host_end = cur_node.host_start + sizes[i];
  splay_tree_key n2 = splay_tree_lookup (mem_map, &cur_node);
  kind = get_kind (short_mapkind, kinds, i);
  implicit = get_implicit (short_mapkind, kinds, i);
  if (n2
      && n2->tgt == n->tgt
      && n2->host_start - n->host_start == n2->tgt_offset - n->tgt_offset)
    {
      gomp_map_vars_existing (devicep, aq, n2, &cur_node, &tgt->list[i],
			      kind & typemask, false, implicit, cbuf,
			      refcount_set);
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
	      gomp_map_vars_existing (devicep, aq, n2, &cur_node, &tgt->list[i],
				      kind & typemask, false, implicit, cbuf,
				      refcount_set);
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
	  gomp_map_vars_existing (devicep, aq, n2, &cur_node, &tgt->list[i],
				  kind & typemask, false, implicit, cbuf,
				  refcount_set);
	  return;
	}
    }
  gomp_mutex_unlock (&devicep->lock);
  gomp_fatal ("Trying to map into device [%p..%p) structure element when "
	      "other mapped elements from the same structure weren't mapped "
	      "together with it", (void *) cur_node.host_start,
	      (void *) cur_node.host_end);
}

attribute_hidden void
gomp_attach_pointer (struct gomp_device_descr *devicep,
		     struct goacc_asyncqueue *aq, splay_tree mem_map,
		     splay_tree_key n, uintptr_t attach_to, size_t bias,
		     struct gomp_coalesce_buf *cbufp,
		     bool allow_zero_length_array_sections)
{
  struct splay_tree_key_s s;
  size_t size, idx;

  if (n == NULL)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("enclosing struct not mapped for attach");
    }

  size = (n->host_end - n->host_start + sizeof (void *) - 1) / sizeof (void *);
  /* We might have a pointer in a packed struct: however we cannot have more
     than one such pointer in each pointer-sized portion of the struct, so
     this is safe.  */
  idx = (attach_to - n->host_start) / sizeof (void *);

  if (!n->aux)
    n->aux = gomp_malloc_cleared (sizeof (struct splay_tree_aux));

  if (!n->aux->attach_count)
    n->aux->attach_count
      = gomp_malloc_cleared (sizeof (*n->aux->attach_count) * size);

  if (n->aux->attach_count[idx] < UINTPTR_MAX)
    n->aux->attach_count[idx]++;
  else
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("attach count overflow");
    }

  if (n->aux->attach_count[idx] == 1)
    {
      uintptr_t devptr = n->tgt->tgt_start + n->tgt_offset + attach_to
			 - n->host_start;
      uintptr_t target = (uintptr_t) *(void **) attach_to;
      splay_tree_key tn;
      uintptr_t data;

      if ((void *) target == NULL)
	{
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("attempt to attach null pointer");
	}

      s.host_start = target + bias;
      s.host_end = s.host_start + 1;
      tn = splay_tree_lookup (mem_map, &s);

      if (!tn)
	{
	  if (allow_zero_length_array_sections)
	    /* When allowing attachment to zero-length array sections, we
	       allow attaching to NULL pointers when the target region is not
	       mapped.  */
	    data = 0;
	  else
	    {
	      gomp_mutex_unlock (&devicep->lock);
	      gomp_fatal ("pointer target not mapped for attach");
	    }
	}
      else
	data = tn->tgt->tgt_start + tn->tgt_offset + target - tn->host_start;

      gomp_debug (1,
		  "%s: attaching host %p, target %p (struct base %p) to %p\n",
		  __FUNCTION__, (void *) attach_to, (void *) devptr,
		  (void *) (n->tgt->tgt_start + n->tgt_offset), (void *) data);

      gomp_copy_host2dev (devicep, aq, (void *) devptr, (void *) &data,
			  sizeof (void *), true, cbufp);
    }
  else
    gomp_debug (1, "%s: attach count for %p -> %u\n", __FUNCTION__,
		(void *) attach_to, (int) n->aux->attach_count[idx]);
}

attribute_hidden void
gomp_detach_pointer (struct gomp_device_descr *devicep,
		     struct goacc_asyncqueue *aq, splay_tree_key n,
		     uintptr_t detach_from, bool finalize,
		     struct gomp_coalesce_buf *cbufp)
{
  size_t idx;

  if (n == NULL)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("enclosing struct not mapped for detach");
    }

  idx = (detach_from - n->host_start) / sizeof (void *);

  if (!n->aux || !n->aux->attach_count)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("no attachment counters for struct");
    }

  if (finalize)
    n->aux->attach_count[idx] = 1;

  if (n->aux->attach_count[idx] == 0)
    {
      gomp_mutex_unlock (&devicep->lock);
      gomp_fatal ("attach count underflow");
    }
  else
    n->aux->attach_count[idx]--;

  if (n->aux->attach_count[idx] == 0)
    {
      uintptr_t devptr = n->tgt->tgt_start + n->tgt_offset + detach_from
			 - n->host_start;
      uintptr_t target = (uintptr_t) *(void **) detach_from;

      gomp_debug (1,
		  "%s: detaching host %p, target %p (struct base %p) to %p\n",
		  __FUNCTION__, (void *) detach_from, (void *) devptr,
		  (void *) (n->tgt->tgt_start + n->tgt_offset),
		  (void *) target);

      gomp_copy_host2dev (devicep, aq, (void *) devptr, (void *) &target,
			  sizeof (void *), true, cbufp);
    }
  else
    gomp_debug (1, "%s: attach count for %p -> %u\n", __FUNCTION__,
		(void *) detach_from, (int) n->aux->attach_count[idx]);
}

attribute_hidden uintptr_t
gomp_map_val (struct target_mem_desc *tgt, void **hostaddrs, size_t i)
{
  if (tgt->list[i].key != NULL)
    return tgt->list[i].key->tgt->tgt_start
	   + tgt->list[i].key->tgt_offset
	   + tgt->list[i].offset;

  switch (tgt->list[i].offset)
    {
    case OFFSET_INLINED:
      return (uintptr_t) hostaddrs[i];

    case OFFSET_POINTER:
      return 0;

    case OFFSET_STRUCT:
      return tgt->list[i + 1].key->tgt->tgt_start
	     + tgt->list[i + 1].key->tgt_offset
	     + tgt->list[i + 1].offset
	     + (uintptr_t) hostaddrs[i]
	     - (uintptr_t) hostaddrs[i + 1];

    default:
      return tgt->tgt_start + tgt->list[i].offset;
    }
}

static inline __attribute__((always_inline)) struct target_mem_desc *
gomp_map_vars_internal (struct gomp_device_descr *devicep,
			struct goacc_asyncqueue *aq, size_t mapnum,
			void **hostaddrs, void **devaddrs, size_t *sizes,
			void *kinds, bool short_mapkind,
			htab_t *refcount_set,
			enum gomp_map_vars_kind pragma_kind)
{
  size_t i, tgt_align, tgt_size, not_found_cnt = 0;
  bool has_firstprivate = false;
  bool has_always_ptrset = false;
  bool openmp_p = (pragma_kind & GOMP_MAP_VARS_OPENACC) == 0;
  const int rshift = short_mapkind ? 8 : 3;
  const int typemask = short_mapkind ? 0xff : 0x7;
  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  struct target_mem_desc *tgt
    = gomp_malloc (sizeof (*tgt) + sizeof (tgt->list[0]) * mapnum);
  tgt->list_count = mapnum;
  tgt->refcount = (pragma_kind & GOMP_MAP_VARS_ENTER_DATA) ? 0 : 1;
  tgt->device_descr = devicep;
  tgt->prev = NULL;
  struct gomp_coalesce_buf cbuf, *cbufp = NULL;

  if (mapnum == 0)
    {
      tgt->tgt_start = 0;
      tgt->tgt_end = 0;
      return tgt;
    }

  tgt_align = sizeof (void *);
  tgt_size = 0;
  cbuf.chunks = NULL;
  cbuf.chunk_cnt = -1;
  cbuf.use_cnt = 0;
  cbuf.buf = NULL;
  if (mapnum > 1 || (pragma_kind & GOMP_MAP_VARS_TARGET))
    {
      size_t chunks_size = (mapnum + 1) * sizeof (struct gomp_coalesce_chunk);
      cbuf.chunks = (struct gomp_coalesce_chunk *) gomp_alloca (chunks_size);
      cbuf.chunk_cnt = 0;
    }
  if (pragma_kind & GOMP_MAP_VARS_TARGET)
    {
      size_t align = 4 * sizeof (void *);
      tgt_align = align;
      tgt_size = mapnum * sizeof (void *);
      cbuf.chunk_cnt = 1;
      cbuf.use_cnt = 1 + (mapnum > 1);
      cbuf.chunks[0].start = 0;
      cbuf.chunks[0].end = tgt_size;
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
      bool implicit = get_implicit (short_mapkind, kinds, i);
      if (hostaddrs[i] == NULL
	  || (kind & typemask) == GOMP_MAP_FIRSTPRIVATE_INT)
	{
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = OFFSET_INLINED;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_USE_DEVICE_PTR
	       || (kind & typemask) == GOMP_MAP_USE_DEVICE_PTR_IF_PRESENT)
	{
	  tgt->list[i].key = NULL;
	  if (!not_found_cnt)
	    {
	      /* In OpenMP < 5.0 and OpenACC the mapping has to be done
		 on a separate construct prior to using use_device_{addr,ptr}.
		 In OpenMP 5.0, map directives need to be ordered by the
		 middle-end before the use_device_* clauses.  If
		 !not_found_cnt, all mappings requested (if any) are already
		 mapped, so use_device_{addr,ptr} can be resolved right away.
		 Otherwise, if not_found_cnt, gomp_map_lookup might fail
		 now but would succeed after performing the mappings in the
		 following loop.  We can't defer this always to the second
		 loop, because it is not even invoked when !not_found_cnt
		 after the first loop.  */
	      cur_node.host_start = (uintptr_t) hostaddrs[i];
	      cur_node.host_end = cur_node.host_start;
	      splay_tree_key n = gomp_map_lookup (mem_map, &cur_node);
	      if (n != NULL)
		{
		  cur_node.host_start -= n->host_start;
		  hostaddrs[i]
		    = (void *) (n->tgt->tgt_start + n->tgt_offset
				+ cur_node.host_start);
		}
	      else if ((kind & typemask) == GOMP_MAP_USE_DEVICE_PTR)
		{
		  gomp_mutex_unlock (&devicep->lock);
		  gomp_fatal ("use_device_ptr pointer wasn't mapped");
		}
	      else if ((kind & typemask) == GOMP_MAP_USE_DEVICE_PTR_IF_PRESENT)
		/* If not present, continue using the host address.  */
		;
	      else
		__builtin_unreachable ();
	      tgt->list[i].offset = OFFSET_INLINED;
	    }
	  else
	    tgt->list[i].offset = 0;
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
	  tgt->list[i].offset = OFFSET_STRUCT;
	  splay_tree_key n = splay_tree_lookup (mem_map, &cur_node);
	  if (n == NULL)
	    {
	      size_t align = (size_t) 1 << (kind >> rshift);
	      if (tgt_align < align)
		tgt_align = align;
	      tgt_size -= (uintptr_t) hostaddrs[first] - cur_node.host_start;
	      tgt_size = (tgt_size + align - 1) & ~(align - 1);
	      tgt_size += cur_node.host_end - cur_node.host_start;
	      not_found_cnt += last - i;
	      for (i = first; i <= last; i++)
		{
		  tgt->list[i].key = NULL;
		  if (!aq
		      && gomp_to_device_kind_p (get_kind (short_mapkind, kinds, i)
						& typemask))
		    gomp_coalesce_buf_add (&cbuf,
					   tgt_size - cur_node.host_end
					   + (uintptr_t) hostaddrs[i],
					   sizes[i]);
		}
	      i--;
	      continue;
	    }
	  for (i = first; i <= last; i++)
	    gomp_map_fields_existing (tgt, aq, n, first, i, hostaddrs,
				      sizes, kinds, NULL, refcount_set);
	  i--;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_ALWAYS_POINTER)
	{
	  tgt->list[i].key = NULL;
	  tgt->list[i].offset = OFFSET_POINTER;
	  has_firstprivate = true;
	  continue;
	}
      else if ((kind & typemask) == GOMP_MAP_ATTACH
	       || ((kind & typemask)
		   == GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION))
	{
	  tgt->list[i].key = NULL;
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
	  if (!aq)
	    gomp_coalesce_buf_add (&cbuf, tgt_size,
				   cur_node.host_end - cur_node.host_start);
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
	      tgt->list[i].offset = OFFSET_POINTER;
	      continue;
	    }
	}
      else
	n = splay_tree_lookup (mem_map, &cur_node);
      if (n && n->refcount != REFCOUNT_LINK)
	{
	  int always_to_cnt = 0;
	  if ((kind & typemask) == GOMP_MAP_TO_PSET)
	    {
	      bool has_nullptr = false;
	      size_t j;
	      for (j = 0; j < n->tgt->list_count; j++)
		if (n->tgt->list[j].key == n)
		  {
		    has_nullptr = n->tgt->list[j].has_null_ptr_assoc;
		    break;
		  }
	      if (n->tgt->list_count == 0)
		{
		  /* 'declare target'; assume has_nullptr; it could also be
		     statically assigned pointer, but that it should be to
		     the equivalent variable on the host.  */
		  assert (n->refcount == REFCOUNT_INFINITY);
		  has_nullptr = true;
		}
	      else
		assert (j < n->tgt->list_count);
	      /* Re-map the data if there is an 'always' modifier or if it a
		 null pointer was there and non a nonnull has been found; that
		 permits transparent re-mapping for Fortran array descriptors
		 which were previously mapped unallocated.  */
	      for (j = i + 1; j < mapnum; j++)
		{
		  int ptr_kind = get_kind (short_mapkind, kinds, j) & typemask;
		  if (!GOMP_MAP_ALWAYS_POINTER_P (ptr_kind)
		      && (!has_nullptr
			  || !GOMP_MAP_POINTER_P (ptr_kind)
			  || *(void **) hostaddrs[j] == NULL))
		    break;
		  else if ((uintptr_t) hostaddrs[j] < cur_node.host_start
			   || ((uintptr_t) hostaddrs[j] + sizeof (void *)
			       > cur_node.host_end))
		    break;
		  else
		    {
		      has_always_ptrset = true;
		      ++always_to_cnt;
		    }
		}
	    }
	  gomp_map_vars_existing (devicep, aq, n, &cur_node, &tgt->list[i],
				  kind & typemask, always_to_cnt > 0, implicit,
				  NULL, refcount_set);
	  i += always_to_cnt;
	}
      else
	{
	  tgt->list[i].key = NULL;

	  if ((kind & typemask) == GOMP_MAP_IF_PRESENT)
	    {
	      /* Not present, hence, skip entry - including its MAP_POINTER,
		 when existing.  */
	      tgt->list[i].offset = OFFSET_INLINED;
	      if (i + 1 < mapnum
		  && ((typemask & get_kind (short_mapkind, kinds, i + 1))
		      == GOMP_MAP_POINTER))
		{
		  ++i;
		  tgt->list[i].key = NULL;
		  tgt->list[i].offset = 0;
		}
	      continue;
	    }
	  size_t align = (size_t) 1 << (kind >> rshift);
	  not_found_cnt++;
	  if (tgt_align < align)
	    tgt_align = align;
	  tgt_size = (tgt_size + align - 1) & ~(align - 1);
	  if (!aq
	      && gomp_to_device_kind_p (kind & typemask))
	    gomp_coalesce_buf_add (&cbuf, tgt_size,
				   cur_node.host_end - cur_node.host_start);
	  tgt_size += cur_node.host_end - cur_node.host_start;
	  if ((kind & typemask) == GOMP_MAP_TO_PSET)
	    {
	      size_t j;
	      int kind;
	      for (j = i + 1; j < mapnum; j++)
		if (!GOMP_MAP_POINTER_P ((kind = (get_kind (short_mapkind,
						  kinds, j)) & typemask))
		    && !GOMP_MAP_ALWAYS_POINTER_P (kind))
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
  else if (not_found_cnt || (pragma_kind & GOMP_MAP_VARS_TARGET))
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

      if (cbuf.use_cnt == 1)
	cbuf.chunk_cnt--;
      if (cbuf.chunk_cnt > 0)
	{
	  cbuf.buf
	    = malloc (cbuf.chunks[cbuf.chunk_cnt - 1].end - cbuf.chunks[0].start);
	  if (cbuf.buf)
	    {
	      cbuf.tgt = tgt;
	      cbufp = &cbuf;
	    }
	}
    }
  else
    {
      tgt->to_free = NULL;
      tgt->tgt_start = 0;
      tgt->tgt_end = 0;
    }

  tgt_size = 0;
  if (pragma_kind & GOMP_MAP_VARS_TARGET)
    tgt_size = mapnum * sizeof (void *);

  tgt->array = NULL;
  if (not_found_cnt || has_firstprivate || has_always_ptrset)
    {
      if (not_found_cnt)
	tgt->array = gomp_malloc (not_found_cnt * sizeof (*tgt->array));
      splay_tree_node array = tgt->array;
      size_t j, field_tgt_offset = 0, field_tgt_clear = FIELD_TGT_EMPTY;
      uintptr_t field_tgt_base = 0;
      splay_tree_key field_tgt_structelem_first = NULL;

      for (i = 0; i < mapnum; i++)
	if (has_always_ptrset
	    && tgt->list[i].key
	    && (get_kind (short_mapkind, kinds, i) & typemask)
	       == GOMP_MAP_TO_PSET)
	  {
	    splay_tree_key k = tgt->list[i].key;
	    bool has_nullptr = false;
	    size_t j;
	    for (j = 0; j < k->tgt->list_count; j++)
	      if (k->tgt->list[j].key == k)
		{
		  has_nullptr = k->tgt->list[j].has_null_ptr_assoc;
		  break;
		}
	    if (k->tgt->list_count == 0)
	      has_nullptr = true;
	    else
	      assert (j < k->tgt->list_count);

	    tgt->list[i].has_null_ptr_assoc = false;
	    for (j = i + 1; j < mapnum; j++)
	      {
		int ptr_kind = get_kind (short_mapkind, kinds, j) & typemask;
		if (!GOMP_MAP_ALWAYS_POINTER_P (ptr_kind)
		    && (!has_nullptr
			|| !GOMP_MAP_POINTER_P (ptr_kind)
			|| *(void **) hostaddrs[j] == NULL))
		  break;
		else if ((uintptr_t) hostaddrs[j] < k->host_start
			 || ((uintptr_t) hostaddrs[j] + sizeof (void *)
			     > k->host_end))
		  break;
		else
		  {
		    if (*(void **) hostaddrs[j] == NULL)
		      tgt->list[i].has_null_ptr_assoc = true;
		    tgt->list[j].key = k;
		    tgt->list[j].copy_from = false;
		    tgt->list[j].always_copy_from = false;
		    tgt->list[j].is_attach = false;
		    gomp_increment_refcount (k, refcount_set);
		    gomp_map_pointer (k->tgt, aq,
				      (uintptr_t) *(void **) hostaddrs[j],
				      k->tgt_offset + ((uintptr_t) hostaddrs[j]
						       - k->host_start),
				      sizes[j], cbufp, false);
		  }
	      }
	    i = j - 1;
	  }
	else if (tgt->list[i].key == NULL)
	  {
	    int kind = get_kind (short_mapkind, kinds, i);
	    bool implicit = get_implicit (short_mapkind, kinds, i);
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
		gomp_copy_host2dev (devicep, aq,
				    (void *) (tgt->tgt_start + tgt_size),
				    (void *) hostaddrs[i], len, false, cbufp);
		/* Save device address in hostaddr to permit latter availablity
		   when doing a deep-firstprivate with pointer attach.  */
		hostaddrs[i] = (void *) (tgt->tgt_start + tgt_size);
		tgt_size += len;

		/* If followed by GOMP_MAP_ATTACH, pointer assign this
		   firstprivate to hostaddrs[i+1], which is assumed to contain a
		   device address.  */
		if (i + 1 < mapnum
		    && (GOMP_MAP_ATTACH
			== (typemask & get_kind (short_mapkind, kinds, i+1))))
		  {
		    uintptr_t target = (uintptr_t) hostaddrs[i];
		    void *devptr = *(void**) hostaddrs[i+1] + sizes[i+1];
		    /* Per
		       <https://inbox.sourceware.org/gcc-patches/87o7pe12ke.fsf@euler.schwinge.homeip.net>
		       "OpenMP: Handle descriptors in target's firstprivate [PR104949]"
		       this probably needs revision for 'aq' usage.  */
		    assert (!aq);
		    gomp_copy_host2dev (devicep, aq, devptr, &target,
					sizeof (void *), false, cbufp);
		    ++i;
		  }
		continue;
	      case GOMP_MAP_FIRSTPRIVATE_INT:
	      case GOMP_MAP_ZERO_LEN_ARRAY_SECTION:
		continue;
	      case GOMP_MAP_USE_DEVICE_PTR_IF_PRESENT:
		/* The OpenACC 'host_data' construct only allows 'use_device'
		   "mapping" clauses, so in the first loop, 'not_found_cnt'
		   must always have been zero, so all OpenACC 'use_device'
		   clauses have already been handled.  (We can only easily test
		   'use_device' with 'if_present' clause here.)  */
		assert (tgt->list[i].offset == OFFSET_INLINED);
		/* Nevertheless, FALLTHRU to the normal handling, to keep the
		   code conceptually simple, similar to the first loop.  */
	      case GOMP_MAP_USE_DEVICE_PTR:
		if (tgt->list[i].offset == 0)
		  {
		    cur_node.host_start = (uintptr_t) hostaddrs[i];
		    cur_node.host_end = cur_node.host_start;
		    n = gomp_map_lookup (mem_map, &cur_node);
		    if (n != NULL)
		      {
			cur_node.host_start -= n->host_start;
			hostaddrs[i]
			  = (void *) (n->tgt->tgt_start + n->tgt_offset
				      + cur_node.host_start);
		      }
		    else if ((kind & typemask) == GOMP_MAP_USE_DEVICE_PTR)
		      {
			gomp_mutex_unlock (&devicep->lock);
			gomp_fatal ("use_device_ptr pointer wasn't mapped");
		      }
		    else if ((kind & typemask)
			     == GOMP_MAP_USE_DEVICE_PTR_IF_PRESENT)
		      /* If not present, continue using the host address.  */
		      ;
		    else
		      __builtin_unreachable ();
		    tgt->list[i].offset = OFFSET_INLINED;
		  }
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
		    field_tgt_structelem_first = NULL;
		    tgt_size += cur_node.host_end
				- (uintptr_t) hostaddrs[first];
		    continue;
		  }
		for (i = first; i <= last; i++)
		  gomp_map_fields_existing (tgt, aq, n, first, i, hostaddrs,
					    sizes, kinds, cbufp, refcount_set);
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
		if (i > 0
		    && ((get_kind (short_mapkind, kinds, i - 1) & typemask)
			!= GOMP_MAP_ALWAYS_POINTER))
		  cur_node.tgt_offset = gomp_map_val (tgt, hostaddrs, i - 1);
		if (cur_node.tgt_offset)
		  cur_node.tgt_offset -= sizes[i];
		gomp_copy_host2dev (devicep, aq,
				    (void *) (n->tgt->tgt_start
					      + n->tgt_offset
					      + cur_node.host_start
					      - n->host_start),
				    (void *) &cur_node.tgt_offset,
				    sizeof (void *), true, cbufp);
		cur_node.tgt_offset = n->tgt->tgt_start + n->tgt_offset
				      + cur_node.host_start - n->host_start;
		continue;
	      case GOMP_MAP_IF_PRESENT:
		/* Not present - otherwise handled above. Skip over its
		   MAP_POINTER as well.  */
		if (i + 1 < mapnum
		    && ((typemask & get_kind (short_mapkind, kinds, i + 1))
			== GOMP_MAP_POINTER))
		  ++i;
		continue;
	      case GOMP_MAP_ATTACH:
	      case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
		{
		  cur_node.host_start = (uintptr_t) hostaddrs[i];
		  cur_node.host_end = cur_node.host_start + sizeof (void *);
		  splay_tree_key n = splay_tree_lookup (mem_map, &cur_node);
		  if (n != NULL)
		    {
		      tgt->list[i].key = n;
		      tgt->list[i].offset = cur_node.host_start - n->host_start;
		      tgt->list[i].length = n->host_end - n->host_start;
		      tgt->list[i].copy_from = false;
		      tgt->list[i].always_copy_from = false;
		      tgt->list[i].is_attach = true;
		      /* OpenACC 'attach'/'detach' doesn't affect
			 structured/dynamic reference counts ('n->refcount',
			 'n->dynamic_refcount').  */

		      bool zlas
			= ((kind & typemask)
			   == GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION);
		      gomp_attach_pointer (devicep, aq, mem_map, n,
					   (uintptr_t) hostaddrs[i], sizes[i],
					   cbufp, zlas);
		    }
		  else if ((pragma_kind & GOMP_MAP_VARS_OPENACC) != 0)
		    {
		      gomp_mutex_unlock (&devicep->lock);
		      gomp_fatal ("outer struct not mapped for attach");
		    }
		  continue;
		}
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
	      gomp_map_vars_existing (devicep, aq, n, k, &tgt->list[i],
				      kind & typemask, false, implicit, cbufp,
				      refcount_set);
	    else
	      {
		k->aux = NULL;
		if (n && n->refcount == REFCOUNT_LINK)
		  {
		    /* Replace target address of the pointer with target address
		       of mapped object in the splay tree.  */
		    splay_tree_remove (mem_map, n);
		    k->aux
		      = gomp_malloc_cleared (sizeof (struct splay_tree_aux));
		    k->aux->link_key = n;
		  }
		size_t align = (size_t) 1 << (kind >> rshift);
		tgt->list[i].key = k;
		k->tgt = tgt;
		k->refcount = 0;
		k->dynamic_refcount = 0;
		if (field_tgt_clear != FIELD_TGT_EMPTY)
		  {
		    k->tgt_offset = k->host_start - field_tgt_base
				    + field_tgt_offset;
		    if (openmp_p)
		      {
			k->refcount = REFCOUNT_STRUCTELEM;
			if (field_tgt_structelem_first == NULL)
			  {
			    /* Set to first structure element of sequence.  */
			    k->refcount |= REFCOUNT_STRUCTELEM_FLAG_FIRST;
			    field_tgt_structelem_first = k;
			  }
			else
			  /* Point to refcount of leading element, but do not
			     increment again.  */
			  k->structelem_refcount_ptr
			    = &field_tgt_structelem_first->structelem_refcount;

			if (i == field_tgt_clear)
			  {
			    k->refcount |= REFCOUNT_STRUCTELEM_FLAG_LAST;
			    field_tgt_structelem_first = NULL;
			  }
		      }
		    if (i == field_tgt_clear)
		      field_tgt_clear = FIELD_TGT_EMPTY;
		  }
		else
		  {
		    tgt_size = (tgt_size + align - 1) & ~(align - 1);
		    k->tgt_offset = tgt_size;
		    tgt_size += k->host_end - k->host_start;
		  }
		/* First increment, from 0 to 1. gomp_increment_refcount
		   encapsulates the different increment cases, so use this
		   instead of directly setting 1 during initialization.  */
		gomp_increment_refcount (k, refcount_set);

		tgt->list[i].copy_from = GOMP_MAP_COPY_FROM_P (kind & typemask);
		tgt->list[i].always_copy_from
		  = GOMP_MAP_ALWAYS_FROM_P (kind & typemask);
		tgt->list[i].is_attach = false;
		tgt->list[i].offset = 0;
		tgt->list[i].length = k->host_end - k->host_start;
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
		    gomp_copy_host2dev (devicep, aq,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					k->host_end - k->host_start,
					false, cbufp);
		    break;
		  case GOMP_MAP_POINTER:
		  case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
		    gomp_map_pointer
		      (tgt, aq, (uintptr_t) *(void **) k->host_start,
		       k->tgt_offset, sizes[i], cbufp,
		       ((kind & typemask)
			== GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION));
		    break;
		  case GOMP_MAP_TO_PSET:
		    gomp_copy_host2dev (devicep, aq,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					k->host_end - k->host_start,
					false, cbufp);
		    tgt->list[i].has_null_ptr_assoc = false;

		    for (j = i + 1; j < mapnum; j++)
		      {
			int ptr_kind = (get_kind (short_mapkind, kinds, j)
					& typemask);
			if (!GOMP_MAP_POINTER_P (ptr_kind)
			    && !GOMP_MAP_ALWAYS_POINTER_P (ptr_kind))
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
			    tgt->list[j].is_attach = false;
			    tgt->list[i].has_null_ptr_assoc |= !(*(void **) hostaddrs[j]);
			    /* For OpenMP, the use of refcount_sets causes
			       errors if we set k->refcount = 1 above but also
			       increment it again here, for decrementing will
			       not properly match, since we decrement only once
			       for each key's refcount. Therefore avoid this
			       increment for OpenMP constructs.  */
			    if (!openmp_p)
			      gomp_increment_refcount (k, refcount_set);
			    gomp_map_pointer (tgt, aq,
					      (uintptr_t) *(void **) hostaddrs[j],
					      k->tgt_offset
					      + ((uintptr_t) hostaddrs[j]
						 - k->host_start),
					      sizes[j], cbufp, false);
			  }
		      }
		    i = j - 1;
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
		    gomp_copy_host2dev (devicep, aq,
					(void *) (tgt->tgt_start
						  + k->tgt_offset),
					(void *) k->host_start,
					sizeof (void *), false, cbufp);
		    break;
		  default:
		    gomp_mutex_unlock (&devicep->lock);
		    gomp_fatal ("%s: unhandled kind 0x%.2x", __FUNCTION__,
				kind);
		  }

		if (k->aux && k->aux->link_key)
		  {
		    /* Set link pointer on target to the device address of the
		       mapped object.  */
		    void *tgt_addr = (void *) (tgt->tgt_start + k->tgt_offset);
		    /* We intentionally do not use coalescing here, as it's not
		       data allocated by the current call to this function.  */
		    gomp_copy_host2dev (devicep, aq, (void *) n->tgt_offset,
					&tgt_addr, sizeof (void *), true, NULL);
		  }
		array++;
	      }
	  }
    }

  if (pragma_kind & GOMP_MAP_VARS_TARGET)
    {
      for (i = 0; i < mapnum; i++)
	{
	  cur_node.tgt_offset = gomp_map_val (tgt, hostaddrs, i);
	  gomp_copy_host2dev (devicep, aq,
			      (void *) (tgt->tgt_start + i * sizeof (void *)),
			      (void *) &cur_node.tgt_offset, sizeof (void *),
			      true, cbufp);
	}
    }

  if (cbufp)
    {
      long c = 0;
      for (c = 0; c < cbuf.chunk_cnt; ++c)
	gomp_copy_host2dev (devicep, aq,
			    (void *) (tgt->tgt_start + cbuf.chunks[c].start),
			    (char *) cbuf.buf + (cbuf.chunks[c].start
						 - cbuf.chunks[0].start),
			    cbuf.chunks[c].end - cbuf.chunks[c].start,
			    false, NULL);
      if (aq)
	/* Free once the transfer has completed.  */
	devicep->openacc.async.queue_callback_func (aq, free, cbuf.buf);
      else
	free (cbuf.buf);
      cbuf.buf = NULL;
      cbufp = NULL;
    }

  /* If the variable from "omp target enter data" map-list was already mapped,
     tgt is not needed.  Otherwise tgt will be freed by gomp_unmap_vars or
     gomp_exit_data.  */
  if ((pragma_kind & GOMP_MAP_VARS_ENTER_DATA) && tgt->refcount == 0)
    {
      free (tgt);
      tgt = NULL;
    }

  gomp_mutex_unlock (&devicep->lock);
  return tgt;
}

static struct target_mem_desc *
gomp_map_vars (struct gomp_device_descr *devicep, size_t mapnum,
	       void **hostaddrs, void **devaddrs, size_t *sizes, void *kinds,
	       bool short_mapkind, htab_t *refcount_set,
	       enum gomp_map_vars_kind pragma_kind)
{
  /* This management of a local refcount_set is for convenience of callers
     who do not share a refcount_set over multiple map/unmap uses.  */
  htab_t local_refcount_set = NULL;
  if (refcount_set == NULL)
    {
      local_refcount_set = htab_create (mapnum);
      refcount_set = &local_refcount_set;
    }

  struct target_mem_desc *tgt;
  tgt = gomp_map_vars_internal (devicep, NULL, mapnum, hostaddrs, devaddrs,
				sizes, kinds, short_mapkind, refcount_set,
				pragma_kind);
  if (local_refcount_set)
    htab_free (local_refcount_set);

  return tgt;
}

attribute_hidden struct target_mem_desc *
goacc_map_vars (struct gomp_device_descr *devicep,
		struct goacc_asyncqueue *aq, size_t mapnum,
		void **hostaddrs, void **devaddrs, size_t *sizes,
		void *kinds, bool short_mapkind,
		enum gomp_map_vars_kind pragma_kind)
{
  return gomp_map_vars_internal (devicep, aq, mapnum, hostaddrs, devaddrs,
				 sizes, kinds, short_mapkind, NULL,
				 GOMP_MAP_VARS_OPENACC | pragma_kind);
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

static bool
gomp_unref_tgt (void *ptr)
{
  bool is_tgt_unmapped = false;

  struct target_mem_desc *tgt = (struct target_mem_desc *) ptr;

  if (tgt->refcount > 1)
    tgt->refcount--;
  else
    {
      gomp_unmap_tgt (tgt);
      is_tgt_unmapped = true;
    }

  return is_tgt_unmapped;
}

static void
gomp_unref_tgt_void (void *ptr)
{
  (void) gomp_unref_tgt (ptr);
}

static void
gomp_remove_splay_tree_key (splay_tree sp, splay_tree_key k)
{
  splay_tree_remove (sp, k);
  if (k->aux)
    {
      if (k->aux->link_key)
	splay_tree_insert (sp, (splay_tree_node) k->aux->link_key);
      if (k->aux->attach_count)
	free (k->aux->attach_count);
      free (k->aux);
      k->aux = NULL;
    }
}

static inline __attribute__((always_inline)) bool
gomp_remove_var_internal (struct gomp_device_descr *devicep, splay_tree_key k,
			  struct goacc_asyncqueue *aq)
{
  bool is_tgt_unmapped = false;

  if (REFCOUNT_STRUCTELEM_P (k->refcount))
    {
      if (REFCOUNT_STRUCTELEM_FIRST_P (k->refcount) == false)
	/* Infer the splay_tree_key of the first structelem key using the
	   pointer to the first structleme_refcount.  */
	k = (splay_tree_key) ((char *) k->structelem_refcount_ptr
			      - offsetof (struct splay_tree_key_s,
					  structelem_refcount));
      assert (REFCOUNT_STRUCTELEM_FIRST_P (k->refcount));

      /* The array created by gomp_map_vars is an array of splay_tree_nodes,
	 with the splay_tree_keys embedded inside.  */
      splay_tree_node node =
	(splay_tree_node) ((char *) k
			   - offsetof (struct splay_tree_node_s, key));
      while (true)
	{
	  /* Starting from the _FIRST key, and continue for all following
	     sibling keys.  */
	  gomp_remove_splay_tree_key (&devicep->mem_map, k);
	  if (REFCOUNT_STRUCTELEM_LAST_P (k->refcount))
	    break;
	  else
	    k = &(++node)->key;
	}
    }
  else
    gomp_remove_splay_tree_key (&devicep->mem_map, k);

  if (aq)
    devicep->openacc.async.queue_callback_func (aq, gomp_unref_tgt_void,
						(void *) k->tgt);
  else
    is_tgt_unmapped = gomp_unref_tgt ((void *) k->tgt);
  return is_tgt_unmapped;
}

attribute_hidden bool
gomp_remove_var (struct gomp_device_descr *devicep, splay_tree_key k)
{
  return gomp_remove_var_internal (devicep, k, NULL);
}

/* Remove a variable asynchronously.  This actually removes the variable
   mapping immediately, but retains the linked target_mem_desc until the
   asynchronous operation has completed (as it may still refer to target
   memory).  The device lock must be held before entry, and remains locked on
   exit.  */

attribute_hidden void
gomp_remove_var_async (struct gomp_device_descr *devicep, splay_tree_key k,
		       struct goacc_asyncqueue *aq)
{
  (void) gomp_remove_var_internal (devicep, k, aq);
}

/* Unmap variables described by TGT.  If DO_COPYFROM is true, copy relevant
   variables back from device to host: if it is false, it is assumed that this
   has been done already.  */

static inline __attribute__((always_inline)) void
gomp_unmap_vars_internal (struct target_mem_desc *tgt, bool do_copyfrom,
			  htab_t *refcount_set, struct goacc_asyncqueue *aq)
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

  /* We must perform detachments before any copies back to the host.  */
  for (i = 0; i < tgt->list_count; i++)
    {
      splay_tree_key k = tgt->list[i].key;

      if (k != NULL && tgt->list[i].is_attach)
	gomp_detach_pointer (devicep, aq, k, tgt->list[i].key->host_start
					     + tgt->list[i].offset,
			     false, NULL);
    }

  for (i = 0; i < tgt->list_count; i++)
    {
      splay_tree_key k = tgt->list[i].key;
      if (k == NULL)
	continue;

      /* OpenACC 'attach'/'detach' doesn't affect structured/dynamic reference
	 counts ('n->refcount', 'n->dynamic_refcount').  */
      if (tgt->list[i].is_attach)
	continue;

      bool do_copy, do_remove;
      gomp_decrement_refcount (k, refcount_set, false, &do_copy, &do_remove);

      if ((do_copy && do_copyfrom && tgt->list[i].copy_from)
	  || tgt->list[i].always_copy_from)
	gomp_copy_dev2host (devicep, aq,
			    (void *) (k->host_start + tgt->list[i].offset),
			    (void *) (k->tgt->tgt_start + k->tgt_offset
				      + tgt->list[i].offset),
			    tgt->list[i].length);
      if (do_remove)
	{
	  struct target_mem_desc *k_tgt = k->tgt;
	  bool is_tgt_unmapped = gomp_remove_var (devicep, k);
	  /* It would be bad if TGT got unmapped while we're still iterating
	     over its LIST_COUNT, and also expect to use it in the following
	     code.  */
	  assert (!is_tgt_unmapped
		  || k_tgt != tgt);
	}
    }

  if (aq)
    devicep->openacc.async.queue_callback_func (aq, gomp_unref_tgt_void,
						(void *) tgt);
  else
    gomp_unref_tgt ((void *) tgt);

  gomp_mutex_unlock (&devicep->lock);
}

static void
gomp_unmap_vars (struct target_mem_desc *tgt, bool do_copyfrom,
		 htab_t *refcount_set)
{
  /* This management of a local refcount_set is for convenience of callers
     who do not share a refcount_set over multiple map/unmap uses.  */
  htab_t local_refcount_set = NULL;
  if (refcount_set == NULL)
    {
      local_refcount_set = htab_create (tgt->list_count);
      refcount_set = &local_refcount_set;
    }

  gomp_unmap_vars_internal (tgt, do_copyfrom, refcount_set, NULL);

  if (local_refcount_set)
    htab_free (local_refcount_set);
}

attribute_hidden void
goacc_unmap_vars (struct target_mem_desc *tgt, bool do_copyfrom,
		  struct goacc_asyncqueue *aq)
{
  gomp_unmap_vars_internal (tgt, do_copyfrom, NULL, aq);
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

	    if (n->aux && n->aux->attach_count)
	      {
		uintptr_t addr = cur_node.host_start;
		while (addr < cur_node.host_end)
		  {
		    /* We have to be careful not to overwrite still attached
		       pointers during host<->device updates.  */
		    size_t i = (addr - cur_node.host_start) / sizeof (void *);
		    if (n->aux->attach_count[i] == 0)
		      {
			void *devaddr = (void *) (n->tgt->tgt_start
						  + n->tgt_offset
						  + addr - n->host_start);
			if (GOMP_MAP_COPY_TO_P (kind & typemask))
			  gomp_copy_host2dev (devicep, NULL,
					      devaddr, (void *) addr,
					      sizeof (void *), false, NULL);
			if (GOMP_MAP_COPY_FROM_P (kind & typemask))
			  gomp_copy_dev2host (devicep, NULL,
					      (void *) addr, devaddr,
					      sizeof (void *));
		      }
		    addr += sizeof (void *);
		  }
	      }
	    else
	      {
		void *hostaddr = (void *) cur_node.host_start;
		void *devaddr = (void *) (n->tgt->tgt_start + n->tgt_offset
					  + cur_node.host_start
					  - n->host_start);
		size_t size = cur_node.host_end - cur_node.host_start;

		if (GOMP_MAP_COPY_TO_P (kind & typemask))
		  gomp_copy_host2dev (devicep, NULL, devaddr, hostaddr, size,
				      false, NULL);
		if (GOMP_MAP_COPY_FROM_P (kind & typemask))
		  gomp_copy_dev2host (devicep, NULL, hostaddr, devaddr, size);
	      }
	  }
      }
  gomp_mutex_unlock (&devicep->lock);
}

static struct gomp_offload_icv_list *
gomp_get_offload_icv_item (int dev_num)
{
  struct gomp_offload_icv_list *l = gomp_offload_icv_list;
  while (l != NULL && l->device_num != dev_num)
    l = l->next;

  return l;
}

/* Helper function for 'gomp_load_image_to_device'.  Returns the ICV values
   depending on the device num and the variable hierarchy
   (_DEV_42, _DEV, _ALL).  If no ICV was initially configured for the given
   device and thus no item with that device number is contained in
   gomp_offload_icv_list, then a new item is created and added to the list.  */

static struct gomp_offload_icvs *
get_gomp_offload_icvs (int dev_num)
{
  struct gomp_icv_list *dev
    = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_DEV);
  struct gomp_icv_list *all
    = gomp_get_initial_icv_item (GOMP_DEVICE_NUM_FOR_ALL);
  struct gomp_icv_list *dev_x = gomp_get_initial_icv_item (dev_num);
  struct gomp_offload_icv_list *offload_icvs
    = gomp_get_offload_icv_item (dev_num);

  if (offload_icvs != NULL)
    return &offload_icvs->icvs;

  struct gomp_offload_icv_list *new
    = (struct gomp_offload_icv_list *) gomp_malloc (sizeof (struct gomp_offload_icv_list));

  new->device_num = dev_num;
  new->icvs.device_num = dev_num;
  new->next = gomp_offload_icv_list;

  if (dev_x != NULL && gomp_get_icv_flag (dev_x->flags, GOMP_ICV_NTEAMS))
    new->icvs.nteams = dev_x->icvs.nteams_var;
  else if (dev != NULL && gomp_get_icv_flag (dev->flags, GOMP_ICV_NTEAMS))
    new->icvs.nteams = dev->icvs.nteams_var;
  else if (all != NULL && gomp_get_icv_flag (all->flags, GOMP_ICV_NTEAMS))
    new->icvs.nteams = all->icvs.nteams_var;
  else
    new->icvs.nteams = gomp_default_icv_values.nteams_var;

  if (dev_x != NULL
      && gomp_get_icv_flag (dev_x->flags, GOMP_ICV_TEAMS_THREAD_LIMIT))
    new->icvs.teams_thread_limit = dev_x->icvs.teams_thread_limit_var;
  else if (dev != NULL
	   && gomp_get_icv_flag (dev->flags, GOMP_ICV_TEAMS_THREAD_LIMIT))
    new->icvs.teams_thread_limit = dev->icvs.teams_thread_limit_var;
  else if (all != NULL
	   && gomp_get_icv_flag (all->flags, GOMP_ICV_TEAMS_THREAD_LIMIT))
    new->icvs.teams_thread_limit = all->icvs.teams_thread_limit_var;
  else
    new->icvs.teams_thread_limit
      = gomp_default_icv_values.teams_thread_limit_var;

  if (dev_x != NULL
      && gomp_get_icv_flag (dev_x->flags, GOMP_ICV_DEFAULT_DEVICE))
    new->icvs.default_device = dev_x->icvs.default_device_var;
  else if (dev != NULL
	   && gomp_get_icv_flag (dev->flags, GOMP_ICV_DEFAULT_DEVICE))
    new->icvs.default_device = dev->icvs.default_device_var;
  else if (all != NULL
	   && gomp_get_icv_flag (all->flags, GOMP_ICV_DEFAULT_DEVICE))
    new->icvs.default_device = all->icvs.default_device_var;
  else
    new->icvs.default_device = gomp_default_icv_values.default_device_var;

  gomp_offload_icv_list = new;
  return &new->icvs;
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
  uint64_t *rev_target_fn_table = NULL;
  int i, num_target_entries;

  /* With reverse offload, insert also target-host addresses. */
  bool rev_lookup = omp_requires_mask & GOMP_REQUIRES_REVERSE_OFFLOAD;

  num_target_entries
    = devicep->load_image_func (devicep->target_id, version,
				target_data, &target_table,
				rev_lookup ? &rev_target_fn_table : NULL);

  if (num_target_entries != num_funcs + num_vars
      /* "+1" due to the additional ICV struct.  */
      && num_target_entries != num_funcs + num_vars + 1)
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
  /* "+1" due to the additional ICV struct.  */
  tgt->array = gomp_malloc ((num_funcs + num_vars + 1)
			    * sizeof (*tgt->array));
  if (rev_target_fn_table)
    tgt->rev_array = gomp_malloc (num_funcs * sizeof (*tgt->rev_array));
  else
    tgt->rev_array = NULL;
  tgt->refcount = REFCOUNT_INFINITY;
  tgt->tgt_start = 0;
  tgt->tgt_end = 0;
  tgt->to_free = NULL;
  tgt->prev = NULL;
  tgt->list_count = 0;
  tgt->device_descr = devicep;
  splay_tree_node array = tgt->array;
  reverse_splay_tree_node rev_array = tgt->rev_array;

  for (i = 0; i < num_funcs; i++)
    {
      splay_tree_key k = &array->key;
      k->host_start = (uintptr_t) host_func_table[i];
      k->host_end = k->host_start + 1;
      k->tgt = tgt;
      k->tgt_offset = target_table[i].start;
      k->refcount = REFCOUNT_INFINITY;
      k->dynamic_refcount = 0;
      k->aux = NULL;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      if (rev_target_fn_table)
	{
	  reverse_splay_tree_key k2 = &rev_array->key;
	  k2->dev = rev_target_fn_table[i];
	  k2->k = k;
	  rev_array->left = NULL;
	  rev_array->right = NULL;
	  if (k2->dev != 0)
	    reverse_splay_tree_insert (&devicep->mem_map_rev, rev_array);
	  rev_array++;
	}
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
      bool is_link_var = link_bit & (uintptr_t) host_var_table[i * 2 + 1];

      if (!is_link_var && (uintptr_t) host_var_table[i * 2 + 1] != target_size)
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
      k->refcount = is_link_var ? REFCOUNT_LINK : REFCOUNT_INFINITY;
      k->dynamic_refcount = 0;
      k->aux = NULL;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      array++;
    }

  /* Last entry is for a ICVs variable.
     Tolerate case where plugin does not return those entries.  */
  if (num_funcs + num_vars < num_target_entries)
    {
      struct addr_pair *var = &target_table[num_funcs + num_vars];

      /* Start address will be non-zero for the ICVs variable if
	 the variable was found in this image.  */
      if (var->start != 0)
	{
	  /* The index of the devicep within devices[] is regarded as its
	     'device number', which is different from the per-device type
	     devicep->target_id.  */
	  int dev_num = (int) (devicep - &devices[0]);
	  struct gomp_offload_icvs *icvs = get_gomp_offload_icvs (dev_num);
	  size_t var_size = var->end - var->start;
	  if (var_size != sizeof (struct gomp_offload_icvs))
	    {
	      gomp_mutex_unlock (&devicep->lock);
	      if (is_register_lock)
		gomp_mutex_unlock (&register_lock);
	      gomp_fatal ("offload plugin managed 'icv struct' not of expected "
			  "format");
	    }
	  /* Copy the ICVs variable to place on device memory, hereby
	     actually designating its device number into effect.  */
	  gomp_copy_host2dev (devicep, NULL, (void *) var->start, icvs,
			      var_size, false, NULL);
	  splay_tree_key k = &array->key;
	  k->host_start = (uintptr_t) icvs;
	  k->host_end =
	    k->host_start + (size_mask & sizeof (struct gomp_offload_icvs));
	  k->tgt = tgt;
	  k->tgt_offset = var->start;
	  k->refcount = REFCOUNT_INFINITY;
	  k->dynamic_refcount = 0;
	  k->aux = NULL;
	  array->left = NULL;
	  array->right = NULL;
	  splay_tree_insert (&devicep->mem_map, array);
	  array++;
	}
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
  if (devicep->mem_map_rev.root)
    {
      /* Free reverse offload splay tree + data; 'tgt->rev_array' is the only
	 real allocation.  */
      assert (node && node->tgt && node->tgt->rev_array);
      assert (devicep->mem_map_rev.root->key.k->tgt == node->tgt);
      free (node->tgt->rev_array);
      devicep->mem_map_rev.root = NULL;
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
	  is_tgt_unmapped = gomp_remove_var (devicep, n);
	}
    }

  if (node && !is_tgt_unmapped)
    {
      free (node->tgt);
      free (node);
    }
}

static void
gomp_requires_to_name (char *buf, size_t size, int requires_mask)
{
  char *end = buf + size, *p = buf;
  if (requires_mask & GOMP_REQUIRES_UNIFIED_ADDRESS)
    p += snprintf (p, end - p, "unified_address");
  if (requires_mask & GOMP_REQUIRES_UNIFIED_SHARED_MEMORY)
    p += snprintf (p, end - p, "%sunified_shared_memory",
		   (p == buf ? "" : ", "));
  if (requires_mask & GOMP_REQUIRES_REVERSE_OFFLOAD)
    p += snprintf (p, end - p, "%sreverse_offload",
		   (p == buf ? "" : ", "));
}

/* This function should be called from every offload image while loading.
   It gets the descriptor of the host func and var tables HOST_TABLE, TYPE of
   the target, and DATA.  */

void
GOMP_offload_register_ver (unsigned version, const void *host_table,
			   int target_type, const void *data)
{
  int i;

  if (GOMP_VERSION_LIB (version) > GOMP_VERSION)
    gomp_fatal ("Library too old for offload (version %u < %u)",
		GOMP_VERSION, GOMP_VERSION_LIB (version));

  int omp_req;
  const void *target_data;
  if (GOMP_VERSION_LIB (version) > 1)
    {
      omp_req = (int) (size_t) ((void **) data)[0];
      target_data = &((void **) data)[1];
    }
  else
    {
      omp_req = 0;
      target_data = data;
    }

  gomp_mutex_lock (&register_lock);

  if (omp_req && omp_requires_mask && omp_requires_mask != omp_req)
    {
      char buf1[sizeof ("unified_address, unified_shared_memory, "
			"reverse_offload")];
      char buf2[sizeof ("unified_address, unified_shared_memory, "
			"reverse_offload")];
      gomp_requires_to_name (buf2, sizeof (buf2),
			     omp_req != GOMP_REQUIRES_TARGET_USED
			     ? omp_req : omp_requires_mask);
      if (omp_req != GOMP_REQUIRES_TARGET_USED
	  && omp_requires_mask != GOMP_REQUIRES_TARGET_USED)
	{
	  gomp_requires_to_name (buf1, sizeof (buf1), omp_requires_mask);
	  gomp_fatal ("OpenMP 'requires' directive with non-identical clauses "
		      "in multiple compilation units: '%s' vs. '%s'",
		      buf1, buf2);
	}
      else
	gomp_fatal ("OpenMP 'requires' directive with '%s' specified only in "
		    "some compilation units", buf2);
    }
  omp_requires_mask = omp_req;

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

/* Legacy entry point.  */

void
GOMP_offload_register (const void *host_table, int target_type,
		       const void *target_data)
{
  GOMP_offload_register_ver (0, host_table, target_type, target_data);
}

/* This function should be called from every offload image while unloading.
   It gets the descriptor of the host func and var tables HOST_TABLE, TYPE of
   the target, and DATA.  */

void
GOMP_offload_unregister_ver (unsigned version, const void *host_table,
			     int target_type, const void *data)
{
  int i;

  if (GOMP_VERSION_LIB (version) > GOMP_VERSION)
    gomp_fatal ("Library too old for offload (version %u < %u)",
		GOMP_VERSION, GOMP_VERSION_LIB (version));

  const void *target_data;
  if (GOMP_VERSION_LIB (version) > 1)
    target_data = &((void **) data)[1];
  else
    target_data = data;

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

/* Legacy entry point.  */

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

  /* Initialize OpenACC asynchronous queues.  */
  goacc_init_asyncqueues (devicep);

  devicep->state = GOMP_DEVICE_INITIALIZED;
}

/* This function finalizes the target device, specified by DEVICEP.  DEVICEP
   must be locked on entry, and remains locked on return.  */

attribute_hidden bool
gomp_fini_device (struct gomp_device_descr *devicep)
{
  bool ret = goacc_fini_asyncqueues (devicep);
  ret &= devicep->fini_device_func (devicep->target_id);
  devicep->state = GOMP_DEVICE_FINALIZED;
  return ret;
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

/* Host fallback for GOMP_target{,_ext} routines.  */

static void
gomp_target_fallback (void (*fn) (void *), void **hostaddrs,
		      struct gomp_device_descr *devicep, void **args)
{
  struct gomp_thread old_thr, *thr = gomp_thread ();

  if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_MANDATORY
      && devicep != NULL)
    gomp_fatal ("OMP_TARGET_OFFLOAD is set to MANDATORY, but device cannot "
		"be used for offloading");

  old_thr = *thr;
  memset (thr, '\0', sizeof (*thr));
  if (gomp_places_list)
    {
      thr->place = old_thr.place;
      thr->ts.place_partition_len = gomp_places_list_len;
    }
  if (args)
    while (*args)
      {
	intptr_t id = (intptr_t) *args++, val;
	if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
	  val = (intptr_t) *args++;
	else
	  val = id >> GOMP_TARGET_ARG_VALUE_SHIFT;
	if ((id & GOMP_TARGET_ARG_DEVICE_MASK) != GOMP_TARGET_ARG_DEVICE_ALL)
	  continue;
	id &= GOMP_TARGET_ARG_ID_MASK;
	if (id != GOMP_TARGET_ARG_THREAD_LIMIT)
	  continue;
	val = val > INT_MAX ? INT_MAX : val;
	if (val)
	  gomp_icv (true)->thread_limit_var = val;
	break;
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
    if ((kinds[i] & 0xff) == GOMP_MAP_FIRSTPRIVATE && hostaddrs[i] != NULL)
      {
	size_t align = (size_t) 1 << (kinds[i] >> 8);
	tgt_size = (tgt_size + align - 1) & ~(align - 1);
	memcpy (tgt + tgt_size, hostaddrs[i], sizes[i]);
	hostaddrs[i] = tgt + tgt_size;
	tgt_size = tgt_size + sizes[i];
	if (i + 1 < mapnum && (kinds[i+1] & 0xff) == GOMP_MAP_ATTACH)
	  {
	    *(*(uintptr_t**) hostaddrs[i+1] + sizes[i+1]) = (uintptr_t) hostaddrs[i];
	    ++i;
	  }
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
  struct gomp_device_descr *devicep = resolve_device (device, true);

  void *fn_addr;
  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      /* All shared memory devices should use the GOMP_target_ext function.  */
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM
      || !(fn_addr = gomp_get_target_fn_addr (devicep, fn)))
    return gomp_target_fallback (fn, hostaddrs, devicep, NULL);

  htab_t refcount_set = htab_create (mapnum);
  struct target_mem_desc *tgt_vars
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, false,
		     &refcount_set, GOMP_MAP_VARS_TARGET);
  devicep->run_func (devicep->target_id, fn_addr, (void *) tgt_vars->tgt_start,
		     NULL);
  htab_clear (refcount_set);
  gomp_unmap_vars (tgt_vars, true, &refcount_set);
  htab_free (refcount_set);
}

static inline unsigned int
clear_unsupported_flags (struct gomp_device_descr *devicep, unsigned int flags)
{
  /* If we cannot run asynchronously, simply ignore nowait.  */
  if (devicep != NULL && devicep->async_run_func == NULL)
    flags &= ~GOMP_TARGET_FLAG_NOWAIT;

  return flags;
}

static void
gomp_copy_back_icvs (struct gomp_device_descr *devicep, int device)
{
  struct gomp_offload_icv_list *item = gomp_get_offload_icv_item (device);
  if (item == NULL)
    return;

  void *host_ptr = &item->icvs;
  void *dev_ptr = omp_get_mapped_ptr (host_ptr, device);
  if (dev_ptr != NULL)
    gomp_copy_dev2host (devicep, NULL, host_ptr, dev_ptr,
			sizeof (struct gomp_offload_icvs));
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
  struct gomp_device_descr *devicep = resolve_device (device, true);
  size_t tgt_align = 0, tgt_size = 0;
  bool fpc_done = false;

  /* Obtain the original TEAMS and THREADS values from ARGS.  */
  intptr_t orig_teams = 1, orig_threads = 0;
  size_t num_args = 0, len = 1, teams_len = 1, threads_len = 1;
  void **tmpargs = args;
  while (*tmpargs)
    {
      intptr_t id = (intptr_t) *tmpargs++, val;
      if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
	{
	  val = (intptr_t) *tmpargs++;
	  len = 2;
	}
      else
	{
	  val = id >> GOMP_TARGET_ARG_VALUE_SHIFT;
	  len = 1;
	}
      num_args += len;
      if ((id & GOMP_TARGET_ARG_DEVICE_MASK) != GOMP_TARGET_ARG_DEVICE_ALL)
	continue;
      val = val > INT_MAX ? INT_MAX : val;
      if ((id & GOMP_TARGET_ARG_ID_MASK) == GOMP_TARGET_ARG_NUM_TEAMS)
	{
	  orig_teams = val;
	  teams_len = len;
	}
      else if ((id & GOMP_TARGET_ARG_ID_MASK) == GOMP_TARGET_ARG_THREAD_LIMIT)
	{
	  orig_threads = val;
	  threads_len = len;
	}
    }

  intptr_t new_teams = orig_teams, new_threads = orig_threads;
  /* ORIG_TEAMS == -2: No explicit teams construct specified.  Set to 1.
     ORIG_TEAMS == -1: TEAMS construct with NUM_TEAMS clause specified, but the
		       value could not be determined.  No change.
     ORIG_TEAMS == 0: TEAMS construct without NUM_TEAMS clause.
		      Set device-specific value.
     ORIG_TEAMS > 0: Value was already set through e.g. NUM_TEAMS clause.
		     No change.  */
  if (orig_teams == -2)
    new_teams = 1;
  else if (orig_teams == 0)
    {
      struct gomp_offload_icv_list *item = gomp_get_offload_icv_item (device);
      if (item != NULL)
	new_teams = item->icvs.nteams;
    }
  /* The device-specific teams-thread-limit is only set if (a) an explicit TEAMS
     region exists, i.e. ORIG_TEAMS > -2, and (b) THREADS was not already set by
     e.g. a THREAD_LIMIT clause.  */
  if (orig_teams > -2 && orig_threads == 0)
    {
      struct gomp_offload_icv_list *item = gomp_get_offload_icv_item (device);
      if (item != NULL)
	new_threads = item->icvs.teams_thread_limit;
    }

  /* Copy and change the arguments list only if TEAMS or THREADS need to be
     updated.  */
  void **new_args = args;
  if (orig_teams != new_teams || orig_threads != new_threads)
    {
      size_t tms_len = (orig_teams == new_teams
			? teams_len
			: (new_teams > -(1 << 15) && new_teams < (1 << 15)
			   ? 1 : 2));
      size_t ths_len = (orig_threads == new_threads
			? threads_len
			: (new_threads > -(1 << 15) && new_threads < (1 << 15)
			   ? 1 : 2));
      /* One additional item after the last arg must be NULL.  */
      size_t new_args_cnt = num_args - teams_len - threads_len + tms_len
			    + ths_len + 1;
      new_args = (void **) gomp_alloca (new_args_cnt * sizeof (void*));

      tmpargs = args;
      void **tmp_new_args = new_args;
      /* Copy all args except TEAMS and THREADS.  TEAMS and THREADS are copied
	 too if they have not been changed and skipped otherwise.  */
      while (*tmpargs)
	{
	  intptr_t id = (intptr_t) *tmpargs;
	  if (((id & GOMP_TARGET_ARG_ID_MASK) == GOMP_TARGET_ARG_NUM_TEAMS
	       && orig_teams != new_teams)
	      || ((id & GOMP_TARGET_ARG_ID_MASK) == GOMP_TARGET_ARG_THREAD_LIMIT
		  && orig_threads != new_threads))
	    {
	      tmpargs++;
	      if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
		tmpargs++;
	    }
	  else
	    {
	      *tmp_new_args++ = *tmpargs++;
	      if (id & GOMP_TARGET_ARG_SUBSEQUENT_PARAM)
		*tmp_new_args++ = *tmpargs++;
	    }
	}

      /* Add the new TEAMS arg to the new args list if it has been changed.  */
      if (orig_teams != new_teams)
	{
	  intptr_t new_val = new_teams;
	  if (tms_len == 1)
	    {
	      new_val = (new_val << GOMP_TARGET_ARG_VALUE_SHIFT)
			 | GOMP_TARGET_ARG_NUM_TEAMS;
	      *tmp_new_args++ = (void *) new_val;
	    }
	  else
	    {
	      *tmp_new_args++ = (void *) (GOMP_TARGET_ARG_SUBSEQUENT_PARAM
					  | GOMP_TARGET_ARG_NUM_TEAMS);
	      *tmp_new_args++ = (void *) new_val;
	    }
	}

      /* Add the new THREADS arg to the new args list if it has been changed. */
      if (orig_threads != new_threads)
	{
	  intptr_t new_val = new_threads;
	  if (ths_len == 1)
	    {
	      new_val = (new_val << GOMP_TARGET_ARG_VALUE_SHIFT)
			 | GOMP_TARGET_ARG_THREAD_LIMIT;
	      *tmp_new_args++ = (void *) new_val;
	    }
	  else
	    {
	      *tmp_new_args++ = (void *) (GOMP_TARGET_ARG_SUBSEQUENT_PARAM
					  | GOMP_TARGET_ARG_THREAD_LIMIT);
	      *tmp_new_args++ = (void *) new_val;
	    }
	}

      *tmp_new_args = NULL;
    }

  flags = clear_unsupported_flags (devicep, flags);

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
	  struct gomp_task **implicit_task = &task;
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
	  while (*implicit_task
		 && (*implicit_task)->kind != GOMP_TASK_IMPLICIT)
	    implicit_task = &(*implicit_task)->parent;
	  if (*implicit_task)
	    {
	      thr->task = *implicit_task;
	      gomp_end_task ();
	      free (*implicit_task);
	      thr->task = &team->implicit_task[0];
	    }
	  else
	    pthread_setspecific (gomp_thread_destructor, thr);
	  if (implicit_task != &task)
	    {
	      *implicit_task = thr->task;
	      thr->task = task;
	    }
	}
      if (thr->ts.team
	  && !thr->task->final_task)
	{
	  gomp_create_target_task (devicep, fn, mapnum, hostaddrs,
				   sizes, kinds, flags, depend, new_args,
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
      gomp_target_fallback (fn, hostaddrs, devicep, new_args);
      return;
    }

  struct target_mem_desc *tgt_vars;
  htab_t refcount_set = NULL;

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
    {
      refcount_set = htab_create (mapnum);
      tgt_vars = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds,
				true, &refcount_set, GOMP_MAP_VARS_TARGET);
    }
  devicep->run_func (devicep->target_id, fn_addr,
		     tgt_vars ? (void *) tgt_vars->tgt_start : hostaddrs,
		     new_args);
  if (tgt_vars)
    {
      htab_clear (refcount_set);
      gomp_unmap_vars (tgt_vars, true, &refcount_set);
    }
  if (refcount_set)
    htab_free (refcount_set);

  /* Copy back ICVs from device to host.
     HOST_PTR is expected to exist since it was added in
     gomp_load_image_to_device if not already available.  */
  gomp_copy_back_icvs (devicep, device);

}


/* Reverse lookup (device addr -> host addr) for reverse offload.  We avoid
   keeping track of all variable handling - assuming that reverse offload occurs
   ony very rarely.  Downside is that the reverse search is slow.  */

struct gomp_splay_tree_rev_lookup_data {
  uintptr_t tgt_start;
  uintptr_t tgt_end;
  splay_tree_key key;
};

static int
gomp_splay_tree_rev_lookup (splay_tree_key key, void *d)
{
  struct gomp_splay_tree_rev_lookup_data *data;
  data = (struct gomp_splay_tree_rev_lookup_data *)d;
  uintptr_t tgt_start = key->tgt->tgt_start + key->tgt_offset;

  if (tgt_start > data->tgt_start || key->tgt->list_count == 0)
    return 0;

  size_t j;
  for (j = 0; j < key->tgt->list_count; j++)
    if (key->tgt->list[j].key == key)
      break;
  assert (j < key->tgt->list_count);
  uintptr_t tgt_end = tgt_start + key->tgt->list[j].length;

  if ((tgt_start == data->tgt_start && tgt_end == data->tgt_end)
      || (tgt_end > data->tgt_start && tgt_start < data->tgt_end))
    {
      data->key = key;
      return 1;
    }
  return 0;
}

static inline splay_tree_key
gomp_map_rev_lookup (splay_tree mem_map, uint64_t tgt_start, uint64_t tgt_end,
		     bool zero_len)
{
  struct gomp_splay_tree_rev_lookup_data data;
  data.key = NULL;
  data.tgt_start = tgt_start;
  data.tgt_end = tgt_end;

  if (tgt_start != tgt_end)
    {
      splay_tree_foreach_lazy (mem_map, gomp_splay_tree_rev_lookup, &data);
      return data.key;
    }

  data.tgt_end++;
  splay_tree_foreach_lazy (mem_map, gomp_splay_tree_rev_lookup, &data);
  if (data.key != NULL || zero_len)
    return data.key;
  data.tgt_end--;

  data.tgt_start--;
  splay_tree_foreach_lazy (mem_map, gomp_splay_tree_rev_lookup, &data);
  return data.key;
}

struct cpy_data
{
  uint64_t devaddr;
  bool present, aligned;
};


/* Search just mapped reverse-offload data; returns index if found,
   otherwise >= n.  */

static inline int
gomp_map_cdata_lookup_int (struct cpy_data *d, uint64_t *devaddrs,
			   unsigned short *kinds, uint64_t *sizes, size_t n,
			   uint64_t tgt_start, uint64_t tgt_end)
{
  const bool short_mapkind = true;
  const int typemask = short_mapkind ? 0xff : 0x7;
  size_t i;
  for (i = 0; i < n; i++)
    {
      bool is_struct = ((get_kind (short_mapkind, kinds, i) & typemask)
			== GOMP_MAP_STRUCT);
      uint64_t dev_end;
      if (!is_struct)
	dev_end = d[i].devaddr + sizes[i];
      else
	{
	  if (i + sizes[i] < n)
	    dev_end = d[i + sizes[i]].devaddr + sizes[i + sizes[i]];
	  else
	    dev_end = devaddrs[i + sizes[i]] + sizes[i + sizes[i]];
	}
      if ((d[i].devaddr == tgt_start && dev_end == tgt_end)
	  || (dev_end > tgt_start && d[i].devaddr < tgt_end))
	break;
      if (is_struct)
	i += sizes[i];
    }
  return i;
}

static inline int
gomp_map_cdata_lookup (struct cpy_data *d, uint64_t *devaddrs,
		       unsigned short *kinds, uint64_t *sizes,
		       size_t n, uint64_t tgt_start, uint64_t tgt_end,
		       bool zero_len)
{
  size_t i;
  if (tgt_start != tgt_end)
    return gomp_map_cdata_lookup_int (d, devaddrs, kinds, sizes, n,
				      tgt_start, tgt_end);
  tgt_end++;
  i = gomp_map_cdata_lookup_int (d, devaddrs, kinds, sizes, n,
				 tgt_start, tgt_end);
  if (i < n || zero_len)
    return i;
  tgt_end--;

  tgt_start--;
  return gomp_map_cdata_lookup_int (d, devaddrs, kinds, sizes, n,
				    tgt_start, tgt_end);
}

/* Handle reverse offload.  This is called by the device plugins for a
   reverse offload; it is not called if the outer target runs on the host.
   The mapping is simplified device-affecting constructs (except for target
   with device(ancestor:1)) must not be encountered; in particular not
   target (enter/exit) data.  */

void
gomp_target_rev (uint64_t fn_ptr, uint64_t mapnum, uint64_t devaddrs_ptr,
		 uint64_t sizes_ptr, uint64_t kinds_ptr, int dev_num,
		 void (*dev_to_host_cpy) (void *, const void *, size_t, void*),
		 void (*host_to_dev_cpy) (void *, const void *, size_t, void*),
		 void *token)
{
  /* Return early if there is no offload code.  */
  if (sizeof (OFFLOAD_PLUGINS) == sizeof (""))
    return;
  /* Currently, this fails because of calculate_firstprivate_requirements
     below; it could be fixed but additional code needs to be updated to
     handle 32bit hosts - thus, it is not worthwhile.  */
  if (sizeof (void *) != sizeof (uint64_t))
    gomp_fatal ("Reverse offload of 32bit hosts not supported.");

  struct cpy_data *cdata = NULL;
  uint64_t *devaddrs;
  uint64_t *sizes;
  unsigned short *kinds;
  const bool short_mapkind = true;
  const int typemask = short_mapkind ? 0xff : 0x7;
  struct gomp_device_descr *devicep = resolve_device (dev_num, false);

  reverse_splay_tree_key n;
  struct reverse_splay_tree_key_s k;
  k.dev = fn_ptr;

  gomp_mutex_lock (&devicep->lock);
  n = gomp_map_lookup_rev (&devicep->mem_map_rev, &k);
  gomp_mutex_unlock (&devicep->lock);

  if (n == NULL)
    gomp_fatal ("Cannot find reverse-offload function");
  void (*host_fn)() = (void (*)()) n->k->host_start;

  if ((devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM) || mapnum == 0)
    {
      devaddrs = (uint64_t *) (uintptr_t) devaddrs_ptr;
      sizes = (uint64_t *) (uintptr_t) sizes_ptr;
      kinds = (unsigned short *) (uintptr_t) kinds_ptr;
    }
  else
    {
      devaddrs = (uint64_t *) gomp_malloc (mapnum * sizeof (uint64_t));
      sizes = (uint64_t *) gomp_malloc (mapnum * sizeof (uint64_t));
      kinds = (unsigned short *) gomp_malloc (mapnum * sizeof (unsigned short));
      if (dev_to_host_cpy)
	{
	  dev_to_host_cpy (devaddrs, (const void *) (uintptr_t) devaddrs_ptr,
			   mapnum * sizeof (uint64_t), token);
	  dev_to_host_cpy (sizes, (const void *) (uintptr_t) sizes_ptr,
			   mapnum * sizeof (uint64_t), token);
	  dev_to_host_cpy (kinds, (const void *) (uintptr_t) kinds_ptr,
			   mapnum * sizeof (unsigned short), token);
	}
      else
	{
	  gomp_copy_dev2host (devicep, NULL, devaddrs,
			      (const void *) (uintptr_t) devaddrs_ptr,
			      mapnum * sizeof (uint64_t));
	  gomp_copy_dev2host (devicep, NULL, sizes,
			      (const void *) (uintptr_t) sizes_ptr,
			      mapnum * sizeof (uint64_t));
	  gomp_copy_dev2host (devicep, NULL, kinds, (const void *) (uintptr_t) kinds_ptr,
			      mapnum * sizeof (unsigned short));
	}
    }

  size_t tgt_align = 0, tgt_size = 0;

  /* If actually executed on 32bit systems, the casts lead to wrong code;
     but 32bit with offloading is not supported; see top of this function.  */
  calculate_firstprivate_requirements (mapnum, (void *) (uintptr_t) sizes,
				       (void *) (uintptr_t) kinds,
				       &tgt_align, &tgt_size);

  if (tgt_align)
    {
      char *tgt = gomp_alloca (tgt_size + tgt_align - 1);
      uintptr_t al = (uintptr_t) tgt & (tgt_align - 1);
      if (al)
	tgt += tgt_align - al;
      tgt_size = 0;
      for (uint64_t i = 0; i < mapnum; i++)
	if (get_kind (short_mapkind, kinds, i) == GOMP_MAP_FIRSTPRIVATE
	    && devaddrs[i] != 0)
	  {
	    size_t align = (size_t) 1 << (kinds[i] >> 8);
	    tgt_size = (tgt_size + align - 1) & ~(align - 1);
	    if (devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	      memcpy (tgt + tgt_size, (void *) (uintptr_t) devaddrs[i],
		      (size_t) sizes[i]);
	    else if (dev_to_host_cpy)
	      dev_to_host_cpy (tgt + tgt_size, (void *) (uintptr_t) devaddrs[i],
			       (size_t) sizes[i], token);
	    else
	      gomp_copy_dev2host (devicep, NULL, tgt + tgt_size,
				  (void *) (uintptr_t) devaddrs[i],
				  (size_t) sizes[i]);
	    devaddrs[i] = (uint64_t) (uintptr_t) tgt + tgt_size;
	    tgt_size = tgt_size + sizes[i];
	    if ((devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
		&& i + 1 < mapnum
		&& ((get_kind (short_mapkind, kinds, i) & typemask)
		    == GOMP_MAP_ATTACH))
	      {
		*(uint64_t*) (uintptr_t) (devaddrs[i+1] + sizes[i+1])
		  = (uint64_t) devaddrs[i];
		++i;
	      }
	  }
    }

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM) && mapnum > 0)
    {
      size_t j, struct_cpy = 0;
      splay_tree_key n2;
      cdata = gomp_alloca (sizeof (*cdata) * mapnum);
      memset (cdata, '\0', sizeof (*cdata) * mapnum);
      gomp_mutex_lock (&devicep->lock);
      for (uint64_t i = 0; i < mapnum; i++)
	{
	  if (devaddrs[i] == 0)
	    continue;
	  n = NULL;
	  int kind = get_kind (short_mapkind, kinds, i) & typemask;
	  switch (kind)
	    {
	      case GOMP_MAP_FIRSTPRIVATE:
	      case GOMP_MAP_FIRSTPRIVATE_INT:
		continue;

	      case GOMP_MAP_DELETE:
	      case GOMP_MAP_RELEASE:
	      case GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION:
		/* Assume it is present; look it up - but ignore otherwise. */
	      case GOMP_MAP_ALLOC:
	      case GOMP_MAP_FROM:
	      case GOMP_MAP_FORCE_ALLOC:
	      case GOMP_MAP_FORCE_FROM:
	      case GOMP_MAP_ALWAYS_FROM:
	      case GOMP_MAP_TO:
	      case GOMP_MAP_TOFROM:
	      case GOMP_MAP_FORCE_TO:
	      case GOMP_MAP_FORCE_TOFROM:
	      case GOMP_MAP_ALWAYS_TO:
	      case GOMP_MAP_ALWAYS_TOFROM:
	      case GOMP_MAP_ZERO_LEN_ARRAY_SECTION:
		cdata[i].devaddr = devaddrs[i];
		bool zero_len = (kind == GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION
				 || kind == GOMP_MAP_ZERO_LEN_ARRAY_SECTION);
		j = gomp_map_cdata_lookup (cdata, devaddrs, kinds, sizes, i,
					   devaddrs[i],
					   devaddrs[i] + sizes[i], zero_len);
		if (j < i)
		  {
		    n2 = NULL;
		    cdata[i].present = true;
		    devaddrs[i] = devaddrs[j] + devaddrs[i] - cdata[j].devaddr;
		  }
		else
		  {
		    n2 = gomp_map_rev_lookup (&devicep->mem_map,
					      devaddrs[i],
					      devaddrs[i] + sizes[i], zero_len);
		    cdata[i].present = n2 != NULL;
		  }
		if (!cdata[i].present
		    && kind != GOMP_MAP_DELETE
		    && kind != GOMP_MAP_RELEASE
		    && kind != GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION)
		  {
		    cdata[i].aligned = true;
		    size_t align = (size_t) 1 << (kinds[i] >> 8);
		    devaddrs[i]
		      = (uint64_t) (uintptr_t) gomp_aligned_alloc (align,
								   sizes[i]);
		  }
		else if (n2 != NULL)
		  devaddrs[i] = (n2->host_start + cdata[i].devaddr
				 - (n2->tgt->tgt_start + n2->tgt_offset));
		if (((!cdata[i].present || struct_cpy)
		     && (kind == GOMP_MAP_TO || kind == GOMP_MAP_TOFROM))
		    || kind == GOMP_MAP_FORCE_TO
		    || kind == GOMP_MAP_FORCE_TOFROM
		    || kind == GOMP_MAP_ALWAYS_TO
		    || kind == GOMP_MAP_ALWAYS_TOFROM)
		  {
		    if (dev_to_host_cpy)
		      dev_to_host_cpy ((void *) (uintptr_t) devaddrs[i],
				       (void *) (uintptr_t) cdata[i].devaddr,
				       sizes[i], token);
		    else
		      gomp_copy_dev2host (devicep, NULL,
					  (void *) (uintptr_t) devaddrs[i],
					  (void *) (uintptr_t) cdata[i].devaddr,
					  sizes[i]);
		  }
		if (struct_cpy)
		  struct_cpy--;
		break;
	      case GOMP_MAP_ATTACH:
	      case GOMP_MAP_POINTER:
	      case GOMP_MAP_ALWAYS_POINTER:
		n2 = gomp_map_rev_lookup (&devicep->mem_map,
					  devaddrs[i] + sizes[i],
					  devaddrs[i] + sizes[i]
					  + sizeof (void*), false);
		cdata[i].present = n2 != NULL;
		cdata[i].devaddr = devaddrs[i];
		if (n2)
		  devaddrs[i] = (n2->host_start + cdata[i].devaddr
				 - (n2->tgt->tgt_start + n2->tgt_offset));
		else
		  {
		    j = gomp_map_cdata_lookup (cdata, devaddrs, kinds, sizes, i,
					       devaddrs[i] + sizes[i],
					       devaddrs[i] + sizes[i]
					       + sizeof (void*), false);
		    if (j < i)
		      {
			cdata[i].present = true;
			devaddrs[i] = (devaddrs[j] + devaddrs[i]
				       - cdata[j].devaddr);
		      }
		  }
		if (!cdata[i].present)
		  devaddrs[i] = (uintptr_t) gomp_malloc (sizeof (void*));
		/* Assume that when present, the pointer is already correct.  */
		if (!n2)
		  *(uint64_t *) (uintptr_t) (devaddrs[i] + sizes[i])
		    = devaddrs[i-1];
		break;
	      case GOMP_MAP_TO_PSET:
		/* Assume that when present, the pointers are fine and no 'to:'
		   is required.  */
		n2 = gomp_map_rev_lookup (&devicep->mem_map,
					  devaddrs[i], devaddrs[i] + sizes[i],
					  false);
		cdata[i].present = n2 != NULL;
		cdata[i].devaddr = devaddrs[i];
		if (n2)
		  devaddrs[i] = (n2->host_start + cdata[i].devaddr
				 - (n2->tgt->tgt_start + n2->tgt_offset));
		else
		  {
		    j = gomp_map_cdata_lookup (cdata, devaddrs, kinds, sizes, i,
					       devaddrs[i],
					       devaddrs[i] + sizes[i], false);
		    if (j < i)
		      {
			cdata[i].present = true;
			devaddrs[i] = (devaddrs[j] + devaddrs[i]
				       - cdata[j].devaddr);
		      }
		  }
		if (!cdata[i].present)
		  {
		    cdata[i].aligned = true;
		    size_t align = (size_t) 1 << (kinds[i] >> 8);
		    devaddrs[i]
		      = (uint64_t) (uintptr_t) gomp_aligned_alloc (align,
								   sizes[i]);
		    if (dev_to_host_cpy)
		      dev_to_host_cpy ((void *) (uintptr_t) devaddrs[i],
				       (void *) (uintptr_t) cdata[i].devaddr,
				       sizes[i], token);
		    else
		      gomp_copy_dev2host (devicep, NULL,
					  (void *) (uintptr_t) devaddrs[i],
					  (void *) (uintptr_t) cdata[i].devaddr,
					  sizes[i]);
		  }
		for (j = i + 1; j < mapnum; j++)
		  {
		    kind = get_kind (short_mapkind, kinds, j) & typemask;
		    if (!GOMP_MAP_ALWAYS_POINTER_P (kind)
			&& !GOMP_MAP_POINTER_P (kind))
		      break;
		    if (devaddrs[j] < devaddrs[i])
		      break;
		    if (cdata[i].present)
		      continue;
		    if (devaddrs[j] == 0)
		      {
			*(uint64_t *) (uintptr_t) (devaddrs[i] + sizes[j]) = 0;
			continue;
		      }
		    int k;
		    n2 = NULL;
		    /* Dereference devaddrs[j] to get the device addr.  */
		    assert (devaddrs[j] - sizes[j] == cdata[i].devaddr);
		    devaddrs[j] = *(uint64_t *) (uintptr_t) (devaddrs[i]
							     + sizes[j]);
		    cdata[j].present = true;
		    cdata[j].devaddr = devaddrs[j];
		    if (devaddrs[j] == 0)
		      continue;
		    k = gomp_map_cdata_lookup (cdata, devaddrs, kinds, sizes, j,
					       devaddrs[j],
					       devaddrs[j] + sizeof (void*),
					       false);
		    if (k < j)
		      devaddrs[j] = (devaddrs[k] + devaddrs[j]
				     - cdata[k].devaddr);
		    else
		      {
			n2 = gomp_map_rev_lookup (&devicep->mem_map,
						  devaddrs[j],
						  devaddrs[j] + sizeof (void*),
						  false);
			if (n2 == NULL)
			  {
			    gomp_mutex_unlock (&devicep->lock);
			    gomp_fatal ("Pointer target wasn't mapped");
			  }
			devaddrs[j] = (n2->host_start + cdata[j].devaddr
				       - (n2->tgt->tgt_start + n2->tgt_offset));
		      }
		    *(void **) (uintptr_t) (devaddrs[i] + sizes[j])
		      = (void *) (uintptr_t) devaddrs[j];
		  }
		i = j -1;
		break;
	      case GOMP_MAP_STRUCT:
		n2 = gomp_map_rev_lookup (&devicep->mem_map, devaddrs[i+1],
					  devaddrs[i + sizes[i]]
					  + sizes[i + sizes[i]], false);
		cdata[i].present = n2 != NULL;
		cdata[i].devaddr = devaddrs[i];
		struct_cpy = cdata[i].present ? 0 : sizes[i];
		if (!n2)
		  {
		    size_t sz = (size_t) (devaddrs[i + sizes[i]]
					  - devaddrs[i+1]
					  + sizes[i + sizes[i]]);
		    size_t align = (size_t) 1 << (kinds[i] >> 8);
		    cdata[i].aligned = true;
		    devaddrs[i] = (uintptr_t) gomp_aligned_alloc (align, sz);
		    devaddrs[i] -= devaddrs[i+1] - cdata[i].devaddr;
		  }
		else
		  devaddrs[i] = (n2->host_start + cdata[i].devaddr
				 - (n2->tgt->tgt_start + n2->tgt_offset));
		break;
	      default:
		gomp_mutex_unlock (&devicep->lock);
		gomp_fatal ("gomp_target_rev unhandled kind 0x%.4x", kinds[i]);
	    }
	}
      gomp_mutex_unlock (&devicep->lock);
    }

  host_fn (devaddrs);

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM) && mapnum > 0)
    {
      uint64_t struct_cpy = 0;
      bool clean_struct = false;
      for (uint64_t i = 0; i < mapnum; i++)
	{
	  if (cdata[i].devaddr == 0)
	    continue;
	  int kind = get_kind (short_mapkind, kinds, i) & typemask;
	  bool copy = !cdata[i].present || struct_cpy;
	  switch (kind)
	    {
	      case GOMP_MAP_FORCE_FROM:
	      case GOMP_MAP_FORCE_TOFROM:
	      case GOMP_MAP_ALWAYS_FROM:
	      case GOMP_MAP_ALWAYS_TOFROM:
		copy = true;
		/* FALLTHRU */
	      case GOMP_MAP_FROM:
	      case GOMP_MAP_TOFROM:
		if (copy && host_to_dev_cpy)
		  host_to_dev_cpy ((void *) (uintptr_t) cdata[i].devaddr,
				   (void *) (uintptr_t) devaddrs[i],
				   sizes[i], token);
		else if (copy)
		  gomp_copy_host2dev (devicep, NULL,
				      (void *) (uintptr_t) cdata[i].devaddr,
				      (void *) (uintptr_t) devaddrs[i],
				      sizes[i], false, NULL);
	      default:
		break;
	    }
	  if (struct_cpy)
	    {
	      struct_cpy--;
	      continue;
	    }
	  if (kind == GOMP_MAP_STRUCT && !cdata[i].present)
	    {
	      clean_struct = true;
	      struct_cpy = sizes[i];
	    }
	  else if (!cdata[i].present && cdata[i].aligned)
	    gomp_aligned_free ((void *) (uintptr_t) devaddrs[i]);
	  else if (!cdata[i].present)
	    free ((void *) (uintptr_t) devaddrs[i]);
	}
      if (clean_struct)
	for (uint64_t i = 0; i < mapnum; i++)
	  if (!cdata[i].present
	      && ((get_kind (short_mapkind, kinds, i) & typemask)
		  == GOMP_MAP_STRUCT))
	    {
	      devaddrs[i] += cdata[i+1].devaddr - cdata[i].devaddr;
	      gomp_aligned_free ((void *) (uintptr_t) devaddrs[i]);
	    }

      free (devaddrs);
      free (sizes);
      free (kinds);
    }
}

/* Host fallback for GOMP_target_data{,_ext} routines.  */

static void
gomp_target_data_fallback (struct gomp_device_descr *devicep)
{
  struct gomp_task_icv *icv = gomp_icv (false);

  if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_MANDATORY
      && devicep != NULL)
    gomp_fatal ("OMP_TARGET_OFFLOAD is set to MANDATORY, but device cannot "
		"be used for offloading");

  if (icv->target_data)
    {
      /* Even when doing a host fallback, if there are any active
         #pragma omp target data constructs, need to remember the
         new #pragma omp target data, otherwise GOMP_target_end_data
         would get out of sync.  */
      struct target_mem_desc *tgt
	= gomp_map_vars (NULL, 0, NULL, NULL, NULL, NULL, false,
			 NULL, GOMP_MAP_VARS_DATA);
      tgt->prev = icv->target_data;
      icv->target_data = tgt;
    }
}

void
GOMP_target_data (int device, const void *unused, size_t mapnum,
		  void **hostaddrs, size_t *sizes, unsigned char *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device, true);

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || (devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM))
    return gomp_target_data_fallback (devicep);

  struct target_mem_desc *tgt
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, false,
		     NULL, GOMP_MAP_VARS_DATA);
  struct gomp_task_icv *icv = gomp_icv (true);
  tgt->prev = icv->target_data;
  icv->target_data = tgt;
}

void
GOMP_target_data_ext (int device, size_t mapnum, void **hostaddrs,
		      size_t *sizes, unsigned short *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device, true);

  if (devicep == NULL
      || !(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return gomp_target_data_fallback (devicep);

  struct target_mem_desc *tgt
    = gomp_map_vars (devicep, mapnum, hostaddrs, NULL, sizes, kinds, true,
		     NULL, GOMP_MAP_VARS_DATA);
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
      gomp_unmap_vars (tgt, true, NULL);
    }
}

void
GOMP_target_update (int device, const void *unused, size_t mapnum,
		    void **hostaddrs, size_t *sizes, unsigned char *kinds)
{
  struct gomp_device_descr *devicep = resolve_device (device, true);

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
  struct gomp_device_descr *devicep = resolve_device (device, true);

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
	      if (__builtin_expect (gomp_cancel_var, 0) && team)
		{
		  if (gomp_team_barrier_cancelled (&team->barrier))
		    return;
		  if (thr->task->taskgroup)
		    {
		      if (thr->task->taskgroup->cancelled)
			return;
		      if (thr->task->taskgroup->workshare
			  && thr->task->taskgroup->prev
			  && thr->task->taskgroup->prev->cancelled)
			return;
		    }
		}

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
  if (__builtin_expect (gomp_cancel_var, 0) && team)
    {
      if (gomp_team_barrier_cancelled (&team->barrier))
	return;
      if (thr->task->taskgroup)
	{
	  if (thr->task->taskgroup->cancelled)
	    return;
	  if (thr->task->taskgroup->workshare
	      && thr->task->taskgroup->prev
	      && thr->task->taskgroup->prev->cancelled)
	    return;
	}
    }

  gomp_update (devicep, mapnum, hostaddrs, sizes, kinds, true);
}

static void
gomp_exit_data (struct gomp_device_descr *devicep, size_t mapnum,
		void **hostaddrs, size_t *sizes, unsigned short *kinds,
		htab_t *refcount_set)
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
    if ((kinds[i] & typemask) == GOMP_MAP_DETACH)
      {
	struct splay_tree_key_s cur_node;
	cur_node.host_start = (uintptr_t) hostaddrs[i];
	cur_node.host_end = cur_node.host_start + sizeof (void *);
	splay_tree_key n = splay_tree_lookup (&devicep->mem_map, &cur_node);

	if (n)
	  gomp_detach_pointer (devicep, NULL, n, (uintptr_t) hostaddrs[i],
			       false, NULL);
      }

  int nrmvars = 0;
  splay_tree_key remove_vars[mapnum];

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

	  bool delete_p = (kind == GOMP_MAP_DELETE
			   || kind == GOMP_MAP_DELETE_ZERO_LEN_ARRAY_SECTION);
	  bool do_copy, do_remove;
	  gomp_decrement_refcount (k, refcount_set, delete_p, &do_copy,
				   &do_remove);

	  if ((kind == GOMP_MAP_FROM && do_copy)
	      || kind == GOMP_MAP_ALWAYS_FROM)
	    {
	      if (k->aux && k->aux->attach_count)
		{
		  /* We have to be careful not to overwrite still attached
		     pointers during the copyback to host.  */
		  uintptr_t addr = k->host_start;
		  while (addr < k->host_end)
		    {
		      size_t i = (addr - k->host_start) / sizeof (void *);
		      if (k->aux->attach_count[i] == 0)
			gomp_copy_dev2host (devicep, NULL, (void *) addr,
					    (void *) (k->tgt->tgt_start
						      + k->tgt_offset
						      + addr - k->host_start),
					    sizeof (void *));
		      addr += sizeof (void *);
		    }
		}
	      else
		gomp_copy_dev2host (devicep, NULL, (void *) cur_node.host_start,
				    (void *) (k->tgt->tgt_start + k->tgt_offset
					      + cur_node.host_start
					      - k->host_start),
				    cur_node.host_end - cur_node.host_start);
	    }

	  /* Structure elements lists are removed altogether at once, which
	     may cause immediate deallocation of the target_mem_desc, causing
	     errors if we still have following element siblings to copy back.
	     While we're at it, it also seems more disciplined to simply
	     queue all removals together for processing below.

	     Structured block unmapping (i.e. gomp_unmap_vars_internal) should
	     not have this problem, since they maintain an additional
	     tgt->refcount = 1 reference to the target_mem_desc to start with.
	  */
	  if (do_remove)
	    remove_vars[nrmvars++] = k;
	  break;

	case GOMP_MAP_DETACH:
	  break;
	default:
	  gomp_mutex_unlock (&devicep->lock);
	  gomp_fatal ("GOMP_target_enter_exit_data unhandled kind 0x%.2x",
		      kind);
	}
    }

  for (int i = 0; i < nrmvars; i++)
    gomp_remove_var (devicep, remove_vars[i]);

  gomp_mutex_unlock (&devicep->lock);
}

void
GOMP_target_enter_exit_data (int device, size_t mapnum, void **hostaddrs,
			     size_t *sizes, unsigned short *kinds,
			     unsigned int flags, void **depend)
{
  struct gomp_device_descr *devicep = resolve_device (device, true);

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
	      if (__builtin_expect (gomp_cancel_var, 0) && team)
		{
		  if (gomp_team_barrier_cancelled (&team->barrier))
		    return;
		  if (thr->task->taskgroup)
		    {
		      if (thr->task->taskgroup->cancelled)
			return;
		      if (thr->task->taskgroup->workshare
			  && thr->task->taskgroup->prev
			  && thr->task->taskgroup->prev->cancelled)
			return;
		    }
		}

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
  if (__builtin_expect (gomp_cancel_var, 0) && team)
    {
      if (gomp_team_barrier_cancelled (&team->barrier))
	return;
      if (thr->task->taskgroup)
	{
	  if (thr->task->taskgroup->cancelled)
	    return;
	  if (thr->task->taskgroup->workshare
	      && thr->task->taskgroup->prev
	      && thr->task->taskgroup->prev->cancelled)
	    return;
	}
    }

  htab_t refcount_set = htab_create (mapnum);

  /* The variables are mapped separately such that they can be released
     independently.  */
  size_t i, j;
  if ((flags & GOMP_TARGET_FLAG_EXIT_DATA) == 0)
    for (i = 0; i < mapnum; i++)
      if ((kinds[i] & 0xff) == GOMP_MAP_STRUCT)
	{
	  gomp_map_vars (devicep, sizes[i] + 1, &hostaddrs[i], NULL, &sizes[i],
			 &kinds[i], true, &refcount_set,
			 GOMP_MAP_VARS_ENTER_DATA);
	  i += sizes[i];
	}
      else if ((kinds[i] & 0xff) == GOMP_MAP_TO_PSET)
	{
	  for (j = i + 1; j < mapnum; j++)
	    if (!GOMP_MAP_POINTER_P (get_kind (true, kinds, j) & 0xff)
		&& !GOMP_MAP_ALWAYS_POINTER_P (get_kind (true, kinds, j) & 0xff))
	      break;
	  gomp_map_vars (devicep, j-i, &hostaddrs[i], NULL, &sizes[i],
			 &kinds[i], true, &refcount_set,
			 GOMP_MAP_VARS_ENTER_DATA);
	  i += j - i - 1;
	}
      else if (i + 1 < mapnum
	       && ((kinds[i + 1] & 0xff) == GOMP_MAP_ATTACH
		   || ((kinds[i + 1] & 0xff) == GOMP_MAP_ALWAYS_POINTER
		       && (kinds[i] & 0xff) != GOMP_MAP_ALWAYS_POINTER)))
	{
	  /* An attach operation must be processed together with the mapped
	     base-pointer list item.  */
	  gomp_map_vars (devicep, 2, &hostaddrs[i], NULL, &sizes[i], &kinds[i],
			 true, &refcount_set, GOMP_MAP_VARS_ENTER_DATA);
	  i += 1;
	}
      else
	gomp_map_vars (devicep, 1, &hostaddrs[i], NULL, &sizes[i], &kinds[i],
		       true, &refcount_set, GOMP_MAP_VARS_ENTER_DATA);
  else
    gomp_exit_data (devicep, mapnum, hostaddrs, sizes, kinds, &refcount_set);
  htab_free (refcount_set);
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
	  gomp_target_fallback (ttask->fn, ttask->hostaddrs, devicep,
				ttask->args);
	  return false;
	}

      if (ttask->state == GOMP_TARGET_TASK_FINISHED)
	{
	  if (ttask->tgt)
	    gomp_unmap_vars (ttask->tgt, true, NULL);
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
				      NULL, GOMP_MAP_VARS_TARGET);
	  actual_arguments = (void *) ttask->tgt->tgt_start;
	}
      ttask->state = GOMP_TARGET_TASK_READY_TO_RUN;

      assert (devicep->async_run_func);
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
  else
    {
      htab_t refcount_set = htab_create (ttask->mapnum);
      if ((ttask->flags & GOMP_TARGET_FLAG_EXIT_DATA) == 0)
	for (i = 0; i < ttask->mapnum; i++)
	  if ((ttask->kinds[i] & 0xff) == GOMP_MAP_STRUCT)
	    {
	      gomp_map_vars (devicep, ttask->sizes[i] + 1, &ttask->hostaddrs[i],
			     NULL, &ttask->sizes[i], &ttask->kinds[i], true,
			     &refcount_set, GOMP_MAP_VARS_ENTER_DATA);
	      i += ttask->sizes[i];
	    }
	  else
	    gomp_map_vars (devicep, 1, &ttask->hostaddrs[i], NULL, &ttask->sizes[i],
			   &ttask->kinds[i], true, &refcount_set,
			   GOMP_MAP_VARS_ENTER_DATA);
      else
	gomp_exit_data (devicep, ttask->mapnum, ttask->hostaddrs, ttask->sizes,
			ttask->kinds, &refcount_set);
      htab_free (refcount_set);
    }
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

bool
GOMP_teams4 (unsigned int num_teams_low, unsigned int num_teams_high,
	     unsigned int thread_limit, bool first)
{
  struct gomp_thread *thr = gomp_thread ();
  if (first)
    {
      if (thread_limit)
	{
	  struct gomp_task_icv *icv = gomp_icv (true);
	  icv->thread_limit_var
	    = thread_limit > INT_MAX ? UINT_MAX : thread_limit;
	}
      (void) num_teams_high;
      if (num_teams_low == 0)
	num_teams_low = 1;
      thr->num_teams = num_teams_low - 1;
      thr->team_num = 0;
    }
  else if (thr->team_num == thr->num_teams)
    return false;
  else
    ++thr->team_num;
  return true;
}

void *
omp_target_alloc (size_t size, int device_num)
{
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    return malloc (size);

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
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
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    {
      free (device_ptr);
      return;
    }

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
  if (devicep == NULL || device_ptr == NULL)
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
omp_target_is_present (const void *ptr, int device_num)
{
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    return 1;

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
  if (devicep == NULL)
    return 0;

  if (ptr == NULL)
    return 1;

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

static int
omp_target_memcpy_check (int dst_device_num, int src_device_num,
			 struct gomp_device_descr **dst_devicep,
			 struct gomp_device_descr **src_devicep)
{
  if (dst_device_num != gomp_get_num_devices ()
      /* Above gomp_get_num_devices has to be called unconditionally.  */
      && dst_device_num != omp_initial_device)
    {
      *dst_devicep = resolve_device (dst_device_num, false);
      if (*dst_devicep == NULL)
	return EINVAL;

      if (!((*dst_devicep)->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || (*dst_devicep)->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	*dst_devicep = NULL;
    }

  if (src_device_num != num_devices_openmp
      && src_device_num != omp_initial_device)
    {
      *src_devicep = resolve_device (src_device_num, false);
      if (*src_devicep == NULL)
	return EINVAL;

      if (!((*src_devicep)->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
	  || (*src_devicep)->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
	*src_devicep = NULL;
    }

  return 0;
}

static int
omp_target_memcpy_copy (void *dst, const void *src, size_t length,
			size_t dst_offset, size_t src_offset,
			struct gomp_device_descr *dst_devicep,
			struct gomp_device_descr *src_devicep)
{
  bool ret;
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

int
omp_target_memcpy (void *dst, const void *src, size_t length, size_t dst_offset,
		   size_t src_offset, int dst_device_num, int src_device_num)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;
  int ret = omp_target_memcpy_check (dst_device_num, src_device_num,
				     &dst_devicep, &src_devicep);

  if (ret)
    return ret;

  ret = omp_target_memcpy_copy (dst, src, length, dst_offset, src_offset,
				dst_devicep, src_devicep);

  return ret;
}

typedef struct
{
  void *dst;
  const void *src;
  size_t length;
  size_t dst_offset;
  size_t src_offset;
  struct gomp_device_descr *dst_devicep;
  struct gomp_device_descr *src_devicep;
} omp_target_memcpy_data;

static void
omp_target_memcpy_async_helper (void *args)
{
  omp_target_memcpy_data *a = args;
  if (omp_target_memcpy_copy (a->dst, a->src, a->length, a->dst_offset,
			      a->src_offset, a->dst_devicep, a->src_devicep))
    gomp_fatal ("omp_target_memcpy failed");
}

int
omp_target_memcpy_async (void *dst, const void *src, size_t length,
			 size_t dst_offset, size_t src_offset,
			 int dst_device_num, int src_device_num,
			 int depobj_count, omp_depend_t *depobj_list)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;
  unsigned int flags = 0;
  void *depend[depobj_count + 5];
  int i;
  int check = omp_target_memcpy_check (dst_device_num, src_device_num,
				       &dst_devicep, &src_devicep);

  omp_target_memcpy_data s = {
    .dst = dst,
    .src = src,
    .length = length,
    .dst_offset = dst_offset,
    .src_offset = src_offset,
    .dst_devicep = dst_devicep,
    .src_devicep = src_devicep
  };

  if (check)
    return check;

  if (depobj_count > 0 && depobj_list != NULL)
    {
      flags |= GOMP_TASK_FLAG_DEPEND;
      depend[0] = 0;
      depend[1] = (void *) (uintptr_t) depobj_count;
      depend[2] = depend[3] = depend[4] = 0;
      for (i = 0; i < depobj_count; ++i)
	depend[i + 5] = &depobj_list[i];
    }

  GOMP_task (omp_target_memcpy_async_helper, &s, NULL, sizeof (s),
	     __alignof__ (s), true, flags, depend, 0, NULL);

  return 0;
}

static int
omp_target_memcpy_rect_worker (void *dst, const void *src, size_t element_size,
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
	  memcpy ((char *) dst + dst_off, (const char *) src + src_off,
		  length);
	  ret = 1;
	}
      else if (src_devicep == NULL)
	ret = dst_devicep->host2dev_func (dst_devicep->target_id,
					  (char *) dst + dst_off,
					  (const char *) src + src_off,
					  length);
      else if (dst_devicep == NULL)
	ret = src_devicep->dev2host_func (src_devicep->target_id,
					  (char *) dst + dst_off,
					  (const char *) src + src_off,
					  length);
      else if (src_devicep == dst_devicep)
	ret = src_devicep->dev2dev_func (src_devicep->target_id,
					 (char *) dst + dst_off,
					 (const char *) src + src_off,
					 length);
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
					   (const char *) src + src_off,
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

static int
omp_target_memcpy_rect_check (void *dst, const void *src, int dst_device_num,
			      int src_device_num,
			      struct gomp_device_descr **dst_devicep,
			      struct gomp_device_descr **src_devicep)
{
  if (!dst && !src)
    return INT_MAX;

  int ret = omp_target_memcpy_check (dst_device_num, src_device_num,
				     dst_devicep, src_devicep);
  if (ret)
    return ret;

  if (*src_devicep != NULL && *dst_devicep != NULL && *src_devicep != *dst_devicep)
    return EINVAL;

  return 0;
}

static int
omp_target_memcpy_rect_copy (void *dst, const void *src,
			     size_t element_size, int num_dims,
			     const size_t *volume, const size_t *dst_offsets,
			     const size_t *src_offsets,
			     const size_t *dst_dimensions,
			     const size_t *src_dimensions,
			     struct gomp_device_descr *dst_devicep,
			     struct gomp_device_descr *src_devicep)
{
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
omp_target_memcpy_rect (void *dst, const void *src, size_t element_size,
			int num_dims, const size_t *volume,
			const size_t *dst_offsets,
			const size_t *src_offsets,
			const size_t *dst_dimensions,
			const size_t *src_dimensions,
			int dst_device_num, int src_device_num)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;

  int check = omp_target_memcpy_rect_check (dst, src, dst_device_num,
					    src_device_num, &dst_devicep,
					    &src_devicep);

  if (check)
    return check;

  int ret = omp_target_memcpy_rect_copy (dst, src, element_size, num_dims,
					 volume, dst_offsets, src_offsets,
					 dst_dimensions, src_dimensions,
					 dst_devicep, src_devicep);

  return ret;
}

typedef struct
{
  void *dst;
  const void *src;
  size_t element_size;
  const size_t *volume;
  const size_t *dst_offsets;
  const size_t *src_offsets;
  const size_t *dst_dimensions;
  const size_t *src_dimensions;
  struct gomp_device_descr *dst_devicep;
  struct gomp_device_descr *src_devicep;
  int num_dims;
} omp_target_memcpy_rect_data;

static void
omp_target_memcpy_rect_async_helper (void *args)
{
  omp_target_memcpy_rect_data *a = args;
  int ret = omp_target_memcpy_rect_copy (a->dst, a->src, a->element_size,
					 a->num_dims, a->volume, a->dst_offsets,
					 a->src_offsets, a->dst_dimensions,
					 a->src_dimensions, a->dst_devicep,
					 a->src_devicep);
  if (ret)
    gomp_fatal ("omp_target_memcpy_rect failed");
}

int
omp_target_memcpy_rect_async (void *dst, const void *src, size_t element_size,
			      int num_dims, const size_t *volume,
			      const size_t *dst_offsets,
			      const size_t *src_offsets,
			      const size_t *dst_dimensions,
			      const size_t *src_dimensions,
			      int dst_device_num, int src_device_num,
			      int depobj_count, omp_depend_t *depobj_list)
{
  struct gomp_device_descr *dst_devicep = NULL, *src_devicep = NULL;
  unsigned flags = 0;
  int check = omp_target_memcpy_rect_check (dst, src, dst_device_num,
					    src_device_num, &dst_devicep,
					    &src_devicep);
  void *depend[depobj_count + 5];
  int i;

  omp_target_memcpy_rect_data s = {
    .dst = dst,
    .src = src,
    .element_size = element_size,
    .num_dims = num_dims,
    .volume = volume,
    .dst_offsets = dst_offsets,
    .src_offsets = src_offsets,
    .dst_dimensions = dst_dimensions,
    .src_dimensions = src_dimensions,
    .dst_devicep = dst_devicep,
    .src_devicep = src_devicep
  };

  if (check)
    return check;

  if (depobj_count > 0 && depobj_list != NULL)
    {
      flags |= GOMP_TASK_FLAG_DEPEND;
      depend[0] = 0;
      depend[1] = (void *) (uintptr_t) depobj_count;
      depend[2] = depend[3] = depend[4] = 0;
      for (i = 0; i < depobj_count; ++i)
	depend[i + 5] = &depobj_list[i];
    }

  GOMP_task (omp_target_memcpy_rect_async_helper, &s, NULL, sizeof (s),
	     __alignof__ (s), true, flags, depend, 0, NULL);

  return 0;
}

int
omp_target_associate_ptr (const void *host_ptr, const void *device_ptr,
			  size_t size, size_t device_offset, int device_num)
{
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    return EINVAL;

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
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
      k->dynamic_refcount = 0;
      k->aux = NULL;
      array->left = NULL;
      array->right = NULL;
      splay_tree_insert (&devicep->mem_map, array);
      ret = 0;
    }
  gomp_mutex_unlock (&devicep->lock);
  return ret;
}

int
omp_target_disassociate_ptr (const void *ptr, int device_num)
{
  struct gomp_device_descr *devicep = resolve_device (device_num, false);
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

void *
omp_get_mapped_ptr (const void *ptr, int device_num)
{
  if (device_num == omp_initial_device
      || device_num == omp_get_initial_device ())
    return (void *) ptr;

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
  if (devicep == NULL)
    return NULL;

  if (!(devicep->capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      || devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
    return (void *) ptr;

  gomp_mutex_lock (&devicep->lock);

  struct splay_tree_s *mem_map = &devicep->mem_map;
  struct splay_tree_key_s cur_node;
  void *ret = NULL;

  cur_node.host_start = (uintptr_t) ptr;
  cur_node.host_end = cur_node.host_start;
  splay_tree_key n = gomp_map_0len_lookup (mem_map, &cur_node);

  if (n)
    {
      uintptr_t offset = cur_node.host_start - n->host_start;
      ret = (void *) (n->tgt->tgt_start + n->tgt_offset + offset);
    }

  gomp_mutex_unlock (&devicep->lock);

  return ret;
}

int
omp_target_is_accessible (const void *ptr, size_t size, int device_num)
{
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    return true;

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
  if (devicep == NULL)
    return false;

  /* TODO: Unified shared memory must be handled when available.  */

  return devicep->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM;
}

int
omp_pause_resource (omp_pause_resource_t kind, int device_num)
{
  (void) kind;
  if (device_num == omp_initial_device
      || device_num == gomp_get_num_devices ())
    return gomp_pause_host ();

  struct gomp_device_descr *devicep = resolve_device (device_num, false);
  if (devicep == NULL)
    return -1;

  /* Do nothing for target devices for now.  */
  return 0;
}

int
omp_pause_resource_all (omp_pause_resource_t kind)
{
  (void) kind;
  if (gomp_pause_host ())
    return -1;
  /* Do nothing for target devices for now.  */
  return 0;
}

ialias (omp_pause_resource)
ialias (omp_pause_resource_all)

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
#if OFFLOAD_DEFAULTED
    return 0;
#else
    goto dl_fail;
#endif

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
      DLSYM_OPT (async_run, async_run);
      DLSYM_OPT (can_run, can_run);
      DLSYM (dev2dev);
    }
  if (device->capabilities & GOMP_OFFLOAD_CAP_OPENACC_200)
    {
      if (!DLSYM_OPT (openacc.exec, openacc_exec)
	  || !DLSYM_OPT (openacc.create_thread_data,
			 openacc_create_thread_data)
	  || !DLSYM_OPT (openacc.destroy_thread_data,
			 openacc_destroy_thread_data)
	  || !DLSYM_OPT (openacc.async.construct, openacc_async_construct)
	  || !DLSYM_OPT (openacc.async.destruct, openacc_async_destruct)
	  || !DLSYM_OPT (openacc.async.test, openacc_async_test)
	  || !DLSYM_OPT (openacc.async.synchronize, openacc_async_synchronize)
	  || !DLSYM_OPT (openacc.async.serialize, openacc_async_serialize)
	  || !DLSYM_OPT (openacc.async.queue_callback,
			 openacc_async_queue_callback)
	  || !DLSYM_OPT (openacc.async.exec, openacc_async_exec)
	  || !DLSYM_OPT (openacc.async.dev2host, openacc_async_dev2host)
	  || !DLSYM_OPT (openacc.async.host2dev, openacc_async_host2dev)
	  || !DLSYM_OPT (openacc.get_property, openacc_get_property))
	{
	  /* Require all the OpenACC handlers if we have
	     GOMP_OFFLOAD_CAP_OPENACC_200.  */
	  err = "plugin missing OpenACC handler function";
	  goto fail;
	}

      unsigned cuda = 0;
      cuda += DLSYM_OPT (openacc.cuda.get_current_device,
			 openacc_cuda_get_current_device);
      cuda += DLSYM_OPT (openacc.cuda.get_current_context,
			 openacc_cuda_get_current_context);
      cuda += DLSYM_OPT (openacc.cuda.get_stream, openacc_cuda_get_stream);
      cuda += DLSYM_OPT (openacc.cuda.set_stream, openacc_cuda_set_stream);
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
	ret = gomp_fini_device (devicep);
      gomp_mutex_unlock (&devicep->lock);
      if (!ret)
	gomp_fatal ("device finalization failed");
    }
}

/* This function initializes the runtime for offloading.
   It parses the list of offload plugins, and tries to load these.
   On return, the variables NUM_DEVICES and NUM_DEVICES_OPENMP
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
  int i, new_num_devs;
  int num_devs = 0, num_devs_openmp;
  struct gomp_device_descr *devs = NULL;

  if (gomp_target_offload_var == GOMP_TARGET_OFFLOAD_DISABLED)
    return;

  cur = OFFLOAD_PLUGINS;
  if (*cur)
    do
      {
	struct gomp_device_descr current_device;
	size_t prefix_len, suffix_len, cur_len;

	next = strchr (cur, ',');

	prefix_len = strlen (prefix);
	cur_len = next ? next - cur : strlen (cur);
	suffix_len = strlen (suffix);

	plugin_name = (char *) malloc (prefix_len + cur_len + suffix_len + 1);
	if (!plugin_name)
	  {
	    num_devs = 0;
	    break;
	  }

	memcpy (plugin_name, prefix, prefix_len);
	memcpy (plugin_name + prefix_len, cur, cur_len);
	memcpy (plugin_name + prefix_len + cur_len, suffix, suffix_len + 1);

	if (gomp_load_plugin_for_device (&current_device, plugin_name))
	  {
	    int omp_req = omp_requires_mask & ~GOMP_REQUIRES_TARGET_USED;
	    new_num_devs = current_device.get_num_devices_func (omp_req);
	    if (gomp_debug_var > 0 && new_num_devs < 0)
	      {
		bool found = false;
		int type = current_device.get_type_func ();
		for (int img = 0; img < num_offload_images; img++)
		  if (type == offload_images[img].type)
		    found = true;
		if (found)
		  {
		    char buf[sizeof ("unified_address, unified_shared_memory, "
				     "reverse_offload")];
		    gomp_requires_to_name (buf, sizeof (buf), omp_req);
		    char *name = (char *) malloc (cur_len + 1);
		    memcpy (name, cur, cur_len);
		    name[cur_len] = '\0';
		    gomp_debug (1,
				"%s devices present but 'omp requires %s' "
				"cannot be fulfilled\n", name, buf);
		    free (name);
		  }
	      }
	    else if (new_num_devs >= 1)
	      {
		/* Augment DEVICES and NUM_DEVICES.  */

		devs = realloc (devs, (num_devs + new_num_devs)
				      * sizeof (struct gomp_device_descr));
		if (!devs)
		  {
		    num_devs = 0;
		    free (plugin_name);
		    break;
		  }

		current_device.name = current_device.get_name_func ();
		/* current_device.capabilities has already been set.  */
		current_device.type = current_device.get_type_func ();
		current_device.mem_map.root = NULL;
		current_device.mem_map_rev.root = NULL;
		current_device.state = GOMP_DEVICE_UNINITIALIZED;
		for (i = 0; i < new_num_devs; i++)
		  {
		    current_device.target_id = i;
		    devs[num_devs] = current_device;
		    gomp_mutex_init (&devs[num_devs].lock);
		    num_devs++;
		  }
	      }
	  }

	free (plugin_name);
	cur = next + 1;
      }
    while (next);

  /* In DEVICES, sort the GOMP_OFFLOAD_CAP_OPENMP_400 ones first, and set
     NUM_DEVICES_OPENMP.  */
  struct gomp_device_descr *devs_s
    = malloc (num_devs * sizeof (struct gomp_device_descr));
  if (!devs_s)
    {
      num_devs = 0;
      free (devs);
      devs = NULL;
    }
  num_devs_openmp = 0;
  for (i = 0; i < num_devs; i++)
    if (devs[i].capabilities & GOMP_OFFLOAD_CAP_OPENMP_400)
      devs_s[num_devs_openmp++] = devs[i];
  int num_devs_after_openmp = num_devs_openmp;
  for (i = 0; i < num_devs; i++)
    if (!(devs[i].capabilities & GOMP_OFFLOAD_CAP_OPENMP_400))
      devs_s[num_devs_after_openmp++] = devs[i];
  free (devs);
  devs = devs_s;

  for (i = 0; i < num_devs; i++)
    {
      /* The 'devices' array can be moved (by the realloc call) until we have
	 found all the plugins, so registering with the OpenACC runtime (which
	 takes a copy of the pointer argument) must be delayed until now.  */
      if (devs[i].capabilities & GOMP_OFFLOAD_CAP_OPENACC_200)
	goacc_register (&devs[i]);
    }

  num_devices = num_devs;
  num_devices_openmp = num_devs_openmp;
  devices = devs;
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
