/* Copyright (C) 2013-2023 Free Software Foundation, Inc.

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
#include "gomp-constants.h"
#include "oacc-int.h"
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIu64.  */
#endif
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <stdio.h>

/* In the ABI, the GOACC_FLAGs are encoded as an inverted bitmask, so that we
   continue to support the following two legacy values.  */
_Static_assert (GOACC_FLAGS_UNMARSHAL (GOMP_DEVICE_ICV) == 0,
		"legacy GOMP_DEVICE_ICV broken");
_Static_assert (GOACC_FLAGS_UNMARSHAL (GOMP_DEVICE_HOST_FALLBACK)
		== GOACC_FLAG_HOST_FALLBACK,
		"legacy GOMP_DEVICE_HOST_FALLBACK broken");

static size_t
goacc_noncontig_array_count_rows (struct goacc_ncarray_descr_type *descr)
{
  size_t nrows = 1;
  for (size_t d = 0; d < descr->ndims - 1; d++)
    nrows *= descr->dims[d].length / sizeof (void *);
  return nrows;
}

static void
goacc_noncontig_array_compute_sizes (struct goacc_ncarray *nca)
{
  size_t d, n = 1;
  struct goacc_ncarray_descr_type *descr = nca->descr;

  nca->ptrblock_size = 0;
  for (d = 0; d < descr->ndims - 1; d++)
    {
      size_t dim_count = descr->dims[d].length / descr->dims[d].elem_size;
      size_t dim_ptrblock_size = (descr->dims[d + 1].is_array
				  ? 0 : descr->dims[d].length * n);
      nca->ptrblock_size += dim_ptrblock_size;
      n *= dim_count;
    }
  nca->data_row_num = n;
  nca->data_row_size = descr->dims[d].length;
}

static void
goacc_noncontig_array_fill_rows_1 (struct goacc_ncarray_descr_type *descr, void *nca,
				   size_t d, void ***row_ptr, size_t *count)
{
  if (d < descr->ndims - 1)
    {
      size_t elsize = descr->dims[d].elem_size;
      size_t n = descr->dims[d].length / elsize;
      void *p = nca + descr->dims[d].base;
      for (size_t i = 0; i < n; i++)
	{
	  void *ptr = p + i * elsize;
	  /* Deref if next dimension is not array.  */
	  if (!descr->dims[d + 1].is_array)
	    ptr = *((void **) ptr);
	  goacc_noncontig_array_fill_rows_1 (descr, ptr, d + 1, row_ptr, count);
	}
    }
  else
    {
      **row_ptr = nca + descr->dims[d].base;
      *row_ptr += 1;
      *count += 1;
    }
}

static size_t
goacc_noncontig_array_fill_rows (struct goacc_ncarray *nca)
{
  size_t count = 0;
  void **p = nca->data_rows;
  goacc_noncontig_array_fill_rows_1 (nca->descr, nca->ptr, 0, &p, &count);
  return count;
}

static struct goacc_ncarray_info *
goacc_process_noncontiguous_arrays (size_t mapnum, void **hostaddrs,
				    unsigned short *kinds, va_list* ap)
{
  size_t i, nr, num_data_rows = 0, num_ncarray = 0, curr_row_start = 0;
  struct goacc_ncarray_descr_type *descr;

  /* We need to go over *ap twice, so preserve *ap state here.  */
  va_list itr;
  va_copy (itr, *ap);
  for (i = 0; i < mapnum; i++)
    if (GOMP_MAP_NONCONTIG_ARRAY_P (kinds[i] & 0xff))
      {
	descr = va_arg (itr, struct goacc_ncarray_descr_type *);
	num_data_rows += goacc_noncontig_array_count_rows (descr);
	num_ncarray += 1;
      }
    else
      break;

  /* Allocate the entire info struct, array entries, and row pointer
     arrays in one large block.  */
  struct goacc_ncarray_info *nca_info
    = gomp_malloc (sizeof (struct goacc_ncarray_info)
		   + sizeof (struct goacc_ncarray) * num_ncarray
		   + sizeof (void *) * num_data_rows * 2);
  nca_info->num_data_rows = num_data_rows;
  nca_info->num_ncarray = num_ncarray;
  nca_info->data_rows = (void **) (nca_info->ncarray + num_ncarray);
  nca_info->tgt_data_rows = nca_info->data_rows + num_data_rows;

  struct goacc_ncarray *curr_ncarray = nca_info->ncarray;
  for (i = 0; i < mapnum; i++)
    if (GOMP_MAP_NONCONTIG_ARRAY_P (kinds[i] & 0xff))
      {
	descr = va_arg (*ap, struct goacc_ncarray_descr_type *);
	curr_ncarray->descr = descr;
	curr_ncarray->ptr = hostaddrs[i];
	curr_ncarray->map_index = i;

	goacc_noncontig_array_compute_sizes (curr_ncarray);

	curr_ncarray->data_rows = nca_info->data_rows + curr_row_start;
	curr_ncarray->tgt_data_rows = nca_info->tgt_data_rows + curr_row_start;

	nr = goacc_noncontig_array_fill_rows (curr_ncarray);
	assert (nr == curr_ncarray->data_row_num);
	curr_row_start += nr;
	curr_ncarray += 1;
      }
    else
      break;

  return nca_info;
}

void *
goacc_noncontig_array_create_ptrblock (struct goacc_ncarray *nca,
				       void *tgt_ptrblock_addr)
{
  struct goacc_ncarray_descr_type *descr = nca->descr;
  void **tgt_data_rows = nca->tgt_data_rows;
  void *ptrblock = gomp_malloc (nca->ptrblock_size);
  void **curr_dim_ptrblock = (void **) ptrblock;
  size_t n = 1;

  for (size_t d = 0; d < descr->ndims - 1; d++)
    {
      int curr_dim_len = descr->dims[d].length;
      int next_dim_len = descr->dims[d + 1].length;
      int curr_dim_num = curr_dim_len / sizeof (void *);
      size_t next_dim_bias = descr->dims[d + 1].base;

      void *next_dim_ptrblock
	= (void *)(curr_dim_ptrblock + n * curr_dim_num);

      for (int b = 0; b < n; b++)
	for (int i = 0; i < curr_dim_num; i++)
	  {
	    if (d < descr->ndims - 2)
	      {
		void *ptr = (next_dim_ptrblock
			     + b * curr_dim_num * next_dim_len
			     + i * next_dim_len);
		void *tgt_ptr = (tgt_ptrblock_addr
				 + (ptr - ptrblock) - next_dim_bias);
		curr_dim_ptrblock[b * curr_dim_num + i] = tgt_ptr;
	      }
	    else
	      {
		curr_dim_ptrblock[b * curr_dim_num + i]
		  = tgt_data_rows[b * curr_dim_num + i] - next_dim_bias;
	      }
	    void *addr = &curr_dim_ptrblock[b * curr_dim_num + i];
	    assert (ptrblock <= addr && addr < ptrblock + nca->ptrblock_size);
	  }

      n *= curr_dim_num;
      curr_dim_ptrblock = next_dim_ptrblock;
    }
  assert (n == nca->data_row_num);
  return ptrblock;
}

/* Handle the mapping pair that are presented when a
   deviceptr clause is used with Fortran.  */

static void
handle_ftn_pointers (size_t mapnum, void **hostaddrs, size_t *sizes,
		     unsigned short *kinds)
{
  int i;

  for (i = 0; i < mapnum; i++)
    {
      unsigned short kind1 = kinds[i] & 0xff;

      /* Handle Fortran deviceptr clause.  */
      if (kind1 == GOMP_MAP_FORCE_DEVICEPTR)
	{
	  unsigned short kind2;

	  if (i < (signed)mapnum - 1)
	    kind2 = kinds[i + 1] & 0xff;
	  else
	    kind2 = 0xffff;

	  if (sizes[i] == sizeof (void *))
	    continue;

	  /* At this point, we're dealing with a Fortran deviceptr.
	     If the next element is not what we're expecting, then
	     this is an instance of where the deviceptr variable was
	     not used within the region and the pointer was removed
	     by the gimplifier.  */
	  if (kind2 == GOMP_MAP_POINTER
	      && sizes[i + 1] == 0
	      && hostaddrs[i] == *(void **)hostaddrs[i + 1])
	    {
	      kinds[i+1] = kinds[i];
	      sizes[i+1] = sizeof (void *);
	    }

	  /* Invalidate the entry.  */
	  hostaddrs[i] = NULL;
	}
    }
}


/* Launch a possibly offloaded function with FLAGS.  FN is the host fn
   address.  MAPNUM, HOSTADDRS, SIZES & KINDS  describe the memory
   blocks to be copied to/from the device.  Varadic arguments are
   keyed optional parameters terminated with a zero.  */

void
GOACC_parallel_keyed (int flags_m, void (*fn) (void *),
		      size_t mapnum, void **hostaddrs, size_t *sizes,
		      unsigned short *kinds, ...)
{
  int flags = GOACC_FLAGS_UNMARSHAL (flags_m);

  va_list ap;
  struct goacc_thread *thr;
  struct gomp_device_descr *acc_dev;
  unsigned int i;
  struct splay_tree_key_s k;
  splay_tree_key tgt_fn_key;
  void (*tgt_fn);
  int async = GOMP_ASYNC_SYNC;
  unsigned dims[GOMP_DIM_MAX];
  unsigned tag;
  struct goacc_ncarray_info *nca_info = NULL;

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

  bool profiling_p = GOACC_PROFILING_DISPATCH_P (true);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      thr->prof_info = &prof_info;

      prof_info.event_type = acc_ev_compute_construct_start;
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
  acc_event_info compute_construct_event_info;
  if (profiling_p)
    {
      compute_construct_event_info.other_event.event_type
	= prof_info.event_type;
      compute_construct_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      compute_construct_event_info.other_event.parent_construct
	= acc_construct_parallel;
      compute_construct_event_info.other_event.implicit = 0;
      compute_construct_event_info.other_event.tool_info = NULL;
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
    goacc_profiling_dispatch (&prof_info, &compute_construct_event_info,
			      &api_info);

  handle_ftn_pointers (mapnum, hostaddrs, sizes, kinds);

  /* Host fallback if "if" clause is false or if the current device is set to
     the host.  */
  if (flags & GOACC_FLAG_HOST_FALLBACK)
    {
      prof_info.device_type = acc_device_host;
      api_info.device_type = prof_info.device_type;
      goacc_save_and_set_bind (acc_device_host);
      fn (hostaddrs);
      goacc_restore_bind ();
      goto out_prof;
    }
  else if (acc_device_type (acc_dev->type) == acc_device_host)
    {
      fn (hostaddrs);
      goto out_prof;
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

	    if (profiling_p)
	      {
		prof_info.async = async;
		prof_info.async_queue = prof_info.async;
	      }

	    break;
	  }

	case GOMP_LAUNCH_WAIT:
	  {
	    unsigned num_waits = GOMP_LAUNCH_OP (tag);
	    goacc_wait (async, num_waits, &ap);
	    break;
	  }

	  /*case GOMP_LAUNCH_NONCONTIG_ARRAYS:
	  nca_info = goacc_process_noncontiguous_arrays (mapnum, hostaddrs,
							 kinds, &ap);
							 break;*/

	default:
	  gomp_fatal ("unrecognized offload code '%d',"
		      " libgomp is too old", GOMP_LAUNCH_CODE (tag));
	}
    }

  if (mapnum > 0 && GOMP_MAP_NONCONTIG_ARRAY_P (kinds[0] & 0xff))
    nca_info = goacc_process_noncontiguous_arrays (mapnum, hostaddrs, kinds, &ap);

  va_end (ap);

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

  acc_event_info enter_exit_data_event_info;
  if (profiling_p)
    {
      prof_info.event_type = acc_ev_enter_data_start;
      enter_exit_data_event_info.other_event.event_type
	= prof_info.event_type;
      enter_exit_data_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      enter_exit_data_event_info.other_event.parent_construct
	= compute_construct_event_info.other_event.parent_construct;
      enter_exit_data_event_info.other_event.implicit = 1;
      enter_exit_data_event_info.other_event.tool_info = NULL;
      goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
				&api_info);
    }

  goacc_aq aq = get_goacc_asyncqueue (async);

  struct target_mem_desc *tgt
      = gomp_map_vars_openacc (acc_dev, aq, mapnum, hostaddrs, sizes, kinds,
			       nca_info);
  free (nca_info);

  if (profiling_p)
    {
      prof_info.event_type = acc_ev_enter_data_end;
      enter_exit_data_event_info.other_event.event_type
	= prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
				&api_info);
    }

  void **devaddrs = (void **) tgt->tgt_start;
  if (aq == NULL)
    acc_dev->openacc.exec_func (tgt_fn, mapnum, hostaddrs, devaddrs, dims,
				tgt);
  else
    acc_dev->openacc.async.exec_func (tgt_fn, mapnum, hostaddrs, devaddrs,
				      dims, tgt, aq);

  if (profiling_p)
    {
      prof_info.event_type = acc_ev_exit_data_start;
      enter_exit_data_event_info.other_event.event_type = prof_info.event_type;
      enter_exit_data_event_info.other_event.tool_info = NULL;
      goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
				&api_info);
    }

  /* If running synchronously (aq == NULL), this will unmap immediately.  */
  goacc_unmap_vars (tgt, true, aq);

  if (profiling_p)
    {
      prof_info.event_type = acc_ev_exit_data_end;
      enter_exit_data_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &enter_exit_data_event_info,
				&api_info);
    }

 out_prof:
  if (profiling_p)
    {
      prof_info.event_type = acc_ev_compute_construct_end;
      compute_construct_event_info.other_event.event_type
	= prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &compute_construct_event_info,
				&api_info);

      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

/* Legacy entry point (GCC 5).  Only provide host fallback execution.  */

void
GOACC_parallel (int flags_m, void (*fn) (void *),
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
GOACC_data_start (int flags_m, size_t mapnum,
		  void **hostaddrs, size_t *sizes, unsigned short *kinds, ...)
{
  int flags = GOACC_FLAGS_UNMARSHAL (flags_m);

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

  bool profiling_p = GOACC_PROFILING_DISPATCH_P (true);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      thr->prof_info = &prof_info;

      prof_info.event_type = acc_ev_enter_data_start;
      prof_info.valid_bytes = _ACC_PROF_INFO_VALID_BYTES;
      prof_info.version = _ACC_PROF_INFO_VERSION;
      prof_info.device_type = acc_device_type (acc_dev->type);
      prof_info.device_number = acc_dev->target_id;
      prof_info.thread_id = -1;
      prof_info.async = acc_async_sync; /* Always synchronous.  */
      prof_info.async_queue = prof_info.async;
      prof_info.src_file = NULL;
      prof_info.func_name = NULL;
      prof_info.line_no = -1;
      prof_info.end_line_no = -1;
      prof_info.func_line_no = -1;
      prof_info.func_end_line_no = -1;
    }
  acc_event_info enter_data_event_info;
  if (profiling_p)
    {
      enter_data_event_info.other_event.event_type
	= prof_info.event_type;
      enter_data_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      enter_data_event_info.other_event.parent_construct = acc_construct_data;
      for (int i = 0; i < mapnum; ++i)
	if ((kinds[i] & 0xff) == GOMP_MAP_USE_DEVICE_PTR
	    || (kinds[i] & 0xff) == GOMP_MAP_USE_DEVICE_PTR_IF_PRESENT)
	  {
	    /* If there is one such data mapping kind, then this is actually an
	       OpenACC 'host_data' construct.  (GCC maps the OpenACC
	       'host_data' construct to the OpenACC 'data' construct.)  Apart
	       from artificial test cases (such as an OpenACC 'host_data'
	       construct's (implicit) device initialization when there hasn't
	       been any device data be set up before...), there can't really
	       any meaningful events be generated from OpenACC 'host_data'
	       constructs, though.  */
	    enter_data_event_info.other_event.parent_construct
	      = acc_construct_host_data;
	    break;
	  }
      enter_data_event_info.other_event.implicit = 0;
      enter_data_event_info.other_event.tool_info = NULL;
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
    goacc_profiling_dispatch (&prof_info, &enter_data_event_info, &api_info);

  handle_ftn_pointers (mapnum, hostaddrs, sizes, kinds);

  /* Host fallback or 'do nothing'.  */
  if ((acc_dev->capabilities & GOMP_OFFLOAD_CAP_SHARED_MEM)
      || (flags & GOACC_FLAG_HOST_FALLBACK))
    {
      prof_info.device_type = acc_device_host;
      api_info.device_type = prof_info.device_type;
      tgt = gomp_map_vars_openacc (NULL, NULL, 0, NULL, NULL, NULL, NULL);
      tgt->prev = thr->mapped_data;
      thr->mapped_data = tgt;

      goto out_prof;
    }

  struct goacc_ncarray_info *nca_info = NULL;
  if (mapnum > 0 && GOMP_MAP_NONCONTIG_ARRAY_P (kinds[0] & 0xff))
    {
      va_list ap;
      va_start (ap, kinds);
      nca_info = goacc_process_noncontiguous_arrays (mapnum, hostaddrs, kinds, &ap);
      va_end (ap);
    }

  gomp_debug (0, "  %s: prepare mappings\n", __FUNCTION__);
  tgt = gomp_map_vars_openacc (acc_dev, NULL, mapnum, hostaddrs, sizes, kinds,
			       nca_info);
  free (nca_info);
  gomp_debug (0, "  %s: mappings prepared\n", __FUNCTION__);
  tgt->prev = thr->mapped_data;
  thr->mapped_data = tgt;

 out_prof:
  if (profiling_p)
    {
      prof_info.event_type = acc_ev_enter_data_end;
      enter_data_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &enter_data_event_info, &api_info);

      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

void
GOACC_data_end (void)
{
  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;
  struct target_mem_desc *tgt = thr->mapped_data;

  bool profiling_p = GOACC_PROFILING_DISPATCH_P (true);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      thr->prof_info = &prof_info;

      prof_info.event_type = acc_ev_exit_data_start;
      prof_info.valid_bytes = _ACC_PROF_INFO_VALID_BYTES;
      prof_info.version = _ACC_PROF_INFO_VERSION;
      prof_info.device_type = acc_device_type (acc_dev->type);
      prof_info.device_number = acc_dev->target_id;
      prof_info.thread_id = -1;
      prof_info.async = acc_async_sync; /* Always synchronous.  */
      prof_info.async_queue = prof_info.async;
      prof_info.src_file = NULL;
      prof_info.func_name = NULL;
      prof_info.line_no = -1;
      prof_info.end_line_no = -1;
      prof_info.func_line_no = -1;
      prof_info.func_end_line_no = -1;
    }
  acc_event_info exit_data_event_info;
  if (profiling_p)
    {
      exit_data_event_info.other_event.event_type
	= prof_info.event_type;
      exit_data_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      exit_data_event_info.other_event.parent_construct = acc_construct_data;
      exit_data_event_info.other_event.implicit = 0;
      exit_data_event_info.other_event.tool_info = NULL;
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
    goacc_profiling_dispatch (&prof_info, &exit_data_event_info, &api_info);

  gomp_debug (0, "  %s: restore mappings\n", __FUNCTION__);
  thr->mapped_data = tgt->prev;
  goacc_unmap_vars (tgt, true, NULL);
  gomp_debug (0, "  %s: mappings restored\n", __FUNCTION__);

  if (profiling_p)
    {
      prof_info.event_type = acc_ev_exit_data_end;
      exit_data_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &exit_data_event_info, &api_info);

      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}

void
GOACC_update (int flags_m, size_t mapnum,
	      void **hostaddrs, size_t *sizes, unsigned short *kinds,
	      int async, int num_waits, ...)
{
  int flags = GOACC_FLAGS_UNMARSHAL (flags_m);

  size_t i;

  goacc_lazy_initialize ();

  struct goacc_thread *thr = goacc_thread ();
  struct gomp_device_descr *acc_dev = thr->dev;

  bool profiling_p = GOACC_PROFILING_DISPATCH_P (true);

  acc_prof_info prof_info;
  if (profiling_p)
    {
      thr->prof_info = &prof_info;

      prof_info.event_type = acc_ev_update_start;
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
  acc_event_info update_event_info;
  if (profiling_p)
    {
      update_event_info.other_event.event_type
	= prof_info.event_type;
      update_event_info.other_event.valid_bytes
	= _ACC_OTHER_EVENT_INFO_VALID_BYTES;
      update_event_info.other_event.parent_construct = acc_construct_update;
      update_event_info.other_event.implicit = 0;
      update_event_info.other_event.tool_info = NULL;
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
    goacc_profiling_dispatch (&prof_info, &update_event_info, &api_info);

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

  bool update_device = false;
  for (i = 0; i < mapnum; ++i)
    {
      unsigned char kind = kinds[i] & 0xff;

      switch (kind)
	{
	case GOMP_MAP_POINTER:
	case GOMP_MAP_TO_PSET:
	  break;

	case GOMP_MAP_ALWAYS_POINTER:
	  if (update_device)
	    {
	      /* Save the contents of the host pointer.  */
	      void *dptr = acc_deviceptr (hostaddrs[i-1]);
	      uintptr_t t = *(uintptr_t *) hostaddrs[i];

	      /* Update the contents of the host pointer to reflect
		 the value of the allocated device memory in the
		 previous pointer.  */
	      *(uintptr_t *) hostaddrs[i] = (uintptr_t)dptr;
	      /* TODO: verify that we really cannot use acc_update_device_async
		 here.  */
	      acc_update_device (hostaddrs[i], sizeof (uintptr_t));

	      /* Restore the host pointer.  */
	      *(uintptr_t *) hostaddrs[i] = t;
	      update_device = false;
	    }
	  break;

	case GOMP_MAP_TO:
	  if (!acc_is_present (hostaddrs[i], sizes[i]))
	    {
	      update_device = false;
	      break;
	    }
	  /* Fallthru  */
	case GOMP_MAP_FORCE_TO:
	  update_device = true;
	  acc_update_device_async (hostaddrs[i], sizes[i], async);
	  break;

	case GOMP_MAP_FROM:
	  if (!acc_is_present (hostaddrs[i], sizes[i]))
	    {
	      update_device = false;
	      break;
	    }
	  /* Fallthru  */
	case GOMP_MAP_FORCE_FROM:
	  update_device = false;
	  acc_update_self_async (hostaddrs[i], sizes[i], async);
	  break;

	default:
	  gomp_fatal (">>>> GOACC_update UNHANDLED kind 0x%.2x", kind);
	  break;
	}
    }

 out_prof:
  if (profiling_p)
    {
      prof_info.event_type = acc_ev_update_end;
      update_event_info.other_event.event_type = prof_info.event_type;
      goacc_profiling_dispatch (&prof_info, &update_event_info, &api_info);

      thr->prof_info = NULL;
      thr->api_info = NULL;
    }
}


/* Legacy entry point (GCC 5).  */

int
GOACC_get_num_threads (void)
{
  return 1;
}

/* Legacy entry point (GCC 5).  */

int
GOACC_get_thread_num (void)
{
  return 0;
}
