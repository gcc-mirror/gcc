/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
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

#include "collective_subroutine.h"
#include "supervisor.h"
#include "teams_mgmt.h"
#include "thread_support.h"

#include <string.h>

/* Usage:
     pack_info pi;
     packed = pack_array_prepare (&pi, source);

 // Awesome allocation of destptr using pi.num_elem
 if (packed)
   memcpy (...);
 else
   pack_array_finish (&pi, source, destptr);

This could also be used in in_pack_generic.c.  Additionally, since
pack_array_prepare is the same for all type sizes, we would only have to
specialize pack_array_finish, saving on code size.  */

typedef struct
{
  index_type num_elem;
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS]; /* Stride is byte-based.  */
} pack_info;

static bool
pack_array_prepare (pack_info *pi, const gfc_descriptor_t *source)
{
  index_type dim;
  bool packed;
  index_type span;
  index_type type_size;
  index_type ssize;

  dim = GFC_DESCRIPTOR_RANK (source);
  type_size = GFC_DESCRIPTOR_SIZE (source);
  ssize = type_size;

  pi->num_elem = 1;
  packed = true;
  span = source->span != 0 ? source->span : type_size;
  for (index_type n = 0; n < dim; n++)
    {
      pi->stride[n] = GFC_DESCRIPTOR_STRIDE (source, n) * span;
      pi->extent[n] = GFC_DESCRIPTOR_EXTENT (source, n);
      if (pi->extent[n] <= 0)
	{
	  /* Do nothing.  */
	  packed = true;
	  pi->num_elem = 0;
	  break;
	}

      if (ssize != pi->stride[n])
	packed = false;

      pi->num_elem *= pi->extent[n];
      ssize *= pi->extent[n];
    }

  return packed;
}

static void
pack_array_finish (const pack_info *pi, const gfc_descriptor_t *source,
		   char *dest)
{
  index_type dim;
  const char *restrict src;

  index_type size;
  index_type stride0;
  index_type count[GFC_MAX_DIMENSIONS];

  dim = GFC_DESCRIPTOR_RANK (source);
  src = source->base_addr;
  stride0 = pi->stride[0];
  size = GFC_DESCRIPTOR_SIZE (source);
  memset (count, '\0', sizeof (index_type) * dim);
  while (src)
    {
      /* Copy the data.  */
      memcpy (dest, src, size);
      /* Advance to the next element.  */
      dest += size;
      src += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
      while (count[n] == pi->extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  src -= pi->stride[n] * pi->extent[n];
	  n++;
	  if (n == dim)
	    {
	      src = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      src += pi->stride[n];
	    }
	}
    }
}

static void
unpack_array_finish (const pack_info *pi, const gfc_descriptor_t *d,
		     const void *src)
{
  index_type stride0;
  char *restrict dest;
  index_type size;
  index_type count[GFC_MAX_DIMENSIONS];
  index_type dim;

  size = GFC_DESCRIPTOR_SIZE (d);
  stride0 = pi->stride[0];
  dest = d->base_addr;
  dim = GFC_DESCRIPTOR_RANK (d);

  memset (count, '\0', sizeof (index_type) * dim);
  while (dest)
    {
      memcpy (dest, src, size);
      src += size;
      dest += stride0;
      count[0]++;
      index_type n = 0;
      while (count[n] == pi->extent[n])
	{
	  count[n] = 0;
	  dest -= pi->stride[n] * pi->extent[n];
	  n++;
	  if (n == dim)
	    {
	      dest = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += pi->stride[n];
	    }
	}
    }
}

void
collsub_init_supervisor (collsub_shared *cis, allocator *al,
			 const int init_num_images)
{
  /* Choose an arbitrary large buffer.  It can grow later if needed.  */
  const size_t init_size = 1U << 10;

  cis->curr_size = init_size;
  cis->collsub_buf = allocator_shared_malloc (al, init_size);

  counter_barrier_init (&cis->barrier, init_num_images);
  initialize_shared_mutex (&cis->mutex);
}

static void *
get_collsub_buf (size_t size)
{
  void *ret;

  pthread_mutex_lock (&caf_current_team->u.image_info->collsub.mutex);
  /* curr_size is always at least sizeof(double), so we don't need to worry
     about size == 0.  */
  if (size > caf_current_team->u.image_info->collsub.curr_size)
    {
      allocator_shared_free (
	alloc_get_allocator (&local->ai),
	caf_current_team->u.image_info->collsub.collsub_buf,
	caf_current_team->u.image_info->collsub.curr_size);
      caf_current_team->u.image_info->collsub.collsub_buf
	= allocator_shared_malloc (alloc_get_allocator (&local->ai), size);
      caf_current_team->u.image_info->collsub.curr_size = size;
    }

  ret = SHMPTR_AS (void *, caf_current_team->u.image_info->collsub.collsub_buf,
		   &local->sm);
  pthread_mutex_unlock (&caf_current_team->u.image_info->collsub.mutex);
  return ret;
}

/* This function syncs all images with one another.  It will only return once
   all images have called it.  */

static void
collsub_sync (void)
{
  counter_barrier_wait (&caf_current_team->u.image_info->collsub.barrier);
}

typedef void *(*red_op) (void *, void *);
typedef void (*ass_op) (red_op, void *, void *, size_t);

#define GEN_FOR_BITS(BITS)                                                     \
  static void assign_##BITS (void *op, uint##BITS##_t *lhs,                    \
			     uint##BITS##_t *rhs, size_t)                      \
  {                                                                            \
    *lhs                                                                       \
      = ((uint##BITS##_t (*) (uint##BITS##_t *, uint##BITS##_t *)) op) (lhs,   \
									rhs);  \
  }                                                                            \
  static void assign_by_val_##BITS (void *op, uint##BITS##_t *lhs,             \
				    uint##BITS##_t *rhs, size_t)               \
  {                                                                            \
    *lhs = ((uint##BITS##_t (*) (uint##BITS##_t, uint##BITS##_t)) op) (*lhs,   \
								       *rhs);  \
  }

GEN_FOR_BITS (8)
GEN_FOR_BITS (16)
GEN_FOR_BITS (32)
GEN_FOR_BITS (64)
// GEN_FOR_BITS (128)

static void
assign_float (void *op, float *lhs, float *rhs, size_t)
{
  *lhs = ((float (*) (float *, float *)) op) (lhs, rhs);
}

static void
assign_double (void *op, double *lhs, double *rhs, size_t)
{
  *lhs = ((double (*) (double *, double *)) op) (lhs, rhs);
}

static void
assign_var (red_op op, void *lhs, void *rhs, size_t sz)
{
  memcpy (lhs, op (lhs, rhs), sz);
}

static void
assign_char (void *op, void *lhs, void *rhs, size_t sz)
{
  ((void (*) (char *, size_t, char *, char *, size_t,
	      size_t)) op) (lhs, sz, lhs, rhs, sz, sz);
}

static ass_op
gen_reduction (const int type, const size_t sz, const int flags)
{
  const bool by_val = flags & GFC_CAF_ARG_VALUE;
  switch (type)
    {
    case BT_CHARACTER:
      return (ass_op) assign_char;
    case BT_REAL:
      switch (sz)
	{
	case 4:
	  return (ass_op) assign_float;
	case 8:
	  return (ass_op) assign_double;
	default:
	  return assign_var;
	}
    default:
      switch (sz)
	{
	case 1:
	  return (ass_op) (by_val ? assign_by_val_8 : assign_8);
	case 2:
	  return (ass_op) (by_val ? assign_by_val_16 : assign_16);
	case 4:
	  return (ass_op) (by_val ? assign_by_val_32 : assign_32);
	case 8:
	  return (ass_op) (by_val ? assign_by_val_64 : assign_64);
	// case 16:
	//   return assign_128;
	default:
	  return assign_var;
	}
    }
}

/* Having result_image == -1 means allreduce.  */

void
collsub_reduce_array (gfc_descriptor_t *desc, int result_image,
		      void *(*op) (void *, void *), int opr_flags,
		      int str_len __attribute__ ((unused)))
{
  void *buffer;
  pack_info pi;
  bool packed;
  int cbit = 0;
  int imoffset;
  index_type elem_size;
  index_type this_image_size_bytes;
  void *this_image_buf, *roll_iter, *src_iter;
  ass_op assign;
  const int this_img_id = caf_current_team->index;

  packed = pack_array_prepare (&pi, desc);
  if (pi.num_elem == 0)
    return;

  elem_size = GFC_DESCRIPTOR_SPAN (desc);
  this_image_size_bytes = elem_size * pi.num_elem;

  buffer = get_collsub_buf (
    this_image_size_bytes * caf_current_team->u.image_info->image_count.count);
  this_image_buf = buffer + this_image_size_bytes * this_img_id;

  if (packed)
    memcpy (this_image_buf, GFC_DESCRIPTOR_DATA (desc), this_image_size_bytes);
  else
    pack_array_finish (&pi, desc, this_image_buf);

  assign = gen_reduction (GFC_DESCRIPTOR_TYPE (desc), elem_size, opr_flags);
  collsub_sync ();

  for (; ((this_img_id >> cbit) & 1) == 0
	 && (caf_current_team->u.image_info->image_count.count >> cbit) != 0;
       cbit++)
    {
      imoffset = 1 << cbit;
      if (this_img_id + imoffset
	  < caf_current_team->u.image_info->image_count.count)
	{
	  /* Reduce arrays elementwise.  */
	  roll_iter = this_image_buf;
	  src_iter = this_image_buf + this_image_size_bytes * imoffset;
	  for (ssize_t i = 0; i < pi.num_elem;
	       ++i, roll_iter += elem_size, src_iter += elem_size)
	    assign (op, roll_iter, src_iter, elem_size);
	}
      collsub_sync ();
    }
  for (; (caf_current_team->u.image_info->image_count.count >> cbit) != 0;
       cbit++)
    collsub_sync ();

  if (result_image < 0 || result_image == this_image.image_num)
    {
      if (packed)
	memcpy (GFC_DESCRIPTOR_DATA (desc), buffer, this_image_size_bytes);
      else
	unpack_array_finish (&pi, desc, buffer);
    }

  collsub_sync ();
}

/* Do not use sync_all(), because the program should deadlock in the case that
 * some images are on a sync_all barrier while others are in a collective
 * subroutine.  */

void
collsub_broadcast_array (gfc_descriptor_t *desc, int source_image)
{
  void *buffer;
  pack_info pi;
  bool packed;
  index_type elem_size;
  index_type size_bytes;

  packed = pack_array_prepare (&pi, desc);
  if (pi.num_elem == 0)
    return;

  if (GFC_DESCRIPTOR_TYPE (desc) == BT_CHARACTER)
    {
      if (GFC_DESCRIPTOR_SIZE (desc))
	elem_size = GFC_DESCRIPTOR_SIZE (desc);
      else
	elem_size = strlen (desc->base_addr);
    }
  else
    elem_size = GFC_DESCRIPTOR_SPAN (desc) != 0
		  ? ((index_type) GFC_DESCRIPTOR_SPAN (desc))
		  : ((index_type) GFC_DESCRIPTOR_SIZE (desc));
  size_bytes = elem_size * pi.num_elem;
  buffer = get_collsub_buf (size_bytes);

  if (source_image == this_image.image_num)
    {
      if (packed)
	memcpy (buffer, GFC_DESCRIPTOR_DATA (desc), size_bytes);
      else
	pack_array_finish (&pi, desc, buffer);
      collsub_sync ();
    }
  else
    {
      collsub_sync ();
      if (packed)
	memcpy (GFC_DESCRIPTOR_DATA (desc), buffer, size_bytes);
      else
	unpack_array_finish (&pi, desc, buffer);
    }

  collsub_sync ();
}
