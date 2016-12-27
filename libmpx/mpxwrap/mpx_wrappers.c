/* MPX Wrappers Library
   Copyright (C) 2014 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "stdlib.h"
#include "string.h"
#include <sys/mman.h>
#include <stdint.h>
#include <assert.h>
#include "mpxrt/mpxrt.h"

/* Since internal MPX wrapper calls must avoid PLT which will clear bound
   registers, we make them static with an external alias.  */
#define EXTERN_ALIAS(f) \
  __typeof (f) __##f __attribute__((alias(#f)));

static void *
mpx_wrapper_malloc (size_t size)
{
  void *p = (void *)malloc (size);
  if (!p) return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, size);
}

EXTERN_ALIAS (mpx_wrapper_malloc)

void *
__mpx_wrapper_mmap (void *addr, size_t length, int prot, int flags,
		    int fd, off_t offset)
{
  void *p = mmap (addr, length, prot, flags, fd, offset);
  if (!p) return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, length);
}

void *
__mpx_wrapper_realloc (void *ptr, size_t n)
{
  if (!ptr)
    return mpx_wrapper_malloc (n);

  /* We don't kwnow how much data is copied by realloc
     and therefore may check only lower bounds.  */
  __bnd_chk_ptr_lbounds (ptr);
  ptr = realloc (ptr, n);

  if (!ptr)
    return __bnd_null_ptr_bounds (ptr);

  return __bnd_set_ptr_bounds (ptr, n);
}

void *
__mpx_wrapper_calloc (size_t n_elements, size_t element_size)
{
  void *p = calloc (n_elements, element_size);
  if (!p)
    return __bnd_null_ptr_bounds (p);
  return __bnd_set_ptr_bounds (p, n_elements * element_size);
}

static void *
mpx_wrapper_memset (void *dstpp, int c, size_t len)
{
  if (len > 0)
    {
      __bnd_chk_ptr_bounds (dstpp, len);
      memset (dstpp, c, len);
    }
  return dstpp;
}

EXTERN_ALIAS (mpx_wrapper_memset)

void
__mpx_wrapper_bzero (void *dst, size_t len)
{
  mpx_wrapper_memset (dst, 0, len);
}

/* The mpx_pointer type is used for getting bits
   for bt_index (index in bounds table) and
   bd_index (index in bounds directory).  */
typedef union
{
  struct
  {
    unsigned long ignored:NUM_IGN_BITS;
    unsigned long l2entry:NUM_L2_BITS;
    unsigned long l1index:NUM_L1_BITS;
  };
  void *pointer;
} mpx_pointer;

/* The mpx_bt_entry struct represents a cell in bounds table.
   lb is the lower bound, ub is the upper bound,
   p is the stored pointer.  */
struct mpx_bt_entry
{
  void *lb;
  void *ub;
  void *p;
  void *reserved;
};

/* A special type for bd is needed because bt addresses can be modified.  */
typedef struct mpx_bt_entry * volatile * bd_type;

/* Function alloc_bt is used for allocating bounds table
   for the destination pointers if we don't have one.
   We generate a bounds store for some pointer belonging
   to that table and kernel allocates the table for us.  */
static inline void __attribute__ ((bnd_legacy))
alloc_bt (void *ptr)
{
  __asm__ __volatile__ ("bndstx %%bnd0, (%0,%0)"::"r" (ptr):"%bnd0");
}

/* get_bt returns address of bounds table that should
   exist at BD[BD_INDEX].  If there is no address or the address is not valid,
   we try to allocate a valid table.
   If we succeed in getting bt, its address will be returned.
   If we can't get a valid bt, NULL will be returned.  */
__attribute__ ((bnd_legacy)) static inline struct mpx_bt_entry *
get_bt (unsigned bd_index, bd_type bd)
{
  struct mpx_bt_entry *bt = (struct mpx_bt_entry *) ((uintptr_t) bd[bd_index]
                            & MPX_L2_ADDR_MASK);
  if (!(bt) || !((uintptr_t) bd[bd_index] & MPX_L2_VALID_MASK))
    {
      mpx_pointer ptr;
      ptr.l1index = bd_index;
      /* If we don't have BT, allocate it.  */
      alloc_bt (ptr.pointer);
      bt = (struct mpx_bt_entry *) ((uintptr_t) bd[bd_index]
            & MPX_L2_ADDR_MASK);
      if (!(bt) || !((uintptr_t) bd[bd_index] & MPX_L2_VALID_MASK))
    return NULL;
    }
  return bt;
}

/* Function copy_if_possible moves elements from *FROM to *TO.
   If ELEMS is less then the ELEMS_TO_COPY (elements we can copy),
   it copies ELEMS elements and returns 0.
   Otherwise, it copies ELEMS_TO_COPY elements and returns 1.  */
__attribute__ ((bnd_legacy)) static inline int
copy_if_possible (int elems, int elems_to_copy, struct mpx_bt_entry *from,
                  struct mpx_bt_entry *to)
{
  if (elems < elems_to_copy)
    memmove (to, from, elems * sizeof (struct mpx_bt_entry));
  else
    {
      memmove (to, from, elems_to_copy * sizeof (struct mpx_bt_entry));
      return 1;
    }
  return 0;
}

/* Function copy_if_possible_from_end moves elements ending at *SRC_END
   to the place where they will end at *DST_END.
   If ELEMS is less then the ELEMS_TO_COPY (elements we can copy),
   function copies ELEMS elements and returns 0.
   Otherwise, it copies ELEMS_TO_COPY elements and returns 1.  */
__attribute__ ((bnd_legacy)) static inline int
copy_if_possible_from_end (int elems, int elems_to_copy, struct mpx_bt_entry
                           *src_end, struct mpx_bt_entry *dst_end)
{
  if (elems < elems_to_copy)
    memmove (dst_end - elems, src_end - elems,
             elems * sizeof (struct mpx_bt_entry));
  else
    {
      memmove (dst_end - elems_to_copy,
           src_end - elems_to_copy,
           elems_to_copy * sizeof (struct mpx_bt_entry));
      return 1;
    }
  return 0;
}

/* move_bounds function copies bounds for N bytes from bt of SRC to bt of DST.
   It also copies bounds for all pointers inside.
   There are 3 parts of the algorithm:
   1) We copy everything till the end of the first bounds table of SRC
   2) In loop we copy whole bound tables till the second-last one
   3) Data in the last bounds table is copied separately, after the loop.
   If one of bound tables in SRC doesn't exist,
   we skip it because there are no pointers.
   Depending on the arrangement of SRC and DST we copy from the beginning
   or from the end.  */
__attribute__ ((bnd_legacy)) static void
move_bounds (void *dst, const void *src, size_t n)
{
  bd_type bd = (bd_type)get_bd ();
  if (!(bd))
    return;

  /* We get indexes for all tables and number of elements for BT.  */
  unsigned long bt_num_of_elems = (1UL << NUM_L2_BITS);
  mpx_pointer addr_src, addr_dst, addr_src_end, addr_dst_end;
  addr_src.pointer = (char *) src;
  addr_dst.pointer = (char *) dst;
  addr_src_end.pointer = (char *) src + n - 1;
  addr_dst_end.pointer = (char *) dst + n - 1;
  unsigned dst_bd_index = addr_dst.l1index;
  unsigned src_bd_index = addr_src.l1index;
  unsigned dst_bt_index = addr_dst.l2entry;
  unsigned src_bt_index = addr_src.l2entry;

  unsigned dst_bd_index_end = addr_dst_end.l1index;
  unsigned src_bd_index_end = addr_src_end.l1index;
  unsigned dst_bt_index_end = addr_dst_end.l2entry;
  unsigned src_bt_index_end = addr_src_end.l2entry;

  int elems_to_copy = src_bt_index_end - src_bt_index + 1 + (src_bd_index_end
                      - src_bd_index) * bt_num_of_elems;
  struct mpx_bt_entry *bt_src, *bt_dst;
  uintptr_t bt_valid;
  /* size1 and size2 will be used to find out what portions
     can be used to copy data.  */
  int size1_elem, size2_elem, size1_bytes, size2_bytes;

  /* Copy from the beginning.  */
  if (((char *) src - (char *) dst) > 0)
    {
      /* Copy everything till the end of the first bounds table (src)  */
      bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index]
                & MPX_L2_ADDR_MASK);
      bt_valid = (uintptr_t) bd[src_bd_index] & MPX_L2_VALID_MASK;

      /* We can copy the whole preliminary piece of data.  */
      if (src_bt_index > dst_bt_index)
        {
          size1_elem = src_bt_index - dst_bt_index;
          size2_elem = bt_num_of_elems - size1_elem;
          size1_bytes = size1_elem * sizeof (struct mpx_bt_entry);
          size2_bytes = size2_elem * sizeof (struct mpx_bt_entry);

          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
            {
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;
              if (copy_if_possible (bt_num_of_elems - src_bt_index,
                  elems_to_copy, &(bt_src[src_bt_index]),
                  &(bt_dst[dst_bt_index])))
                return;
            }
          elems_to_copy -= bt_num_of_elems - src_bt_index;
        }
      /* We have to copy preliminary data in two parts.  */
      else
        {
          size2_elem = dst_bt_index - src_bt_index;
          size1_elem = bt_num_of_elems - size2_elem;
          size1_bytes = size1_elem * sizeof (struct mpx_bt_entry);
          size2_bytes = size2_elem * sizeof (struct mpx_bt_entry);

          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
            {
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;

              if (copy_if_possible (bt_num_of_elems - dst_bt_index,
                  elems_to_copy, &(bt_src[src_bt_index]),
                  &(bt_dst[dst_bt_index])))
                return;
              elems_to_copy -= bt_num_of_elems - dst_bt_index;

              dst_bd_index++;

              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;
              if (copy_if_possible (size2_elem, elems_to_copy,
                  &(bt_src[size1_elem]), &(bt_dst[0])))
                return;
              elems_to_copy -= size2_elem;
            }
          else
            elems_to_copy -= bt_num_of_elems - src_bt_index;
        }
      src_bd_index++;

      /* For each bounds table check if it's valid and move it.  */
      for (; src_bd_index < src_bd_index_end; src_bd_index++)
        {
          bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index]
                    & MPX_L2_ADDR_MASK);
          bt_valid = (uintptr_t) bd[src_bd_index] & MPX_L2_VALID_MASK;

          /* Check we have bounds to copy. */
          if (!bt_src || !bt_valid)
            dst_bd_index++;
          else
            {
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;
              memmove (&(bt_dst[size2_elem]), &(bt_src[0]), size1_bytes);
              dst_bd_index++;
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;
              memmove (&(bt_dst[0]), &(bt_src[size1_elem]), size2_bytes);
            }
          elems_to_copy -= bt_num_of_elems;
        }

      /* Now we have the last page that may be not full
         we copy it separately.  */
      if (elems_to_copy > 0)
        {
          bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index]
                    & MPX_L2_ADDR_MASK);
          bt_valid = (uintptr_t) bd[src_bd_index] & MPX_L2_VALID_MASK;

          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
            {
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;

              if (copy_if_possible (size1_elem, elems_to_copy, &(bt_src[0]),
                  &(bt_dst[size2_elem])))
                return;

              elems_to_copy -= size1_elem;
              dst_bd_index++;
              bt_dst = get_bt (dst_bd_index, bd);
              if (!bt_dst)
                return;
              memmove (&(bt_dst[0]), &(bt_src[size1_elem]),
                       elems_to_copy * sizeof (struct mpx_bt_entry));

            }
        }
    }
  /* Copy from the end.  */
  else
    {
      /* Copy everything till the end of the first bounds table (src)  */
      bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index_end]
                & MPX_L2_ADDR_MASK);
      bt_valid = (uintptr_t) bd[src_bd_index_end] & MPX_L2_VALID_MASK;

      if (src_bt_index_end <= dst_bt_index_end)
      /* We can copy the whole preliminary piece of data.  */
        {
          size2_elem = dst_bt_index_end - src_bt_index_end;
          size1_elem = bt_num_of_elems - size2_elem;
          size1_bytes = size1_elem * sizeof (struct mpx_bt_entry);
          size2_bytes = size2_elem * sizeof (struct mpx_bt_entry);

          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
            {
              bt_dst = get_bt (dst_bd_index_end, bd);
              if (!bt_dst)
                return;

              if (copy_if_possible_from_end (src_bt_index_end + 1,
                  elems_to_copy, &(bt_src[src_bt_index_end + 1]),
                  &(bt_dst[dst_bt_index_end + 1])))
                return;
            }
          elems_to_copy -= src_bt_index_end + 1;
        }
      /* We have to copy preliminary data in two parts.  */
      else
        {
          size1_elem = src_bt_index_end - dst_bt_index_end;
          size2_elem = bt_num_of_elems - size1_elem;
          size1_bytes = size1_elem * sizeof (struct mpx_bt_entry);
          size2_bytes = size2_elem * sizeof (struct mpx_bt_entry);

          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
            {
              bt_dst = get_bt (dst_bd_index_end, bd);
              if (!bt_dst)
                return;
              if (copy_if_possible_from_end (dst_bt_index_end + 1,
                  elems_to_copy, &(bt_src[src_bt_index_end + 1]),
                  &(bt_dst[dst_bt_index_end + 1])))
                return;
              elems_to_copy -= dst_bt_index_end + 1;

              dst_bd_index_end--;

              bt_dst = get_bt (dst_bd_index_end, bd);
              if (!bt_dst)
                return;
              if (copy_if_possible_from_end (size1_elem, elems_to_copy,
                  &(bt_src[size1_elem]), &(bt_dst[bt_num_of_elems])))
                return;

              elems_to_copy -= size1_elem;
            }
          else
            elems_to_copy -= src_bt_index_end + 1;
        }
      /* Go to previous table but beware of overflow.
	 We should have copied all required element
	 in case src_bd_index_end is 0.  */
      if (src_bd_index_end)
	src_bd_index_end--;
      else
	{
	  assert (!elems_to_copy);
	  return;
	}
      /* For each bounds table we check if there are valid pointers inside.
         If there are some, we copy table in pre-counted portions.  */
      for (; src_bd_index_end > src_bd_index; src_bd_index_end--)
        {
          bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index_end]
                    & MPX_L2_ADDR_MASK);
          bt_valid = (uintptr_t) bd[src_bd_index_end] & MPX_L2_VALID_MASK;
          /* Check we have bounds to copy. */
          if (!bt_src || !bt_valid)
            dst_bd_index_end--;
          else
            {
              bt_dst = get_bt (dst_bd_index_end, bd);
              if (!bt_dst)
                return;
              memmove (&(bt_dst[0]), &(bt_src[size1_elem]), size2_bytes);
              dst_bd_index_end--;
              bt_dst = get_bt (dst_bd_index_end, bd);
              if (!bt_dst)
                return;
              memmove (&(bt_dst[size2_elem]), &(bt_src[0]), size1_bytes);
            }
          elems_to_copy -= bt_num_of_elems;
        }

      /* Now we have the last page that may be not full
         we copy it separately.  */
      if (elems_to_copy > 0)
        {
          bt_src = (struct mpx_bt_entry *) ((uintptr_t) bd[src_bd_index_end]
                    & MPX_L2_ADDR_MASK);
          bt_valid = (uintptr_t) bd[src_bd_index_end] & MPX_L2_VALID_MASK;
          /* Check we have bounds to copy. */
          if (bt_src && bt_valid)
          {
            bt_dst = get_bt (dst_bd_index_end, bd);
            if (!bt_dst)
              return;
            if (copy_if_possible_from_end (size2_elem, elems_to_copy,
                &(bt_src[bt_num_of_elems]), &(bt_dst[size2_elem])))
              return;

            elems_to_copy -= size2_elem;
            dst_bd_index_end--;
            bt_dst = get_bt (dst_bd_index_end, bd);
            if (!bt_dst)
              return;
            memmove (&(bt_dst[dst_bt_index]), &(bt_src[src_bt_index]),
                     elems_to_copy * sizeof (struct mpx_bt_entry));
          }
        }
    }
  return;
}

static void *
mpx_wrapper_memmove (void *dst, const void *src, size_t n)
{
  if (n == 0)
    return dst;

  __bnd_chk_ptr_bounds (dst, n);
  __bnd_chk_ptr_bounds (src, n);

  /* When we copy exactly one pointer it is faster to
     just use bndldx + bndstx.  */
  if (n == sizeof (void *))
    {
      void *const *s = (void *const *) src;
      void **d = (void **) dst;
      *d = *s;
      return dst;
    }

  memmove (dst, src, n);

  /* Not necessary to copy bounds if size is less then size of pointer
     or SRC==DST.  */
  if ((n >= sizeof (void *)) && (src != dst))
    move_bounds (dst, src, n);

  return dst;
}

EXTERN_ALIAS (mpx_wrapper_memmove)

static void *
mpx_wrapper_memcpy (void *dst, const void *src, size_t n)
{
  return mpx_wrapper_memmove (dst, src, n);
}

EXTERN_ALIAS (mpx_wrapper_memcpy)

void *
__mpx_wrapper_mempcpy (void *dst, const void *src, size_t n)
{
  return (char *)mpx_wrapper_memcpy (dst, src, n) + n;
}

char *
__mpx_wrapper_strncat (char *dst, const char *src, size_t n)
{
  size_t dst_size = strlen (dst);
  size_t src_size = strnlen (src, n);

  __bnd_chk_ptr_bounds (dst, dst_size + src_size + 1);
  if (src_size < n)
    __bnd_chk_ptr_bounds (src, src_size + 1);
  else
    __bnd_chk_ptr_bounds (src, src_size);

  strncat (dst, src, n);

  return dst;
}

char *
__mpx_wrapper_strcat (char *dst, const char *src)
{
  size_t dst_size = strlen (dst);
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, dst_size + src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  strcat (dst, src);

  return dst;
}

char *
__mpx_wrapper_stpcpy (char *dst, const char *src)
{
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  memcpy (dst, src, src_size + 1);

  return dst + src_size;
}

char *
__mpx_wrapper_stpncpy (char *dst, const char *src, size_t n)
{
  size_t src_size = strnlen (src, n);
  char *res;

  __bnd_chk_ptr_bounds (dst, n);
  if (src_size < n)
    {
      __bnd_chk_ptr_bounds (src, src_size + 1);
      res = dst + src_size;
    }
  else
    {
      __bnd_chk_ptr_bounds (src, src_size);
      res = dst + n;
    }

  memcpy (dst, src, src_size);
  if (n > src_size)
    memset (dst + src_size, 0, n - src_size);

  return res;
}

char *
__mpx_wrapper_strcpy (char *dst, const char *src)
{
  size_t src_size = strlen (src);

  __bnd_chk_ptr_bounds (dst, src_size + 1);
  __bnd_chk_ptr_bounds (src, src_size + 1);

  memcpy (dst, src, src_size + 1);

  return dst;
}

char *
__mpx_wrapper_strncpy (char *dst, const char *src, size_t n)
{
  size_t src_size = strnlen (src, n);

  __bnd_chk_ptr_bounds (dst, n);
  if (src_size < n)
    __bnd_chk_ptr_bounds (src, src_size + 1);
  else
    __bnd_chk_ptr_bounds (src, src_size);

  memcpy (dst, src, src_size);
  if (n > src_size)
    memset (dst + src_size, 0, n - src_size);

  return dst;
}

size_t
__mpx_wrapper_strlen (const char *s)
{
  size_t length = strlen (s);
  __bnd_chk_ptr_bounds (s, length + 1);
  return length;
}
