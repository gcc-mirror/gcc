/* Copyright (C) 2008-2014 Free Software Foundation, Inc.

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

#include <spu_mfcio.h>
#include <spu_internals.h>
#include <spu_intrinsics.h>
#include <spu_cache.h>

extern unsigned long long __ea_local_store;
extern char __cache_tag_array_size;

#define LINE_SIZE 128
#define TAG_MASK (LINE_SIZE - 1)

#define WAYS 4
#define SET_MASK ((int) &__cache_tag_array_size - LINE_SIZE)

#define CACHE_LINES ((int) &__cache_tag_array_size /		\
		     sizeof (struct __cache_tag_array) * WAYS)

struct __cache_tag_array
{
  unsigned int tag_lo[WAYS];
  unsigned int tag_hi[WAYS];
  void *base[WAYS];
  int reserved[WAYS];
  vector unsigned short dirty_bits[WAYS];
};

extern struct __cache_tag_array __cache_tag_array[];
extern char __cache[];

/* In order to make the code seem a little cleaner, and to avoid having
   64/32 bit ifdefs all over the place, we use macros.  */

#ifdef __EA64__
typedef unsigned long long addr;

#define CHECK_TAG(_entry, _way, _tag)			\
  ((_entry)->tag_lo[(_way)] == ((_tag) & 0xFFFFFFFF)	\
   && (_entry)->tag_hi[(_way)] == ((_tag) >> 32))

#define GET_TAG(_entry, _way) \
  ((unsigned long long)(_entry)->tag_hi[(_way)] << 32	\
   | (unsigned long long)(_entry)->tag_lo[(_way)])

#define SET_TAG(_entry, _way, _tag)			\
  (_entry)->tag_lo[(_way)] = (_tag) & 0xFFFFFFFF;	\
  (_entry)->tag_hi[(_way)] = (_tag) >> 32

#else /*__EA32__*/
typedef unsigned long addr;

#define CHECK_TAG(_entry, _way, _tag)			\
  ((_entry)->tag_lo[(_way)] == (_tag))

#define GET_TAG(_entry, _way)				\
  ((_entry)->tag_lo[(_way)])

#define SET_TAG(_entry, _way, _tag)			\
  (_entry)->tag_lo[(_way)] = (_tag)

#endif

/* In GET_ENTRY, we cast away the high 32 bits,
   as the tag is only in the low 32.  */

#define GET_ENTRY(_addr)						   \
  ((struct __cache_tag_array *)						   \
   si_to_uint (si_a (si_and (si_from_uint ((unsigned int) (addr) (_addr)), \
			     si_from_uint (SET_MASK)),			   \
	       si_from_uint ((unsigned int) __cache_tag_array))))

#define GET_CACHE_LINE(_addr, _way) \
  ((void *) (__cache + ((_addr) & SET_MASK) * WAYS) + ((_way) * LINE_SIZE));

#define CHECK_DIRTY(_vec) (si_to_uint (si_orx ((qword) (_vec))))
#define SET_EMPTY(_entry, _way) ((_entry)->tag_lo[(_way)] = 1)
#define CHECK_EMPTY(_entry, _way) ((_entry)->tag_lo[(_way)] == 1)

#define LS_FLAG 0x80000000
#define SET_IS_LS(_entry, _way) ((_entry)->reserved[(_way)] |= LS_FLAG)
#define CHECK_IS_LS(_entry, _way) ((_entry)->reserved[(_way)] & LS_FLAG)
#define GET_LRU(_entry, _way) ((_entry)->reserved[(_way)] & ~LS_FLAG)

static int dma_tag = 32;

static void
__cache_evict_entry (struct __cache_tag_array *entry, int way)
{
  addr tag = GET_TAG (entry, way);

  if (CHECK_DIRTY (entry->dirty_bits[way]) && !CHECK_IS_LS (entry, way))
    {
#ifdef NONATOMIC
      /* Non-atomic writes.  */
      unsigned int oldmask, mach_stat;
      char *line = ((void *) 0);

      /* Enter critical section.  */
      mach_stat = spu_readch (SPU_RdMachStat);
      spu_idisable ();

      /* Issue DMA request.  */
      line = GET_CACHE_LINE (entry->tag_lo[way], way);
      mfc_put (line, tag, LINE_SIZE, dma_tag, 0, 0);

      /* Wait for DMA completion.  */
      oldmask = mfc_read_tag_mask ();
      mfc_write_tag_mask (1 << dma_tag);
      mfc_read_tag_status_all ();
      mfc_write_tag_mask (oldmask);

      /* Leave critical section.  */
      if (__builtin_expect (mach_stat & 1, 0))
	spu_ienable ();
#else
      /* Allocate a buffer large enough that we know it has 128 bytes
         that are 128 byte aligned (for DMA). */

      char buffer[LINE_SIZE + 127];
      qword *buf_ptr = (qword *) (((unsigned int) (buffer) + 127) & ~127);
      qword *line = GET_CACHE_LINE (entry->tag_lo[way], way);
      qword bits;
      unsigned int mach_stat;

      /* Enter critical section.  */
      mach_stat = spu_readch (SPU_RdMachStat);
      spu_idisable ();

      do
	{
	  /* We atomically read the current memory into a buffer
	     modify the dirty bytes in the buffer, and write it
	     back. If writeback fails, loop and try again.  */

	  mfc_getllar (buf_ptr, tag, 0, 0);
	  mfc_read_atomic_status ();

	  /* The method we're using to write 16 dirty bytes into
	     the buffer at a time uses fsmb which in turn uses
	     the least significant 16 bits of word 0, so we
	     load the bits and rotate so that the first bit of
	     the bitmap is in the first bit that fsmb will use.  */

	  bits = (qword) entry->dirty_bits[way];
	  bits = si_rotqbyi (bits, -2);

	  /* Si_fsmb creates the mask of dirty bytes.
	     Use selb to nab the appropriate bits.  */
	  buf_ptr[0] = si_selb (buf_ptr[0], line[0], si_fsmb (bits));

	  /* Rotate to next 16 byte section of cache.  */
	  bits = si_rotqbyi (bits, 2);

	  buf_ptr[1] = si_selb (buf_ptr[1], line[1], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[2] = si_selb (buf_ptr[2], line[2], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[3] = si_selb (buf_ptr[3], line[3], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[4] = si_selb (buf_ptr[4], line[4], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[5] = si_selb (buf_ptr[5], line[5], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[6] = si_selb (buf_ptr[6], line[6], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);
	  buf_ptr[7] = si_selb (buf_ptr[7], line[7], si_fsmb (bits));
	  bits = si_rotqbyi (bits, 2);

	  mfc_putllc (buf_ptr, tag, 0, 0);
	}
      while (mfc_read_atomic_status ());

      /* Leave critical section.  */
      if (__builtin_expect (mach_stat & 1, 0))
	spu_ienable ();
#endif
    }

  /* In any case, marking the lo tag with 1 which denotes empty.  */
  SET_EMPTY (entry, way);
  entry->dirty_bits[way] = (vector unsigned short) si_from_uint (0);
}

void
__cache_evict (__ea void *ea)
{
  addr tag = (addr) ea & ~TAG_MASK;
  struct __cache_tag_array *entry = GET_ENTRY (ea);
  int i = 0;

  /* Cycles through all the possible ways an address could be at
     and evicts the way if found.  */

  for (i = 0; i < WAYS; i++)
    if (CHECK_TAG (entry, i, tag))
      __cache_evict_entry (entry, i);
}

static void *
__cache_fill (int way, addr tag)
{
  unsigned int oldmask, mach_stat;
  char *line = ((void *) 0);

  /* Reserve our DMA tag.  */
  if (dma_tag == 32)
    dma_tag = mfc_tag_reserve ();

  /* Enter critical section.  */
  mach_stat = spu_readch (SPU_RdMachStat);
  spu_idisable ();

  /* Issue DMA request.  */
  line = GET_CACHE_LINE (tag, way);
  mfc_get (line, tag, LINE_SIZE, dma_tag, 0, 0);

  /* Wait for DMA completion.  */
  oldmask = mfc_read_tag_mask ();
  mfc_write_tag_mask (1 << dma_tag);
  mfc_read_tag_status_all ();
  mfc_write_tag_mask (oldmask);

  /* Leave critical section.  */
  if (__builtin_expect (mach_stat & 1, 0))
    spu_ienable ();

  return (void *) line;
}

static void
__cache_miss (__ea void *ea, struct __cache_tag_array *entry, int way)
{

  addr tag = (addr) ea & ~TAG_MASK;
  unsigned int lru = 0;
  int i = 0;
  int idx = 0;

  /* If way > 4, then there are no empty slots, so we must evict
     the least recently used entry. */
  if (way >= 4)
    {
      for (i = 0; i < WAYS; i++)
	{
	  if (GET_LRU (entry, i) > lru)
	    {
	      lru = GET_LRU (entry, i);
	      idx = i;
	    }
	}
      __cache_evict_entry (entry, idx);
      way = idx;
    }

  /* Set the empty entry's tag and fill it's cache line. */

  SET_TAG (entry, way, tag);
  entry->reserved[way] = 0;

  /* Check if the address is just an effective address within the
     SPU's local store. */

  /* Because the LS is not 256k aligned, we can't do a nice and mask
     here to compare, so we must check the whole range.  */

  if ((addr) ea >= (addr) __ea_local_store
      && (addr) ea < (addr) (__ea_local_store + 0x40000))
    {
      SET_IS_LS (entry, way);
      entry->base[way] =
	(void *) ((unsigned int) ((addr) ea -
				  (addr) __ea_local_store) & ~0x7f);
    }
  else
    {
      entry->base[way] = __cache_fill (way, tag);
    }
}

void *
__cache_fetch_dirty (__ea void *ea, int n_bytes_dirty)
{
#ifdef __EA64__
  unsigned int tag_hi;
  qword etag_hi;
#endif
  unsigned int tag_lo;
  struct __cache_tag_array *entry;

  qword etag_lo;
  qword equal;
  qword bit_mask;
  qword way;

  /* This first chunk, we merely fill the pointer and tag.  */

  entry = GET_ENTRY (ea);

#ifndef __EA64__
  tag_lo =
    si_to_uint (si_andc
		(si_shufb
		 (si_from_uint ((addr) ea), si_from_uint (0),
		  si_from_uint (0x00010203)), si_from_uint (TAG_MASK)));
#else
  tag_lo =
    si_to_uint (si_andc
		(si_shufb
		 (si_from_ullong ((addr) ea), si_from_uint (0),
		  si_from_uint (0x04050607)), si_from_uint (TAG_MASK)));

  tag_hi =
    si_to_uint (si_shufb
		(si_from_ullong ((addr) ea), si_from_uint (0),
		 si_from_uint (0x00010203)));
#endif

  /* Increment LRU in reserved bytes.  */
  si_stqd (si_ai (si_lqd (si_from_ptr (entry), 48), 1),
	   si_from_ptr (entry), 48);

missreturn:
  /* Check if the entry's lo_tag is equal to the address' lo_tag.  */
  etag_lo = si_lqd (si_from_ptr (entry), 0);
  equal = si_ceq (etag_lo, si_from_uint (tag_lo));
#ifdef __EA64__
  /* And the high tag too.  */
  etag_hi = si_lqd (si_from_ptr (entry), 16);
  equal = si_and (equal, (si_ceq (etag_hi, si_from_uint (tag_hi))));
#endif

  if ((si_to_uint (si_orx (equal)) == 0))
    goto misshandler;

  if (n_bytes_dirty)
    {
      /* way = 0x40,0x50,0x60,0x70 for each way, which is also the
         offset of the appropriate dirty bits.  */
      way = si_shli (si_clz (si_gbb (equal)), 2);

      /* To create the bit_mask, we set it to all 1s (uint -1), then we
         shift it over (128 - n_bytes_dirty) times.  */

      bit_mask = si_from_uint (-1);

      bit_mask =
	si_shlqby (bit_mask, si_from_uint ((LINE_SIZE - n_bytes_dirty) / 8));

      bit_mask =
	si_shlqbi (bit_mask, si_from_uint ((LINE_SIZE - n_bytes_dirty) % 8));

      /* Rotate it around to the correct offset.  */
      bit_mask =
	si_rotqby (bit_mask,
		   si_from_uint (-1 * ((addr) ea & TAG_MASK) / 8));

      bit_mask =
	si_rotqbi (bit_mask,
		   si_from_uint (-1 * ((addr) ea & TAG_MASK) % 8));

      /* Update the dirty bits.  */
      si_stqx (si_or (si_lqx (si_from_ptr (entry), way), bit_mask),
	       si_from_ptr (entry), way);
    };

  /* We've definitely found the right entry, set LRU (reserved) to 0
     maintaining the LS flag (MSB).  */

  si_stqd (si_andc
	   (si_lqd (si_from_ptr (entry), 48),
	    si_and (equal, si_from_uint (~(LS_FLAG)))),
	   si_from_ptr (entry), 48);

  return (void *)
    si_to_uint (si_a
		(si_orx
		 (si_and (si_lqd (si_from_ptr (entry), 32), equal)),
		 si_from_uint (((unsigned int) (addr) ea) & TAG_MASK)));

misshandler:
  equal = si_ceqi (etag_lo, 1);
  __cache_miss (ea, entry, (si_to_uint (si_clz (si_gbb (equal))) - 16) >> 2);
  goto missreturn;
}

void *
__cache_fetch (__ea void *ea)
{
  return __cache_fetch_dirty (ea, 0);
}

void
__cache_touch (__ea void *ea __attribute__ ((unused)))
{
  /* NO-OP for now.  */
}

void __cache_flush (void) __attribute__ ((destructor));
void
__cache_flush (void)
{
  struct __cache_tag_array *entry = __cache_tag_array;
  unsigned int i;
  int j;

  /* Cycle through each cache entry and evict all used ways.  */

  for (i = 0; i < CACHE_LINES / WAYS; i++)
    {
      for (j = 0; j < WAYS; j++)
	if (!CHECK_EMPTY (entry, j))
	  __cache_evict_entry (entry, j);

      entry++;
    }
}
