/* Copyright (C) 2009, 2011 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#ifndef LIBITM_ALPHA_CACHELINE_H
#define LIBITM_ALPHA_CACHELINE_H 1

// A cacheline is the smallest unit with which locks are associated.
// The current implementation of the _ITM_[RW] barriers assumes that
// all data types can fit (aligned) within a cachline, which means
// in practice sizeof(complex long double) is the smallest cacheline size.
// It ought to be small enough for efficient manipulation of the
// modification mask, below.
#define CACHELINE_SIZE 64

#ifdef __alpha_bwx__
# include "config/generic/cacheline.h"
#else
// If we don't have byte-word stores, then we'll never be able to
// adjust *all* of the byte loads/stores to be truely atomic.  So
// only guarantee 4-byte aligned values atomicly stored, exactly
// like the native system.  Use byte zap instructions to accelerate
// sub-word masked stores.

namespace GTM HIDDEN {

// A gtm_cacheline_mask stores a modified bit for every modified byte
// in the cacheline with which it is associated.
typedef sized_integral<CACHELINE_SIZE / 8>::type gtm_cacheline_mask;

union gtm_cacheline
{
  // Byte access to the cacheline.
  unsigned char b[CACHELINE_SIZE] __attribute__((aligned(CACHELINE_SIZE)));

  // Larger sized access to the cacheline.
  uint16_t u16[CACHELINE_SIZE / sizeof(uint16_t)];
  uint32_t u32[CACHELINE_SIZE / sizeof(uint32_t)];
  uint64_t u64[CACHELINE_SIZE / sizeof(uint64_t)];
  gtm_word w[CACHELINE_SIZE / sizeof(gtm_word)];

  // Store S into D, but only the bytes specified by M.
  static void store_mask(uint32_t *d, uint32_t s, uint8_t m);
  static void store_mask(uint64_t *d, uint64_t s, uint8_t m);

  // Copy S to D, but only the bytes specified by M.
  static void copy_mask (gtm_cacheline * __restrict d,
			 const gtm_cacheline * __restrict s,
			 gtm_cacheline_mask m);

  // A write barrier to emit after (a series of) copy_mask.
  static void copy_mask_wb () { atomic_write_barrier(); }
};

inline void ALWAYS_INLINE
gtm_cacheline::store_mask (uint32_t *d, uint32_t s, uint8_t m)
{
  const uint8_t tm = (1 << sizeof(uint32_t)) - 1;

  m &= tm;
  if (__builtin_expect (m, tm))
    {
      if (__builtin_expect (m == tm, 1))
	*d = s;
      else
	*d = __builtin_alpha_zap (*d, m) | __builtin_alpha_zapnot (s, m);
    }
}

inline void ALWAYS_INLINE
gtm_cacheline::store_mask (uint64_t *d, uint64_t s, uint8_t m)
{
  if (__builtin_expect (m, 0xff))
    {
      if (__builtin_expect (m == 0xff, 1))
	*d = s;
      else
	{
	  typedef uint32_t *p32 __attribute__((may_alias));
	  p32 d32 = reinterpret_cast<p32>(d);

	  if ((m & 0x0f) == 0x0f)
	    {
	      d32[0] = s;
	      m &= 0xf0;
	    }
	  else if ((m & 0xf0) == 0xf0)
	    {
	      d32[1] = s >> 32;
	      m &= 0x0f;
	    }

	  if (m)
	    *d = __builtin_alpha_zap (*d, m) | __builtin_alpha_zapnot (s, m);
	}
    }
}

} // namespace GTM

#endif // __alpha_bwx__
#endif // LIBITM_ALPHA_CACHELINE_H
