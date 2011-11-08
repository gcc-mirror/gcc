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

#ifndef LIBITM_CACHELINE_H
#define LIBITM_CACHELINE_H 1

namespace GTM HIDDEN {

// A cacheline is the smallest unit with which locks are associated.
// The current implementation of the _ITM_[RW] barriers assumes that
// all data types can fit (aligned) within a cachline, which means
// in practice sizeof(complex long double) is the smallest cacheline size.
// It ought to be small enough for efficient manipulation of the
// modification mask, below.
#ifndef CACHELINE_SIZE
# define CACHELINE_SIZE 32
#endif

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
  template<typename T> static void store_mask (T *d, T s, uint8_t m);

  // Copy S to D, but only the bytes specified by M.
  static void copy_mask (gtm_cacheline * __restrict d,
			 const gtm_cacheline * __restrict s,
			 gtm_cacheline_mask m);

  // A write barrier to emit after (a series of) copy_mask.
  // When we're emitting non-temporal stores, the normal strong
  // ordering of the machine doesn't apply.
  static void copy_mask_wb () { atomic_write_barrier(); }
};

template<typename T>
inline void
gtm_cacheline::store_mask (T *d, T s, uint8_t m)
{
  const uint8_t tm = (1 << sizeof(T)) - 1;

  if (__builtin_expect (m & tm, tm))
    {
      if (__builtin_expect ((m & tm) == tm, 1))
	*d = s;
      else
	{
	  const int half = sizeof(T) / 2;
	  typedef typename sized_integral<half>::type half_t;
	  half_t *dhalf = reinterpret_cast<half_t *>(d);
	  half_t s1, s2;

	  if (WORDS_BIGENDIAN)
	    s1 = s >> half*8, s2 = s;
	  else
	    s1 = s, s2 = s >> half*8;

	  store_mask (dhalf, s1, m);
	  store_mask (dhalf + 1, s2, m >> half);
	}
    }
}

template<>
inline void ALWAYS_INLINE
gtm_cacheline::store_mask<uint8_t> (uint8_t *d, uint8_t s, uint8_t m)
{
  if (m & 1)
    *d = s;
}

} // namespace GTM

#endif // LIBITM_CACHELINE_H
