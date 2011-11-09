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

// Minimum cacheline size is 32, due to both complex long double and __m256.
// There's no requirement that 64-bit use a 64-byte cacheline size, but do
// so for now to make sure everything is parameterized properly.
#ifdef __x86_64__
# define CACHELINE_SIZE 64
#else
# define CACHELINE_SIZE 32
#endif

namespace GTM HIDDEN {

// A gtm_cacheline_mask stores a modified bit for every modified byte
// in the cacheline with which it is associated.
typedef sized_integral<CACHELINE_SIZE / 8>::type gtm_cacheline_mask;

extern uint32_t const gtm_bit_to_byte_mask[16];

union gtm_cacheline
{
  // Byte access to the cacheline.
  unsigned char b[CACHELINE_SIZE] __attribute__((aligned(CACHELINE_SIZE)));

  // Larger sized access to the cacheline.
  uint16_t u16[CACHELINE_SIZE / sizeof(uint16_t)];
  uint32_t u32[CACHELINE_SIZE / sizeof(uint32_t)];
  uint64_t u64[CACHELINE_SIZE / sizeof(uint64_t)];
  gtm_word w[CACHELINE_SIZE / sizeof(gtm_word)];

#ifdef __MMX__
  __m64 m64[CACHELINE_SIZE / sizeof(__m64)];
#endif
#ifdef __SSE__
  __m128 m128[CACHELINE_SIZE / sizeof(__m128)];
#endif
#ifdef __SSE2__
  __m128i m128i[CACHELINE_SIZE / sizeof(__m128i)];
#endif
#ifdef __AVX__
  __m256 m256[CACHELINE_SIZE / sizeof(__m256)];
  __m256i m256i[CACHELINE_SIZE / sizeof(__m256i)];
#endif

  // Store S into D, but only the bytes specified by M.
  static void store_mask (uint32_t *d, uint32_t s, uint8_t m);
  static void store_mask (uint64_t *d, uint64_t s, uint8_t m);
#ifdef __SSE2__
  static void store_mask (__m128i *d, __m128i s, uint16_t m);
#endif

  // Copy S to D, but only the bytes specified by M.
  static void copy_mask (gtm_cacheline * __restrict d,
			 const gtm_cacheline * __restrict s,
			 gtm_cacheline_mask m);

  // A write barrier to emit after (a series of) copy_mask.
  // When we're emitting non-temporal stores, the normal strong
  // ordering of the machine doesn't apply.
  static void copy_mask_wb ();

#if defined(__SSE__) || defined(__AVX__)
  // Copy S to D; only bother defining if we can do this more efficiently
  // than the compiler-generated default implementation.
  gtm_cacheline& operator= (const gtm_cacheline &s);
#endif // SSE, AVX
};

inline void
gtm_cacheline::copy_mask_wb ()
{
#ifdef __SSE2__
  _mm_sfence ();
#endif
}

#if defined(__SSE__) || defined(__AVX__)
inline gtm_cacheline& ALWAYS_INLINE
gtm_cacheline::operator= (const gtm_cacheline & __restrict s)
{
#ifdef __AVX__
# define CP	m256
# define TYPE	__m256
#else
# define CP	m128
# define TYPE	__m128
#endif

  TYPE w, x, y, z;

  // ??? Wouldn't it be nice to have a pragma to tell the compiler
  // to completely unroll a given loop?
  switch (CACHELINE_SIZE / sizeof(TYPE))
    {
    case 1:
      this->CP[0] = s.CP[0];
      break;
    case 2:
      x = s.CP[0];
      y = s.CP[1];
      this->CP[0] = x;
      this->CP[1] = y;
      break;
    case 4:
      w = s.CP[0];
      x = s.CP[1];
      y = s.CP[2];
      z = s.CP[3];
      this->CP[0] = w;
      this->CP[1] = x;
      this->CP[2] = y;
      this->CP[3] = z;
      break;
    default:
      __builtin_trap ();
    }

  return *this;
}
#endif

// Support masked integer stores more efficiently with an unlocked cmpxchg
// insn.  My reasoning is that while we write to locations that we do not wish
// to modify, we do it in an uninterruptable insn, and so we either truely
// write back the original data or the insn fails -- unlike with a
// load/and/or/write sequence which can be interrupted either by a kernel
// task switch or an unlucky cacheline steal by another processor.  Avoiding
// the LOCK prefix improves performance by a factor of 10, and we don't need
// the memory barrier semantics implied by that prefix.

inline void ALWAYS_INLINE
gtm_cacheline::store_mask (uint32_t *d, uint32_t s, uint8_t m)
{
  gtm_cacheline_mask tm = (1 << sizeof (s)) - 1;
  if (__builtin_expect (m & tm, tm))
    {
      if (__builtin_expect ((m & tm) == tm, 1))
	*d = s;
      else
	{
	  gtm_cacheline_mask bm = gtm_bit_to_byte_mask[m & 15];
	  gtm_word n, o = *d;

	  __asm("\n0:\t"
		"mov	%[o], %[n]\n\t"
		"and	%[m], %[n]\n\t"
		"or	%[s], %[n]\n\t"
		"cmpxchg %[n], %[d]\n\t"
		".byte	0x2e\n\t"	// predict not-taken, aka jnz,pn
		"jnz	0b"
		: [d] "+m"(*d), [n] "=&r" (n), [o] "+a"(o)
		: [s] "r" (s & bm), [m] "r" (~bm));
	}
    }
}

inline void ALWAYS_INLINE
gtm_cacheline::store_mask (uint64_t *d, uint64_t s, uint8_t m)
{
  gtm_cacheline_mask tm = (1 << sizeof (s)) - 1;
  if (__builtin_expect (m & tm, tm))
    {
      if (__builtin_expect ((m & tm) == tm, 1))
	*d = s;
      else
	{
#ifdef __x86_64__
	  uint32_t bl = gtm_bit_to_byte_mask[m & 15];
	  uint32_t bh = gtm_bit_to_byte_mask[(m >> 4) & 15];
	  gtm_cacheline_mask bm = bl | ((gtm_cacheline_mask)bh << 31 << 1);
	  uint64_t n, o = *d;
	  __asm("\n0:\t"
		"mov	%[o], %[n]\n\t"
		"and	%[m], %[n]\n\t"
		"or	%[s], %[n]\n\t"
		"cmpxchg %[n], %[d]\n\t"
		".byte	0x2e\n\t"	// predict not-taken, aka jnz,pn
		"jnz	0b"
		: [d] "+m"(*d), [n] "=&r" (n), [o] "+a"(o)
		: [s] "r" (s & bm), [m] "r" (~bm));
#else
	  /* ??? While it's possible to perform this operation with
	     cmpxchg8b, the sequence requires all 7 general registers
	     and thus cannot be performed with -fPIC.  Don't even try.  */
	  uint32_t *d32 = reinterpret_cast<uint32_t *>(d);
	  store_mask (d32, s, m);
	  store_mask (d32 + 1, s >> 32, m >> 4);
#endif
	}
    }
}

#ifdef __SSE2__
inline void ALWAYS_INLINE
gtm_cacheline::store_mask (__m128i *d, __m128i s, uint16_t m)
{
  if (__builtin_expect (m == 0, 0))
    return;
  if (__builtin_expect (m == 0xffff, 1))
    *d = s;
  else
    {
      __m128i bm0, bm1, bm2, bm3;
      bm0 = _mm_set_epi32 (0, 0, 0, gtm_bit_to_byte_mask[m & 15]); m >>= 4;
      bm1 = _mm_set_epi32 (0, 0, 0, gtm_bit_to_byte_mask[m & 15]); m >>= 4;
      bm2 = _mm_set_epi32 (0, 0, 0, gtm_bit_to_byte_mask[m & 15]); m >>= 4;
      bm3 = _mm_set_epi32 (0, 0, 0, gtm_bit_to_byte_mask[m & 15]); m >>= 4;
      bm0 = _mm_unpacklo_epi32 (bm0, bm1);
      bm2 = _mm_unpacklo_epi32 (bm2, bm3);
      bm0 = _mm_unpacklo_epi64 (bm0, bm2);

      _mm_maskmoveu_si128 (s, bm0, (char *)d);
    }
}
#endif // SSE2

} // namespace GTM

#endif // LIBITM_CACHELINE_H
