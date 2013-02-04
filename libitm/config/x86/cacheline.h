/* Copyright (C) 2009-2013 Free Software Foundation, Inc.
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

#if defined(__SSE__) || defined(__AVX__)
  // Copy S to D; only bother defining if we can do this more efficiently
  // than the compiler-generated default implementation.
  gtm_cacheline& operator= (const gtm_cacheline &s);
#endif // SSE, AVX
};

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

#undef CP
#undef TYPE
}
#endif

} // namespace GTM

#endif // LIBITM_CACHELINE_H
