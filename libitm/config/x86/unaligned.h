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

#ifndef LIBITM_X86_UNALIGNED_H
#define LIBITM_X86_UNALIGNED_H 1

#define HAVE_ARCH_UNALIGNED_LOAD2_U4 1
#define HAVE_ARCH_UNALIGNED_LOAD2_U8 1

#include "config/generic/unaligned.h"

namespace GTM HIDDEN {

template<>
inline uint32_t
unaligned_load2<uint32_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint32_t r, lo, hi;
  lo = c1->u32[CACHELINE_SIZE / sizeof(uint32_t) - 1];
  hi = c2->u32[0];
  asm("shrd %b2, %1, %0" : "=r"(r) : "r"(hi), "c"((ofs & 3) * 8), "0"(lo));
  return r;
}

template<>
inline uint64_t
unaligned_load2<uint64_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
#ifdef __x86_64__
  uint64_t r, lo, hi;
  lo = c1->u64[CACHELINE_SIZE / sizeof(uint64_t) - 1];
  hi = c2->u64[0];
  asm("shrd %b2, %1, %0" : "=r"(r) : "r"(hi), "c"((ofs & 3) * 8), "0"(lo));
  return r;
#else
  uint32_t v0, v1, v2;
  uint64_t r;

  if (ofs < CACHELINE_SIZE - 4)
    {
      v0 = c1->u32[CACHELINE_SIZE / sizeof(uint32_t) - 2];
      v1 = c1->u32[CACHELINE_SIZE / sizeof(uint32_t) - 1];
      v2 = c2->u32[0];
    }
  else
    {
      v0 = c1->u32[CACHELINE_SIZE / sizeof(uint32_t) - 1];
      v1 = c2->u32[0];
      v2 = c2->u32[1];
    }
  ofs = (ofs & 3) * 8;
  asm("shrd %%cl, %[v1], %[v0]; shrd %%cl, %[v2], %[v1]"
      : "=A"(r) : "c"(ofs), [v0] "a"(v0), [v1] "d"(v1), [v2] "r"(v2));

  return r;
#endif
}

#if defined(__SSE2__) || defined(__MMX__)
template<>
inline _ITM_TYPE_M64
unaligned_load2<_ITM_TYPE_M64>(const gtm_cacheline *c1,
			       const gtm_cacheline *c2, size_t ofs)
{
# ifdef __x86_64__
  __m128i lo = _mm_movpi64_epi64 (c1->m64[CACHELINE_SIZE / 8 - 1]);
  __m128i hi = _mm_movpi64_epi64 (c2->m64[0]);

  ofs = (ofs & 7) * 8;
  lo = _mm_srli_epi64 (lo, ofs);
  hi = _mm_slli_epi64 (hi, 64 - ofs);
  lo = lo | hi;
  return _mm_movepi64_pi64 (lo);
# else
  // On 32-bit we're about to return the result in an MMX register, so go
  // ahead and do the computation in that unit, even if SSE2 is available.
  __m64 lo = c1->m64[CACHELINE_SIZE / 8 - 1];
  __m64 hi = c2->m64[0];

  ofs = (ofs & 7) * 8;
  lo = _mm_srli_si64 (lo, ofs);
  hi = _mm_slli_si64 (hi, 64 - ofs);
  return lo | hi;
# endif
}
#endif // SSE2 or MMX

// The SSE types are strictly aligned.
#ifdef __SSE__
template<>
  struct strict_alignment<_ITM_TYPE_M128>
    : public std::true_type
  { };

// Expand the unaligned SSE move instructions.
template<>
inline _ITM_TYPE_M128
unaligned_load<_ITM_TYPE_M128>(const void *t)
{
  return _mm_loadu_ps (static_cast<const float *>(t));
}

template<>
inline void
unaligned_store<_ITM_TYPE_M128>(void *t, _ITM_TYPE_M128 val)
{
  _mm_storeu_ps (static_cast<float *>(t), val);
}
#endif // SSE

#ifdef __AVX__
// The AVX types are strictly aligned when it comes to vmovaps vs vmovups.
template<>
  struct strict_alignment<_ITM_TYPE_M256>
    : public std::true_type
  { };

template<>
inline _ITM_TYPE_M256
unaligned_load<_ITM_TYPE_M256>(const void *t)
{
  return _mm256_loadu_ps (static_cast<const float *>(t));
}

template<>
inline void
unaligned_store<_ITM_TYPE_M256>(void *t, _ITM_TYPE_M256 val)
{
  _mm256_storeu_ps (static_cast<float *>(t), val);
}
#endif // AVX

#ifdef __XOP__
# define HAVE_ARCH_REALIGN_M128I 1
extern const __v16qi GTM_vpperm_shift[16];
inline __m128i
realign_m128i (__m128i lo, __m128i hi, unsigned byte_count)
{
  return _mm_perm_epi8 (lo, hi, GTM_vpperm_shift[byte_count]);
}
#elif defined(__AVX__)
# define HAVE_ARCH_REALIGN_M128I 1
extern "C" const uint64_t GTM_vpalignr_table[16];
inline __m128i
realign_m128i (__m128i lo, __m128i hi, unsigned byte_count)
{
  register __m128i xmm0 __asm__("xmm0") = hi;
  register __m128i xmm1 __asm__("xmm1") = lo;
  __asm("call *%2" : "+x"(xmm0) : "x"(xmm1),
	"r"(&GTM_vpalignr_table[byte_count]));
  return xmm0;
}
#elif defined(__SSSE3__)
# define HAVE_ARCH_REALIGN_M128I 1
extern "C" const uint64_t GTM_palignr_table[16];
inline __m128i
realign_m128i (__m128i lo, __m128i hi, unsigned byte_count)
{
  register __m128i xmm0 __asm__("xmm0") = hi;
  register __m128i xmm1 __asm__("xmm1") = lo;
  __asm("call *%2" : "+x"(xmm0) : "x"(xmm1),
	"r"(&GTM_palignr_table[byte_count]));
  return xmm0;
}
#elif defined(__SSE2__)
# define HAVE_ARCH_REALIGN_M128I 1
extern "C" const char GTM_pshift_table[16 * 16];
inline __m128i
realign_m128i (__m128i lo, __m128i hi, unsigned byte_count)
{
  register __m128i xmm0 __asm__("xmm0") = lo;
  register __m128i xmm1 __asm__("xmm1") = hi;
  __asm("call *%2" : "+x"(xmm0), "+x"(xmm1)
	: "r"(GTM_pshift_table + byte_count*16));
  return xmm0;
}
#endif // XOP, AVX, SSSE3, SSE2

#ifdef HAVE_ARCH_REALIGN_M128I
template<>
inline _ITM_TYPE_M128
unaligned_load2<_ITM_TYPE_M128>(const gtm_cacheline *c1,
				const gtm_cacheline *c2, size_t ofs)
{
  return (_ITM_TYPE_M128)
    realign_m128i (c1->m128i[CACHELINE_SIZE / 16 - 1],
		   c2->m128i[0], ofs & 15);
}
#endif // HAVE_ARCH_REALIGN_M128I

#ifdef __AVX__
template<>
inline _ITM_TYPE_M256
unaligned_load2<_ITM_TYPE_M256>(const gtm_cacheline *c1,
				const gtm_cacheline *c2, size_t ofs)
{
  __m128i v0, v1;
  __m256i r;

  v0 = (__m128i) unaligned_load2<_ITM_TYPE_M128>(c1, c2, ofs);
  if (ofs < CACHELINE_SIZE - 16)
    v1 = v0, v0 = _mm_loadu_si128 ((const __m128i *) &c1->b[ofs]);
  else
    v1 = _mm_loadu_si128((const __m128i *)&c2->b[ofs + 16 - CACHELINE_SIZE]);

  r = _mm256_castsi128_si256 ((__m128i)v0);
  r = _mm256_insertf128_si256 (r, (__m128i)v1, 1);
  return (_ITM_TYPE_M256) r;
}
#endif // AVX

} // namespace GTM

#endif // LIBITM_X86_UNALIGNED_H
