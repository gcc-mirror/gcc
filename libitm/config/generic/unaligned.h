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

#ifndef LIBITM_UNALIGNED_H
#define LIBITM_UNALIGNED_H 1

namespace GTM HIDDEN {

#ifndef STRICT_ALIGNMENT
#define STRICT_ALIGNMENT 1
#endif

// A type trait for whether type T requires strict alignment.
// The generic types are assumed to all be the same; specializations
// for target-specific types should be done in config/cpu/unaligned.h.
template<typename T>
  struct strict_alignment
    : public std::integral_constant<bool, STRICT_ALIGNMENT>
  { };

// A helper template for accessing an integral type the same size as T
template<typename T>
  struct make_integral
    : public sized_integral<sizeof(T)>
  { };

// A helper class for accessing T as an unaligned value.
template<typename T>
struct __attribute__((packed)) unaligned_helper
  { T x; };

// A helper class for view-converting T as an integer.
template<typename T>
union view_convert_helper
{
  typedef T type;
  typedef make_integral<T> itype;

  type t;
  itype i;
};

// Generate an unaligned load sequence.
// The compiler knows how to do this for any specific type.
template<typename T>
inline T ALWAYS_INLINE
unaligned_load(const void *t)
{
  typedef unaligned_helper<T> UT;
  const UT *ut = reinterpret_cast<const UT *>(t);
  return ut->x;
}

// Generate an unaligned store sequence.
template<typename T>
inline void ALWAYS_INLINE
unaligned_store(void *t, T val)
{
  typedef unaligned_helper<T> UT;
  UT *ut = reinterpret_cast<UT *>(t);
  ut->x = val;
}

// Generate an unaligned load from two different cachelines.
// It is known that OFS + SIZEOF(T) > CACHELINE_SIZE.
template<typename T>
inline T ALWAYS_INLINE
unaligned_load2(const gtm_cacheline *c1, const gtm_cacheline *c2, size_t ofs)
{
  size_t left = CACHELINE_SIZE - ofs;
  T ret;

  memcpy (&ret, &c1->b[ofs], left);
  memcpy ((char *)&ret + ofs, c2, sizeof(T) - left);

  return ret;
}

// Generate an unaligned store into two different cachelines.
// It is known that OFS + SIZEOF(T) > CACHELINE_SIZE.
template<typename T>
inline void ALWAYS_INLINE
unaligned_store2(gtm_cacheline *c1, gtm_cacheline *c2, size_t ofs, T val)
{
  size_t left = CACHELINE_SIZE - ofs;
  memcpy (&c1->b[ofs], &val, left);
  memcpy (c2, (char *)&val + left, sizeof(T) - left);
}

#ifndef HAVE_ARCH_UNALIGNED_LOAD2_U2
template<>
inline uint16_t ALWAYS_INLINE
unaligned_load2<uint16_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint16_t v1 = c1->b[CACHELINE_SIZE - 1];
  uint16_t v2 = c2->b[0];

  if (WORDS_BIGENDIAN)
    return v1 << 8 | v2;
  else
    return v2 << 8 | v1;
}
#endif

#ifndef HAVE_ARCH_UNALIGNED_LOAD2_U4
template<>
inline uint32_t ALWAYS_INLINE
unaligned_load2<uint32_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint32_t v1 = c1->u32[CACHELINE_SIZE / sizeof(uint32_t) - 1];
  uint32_t v2 = c2->u32[0];
  int s2 = (ofs & (sizeof(uint32_t) - 1)) * 8;
  int s1 = sizeof(uint32_t) * 8 - s2;

  if (WORDS_BIGENDIAN)
    return v1 << s2 | v2 >> s1;
  else
    return v2 << s2 | v1 >> s1;
}
#endif

#ifndef HAVE_ARCH_UNALIGNED_LOAD2_U8
template<>
inline uint64_t ALWAYS_INLINE
unaligned_load2<uint64_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint64_t v1 = c1->u64[CACHELINE_SIZE / sizeof(uint64_t) - 1];
  uint64_t v2 = c2->u64[0];
  int s2 = (ofs & (sizeof(uint64_t) - 1)) * 8;
  int s1 = sizeof(uint64_t) * 8 - s2;

  if (WORDS_BIGENDIAN)
    return v1 << s2 | v2 >> s1;
  else
    return v2 << s2 | v1 >> s1;
}
#endif

template<>
inline float ALWAYS_INLINE
unaligned_load2<float>(const gtm_cacheline *c1,
		       const gtm_cacheline *c2, size_t ofs)
{
  typedef view_convert_helper<float> VC; VC vc;
  vc.i = unaligned_load2<VC::itype>(c1, c2, ofs);
  return vc.t;
}

template<>
inline double ALWAYS_INLINE
unaligned_load2<double>(const gtm_cacheline *c1,
			const gtm_cacheline *c2, size_t ofs)
{
  typedef view_convert_helper<double> VC; VC vc;
  vc.i = unaligned_load2<VC::itype>(c1, c2, ofs);
  return vc.t;
}

#ifndef HAVE_ARCH_UNALIGNED_STORE2_U2
template<>
inline void ALWAYS_INLINE
unaligned_store2<uint16_t>(gtm_cacheline *c1, gtm_cacheline *c2,
			   size_t ofs, uint16_t val)
{
  uint8_t vl = val, vh = val >> 8;

  if (WORDS_BIGENDIAN)
    {
      c1->b[CACHELINE_SIZE - 1] = vh;
      c2->b[0] = vl;
    }
  else
    {
      c1->b[CACHELINE_SIZE - 1] = vl;
      c2->b[0] = vh;
    }
}
#endif

#if 0
#ifndef HAVE_ARCH_UNALIGNED_STORE2_U4
template<>
inline void ALWAYS_INLINE
unaligned_store2<uint32_t>(gtm_cacheline *c1, gtm_cacheline *c2,
			   size_t ofs, uint32_t val)
{
  // ??? We could reuse the store_mask stuff here.
}
#endif

template<>
inline void ALWAYS_INLINE
unaligned_store2<float>(gtm_cacheline *c1, gtm_cacheline *c2,
			size_t ofs, float val)
{
  typedef view_convert_helper<float> VC; VC vc;
  vc.t = val;
  unaligned_store2(c1, c2, ofs, vc.i);
}
#endif

} // namespace GTM

#endif // LIBITM_UNALIGNED_H
