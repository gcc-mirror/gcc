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

#ifndef LIBITM_ALPHA_UNALIGNED_H
#define LIBITM_ALPHA_UNALIGNED_H 1

#define HAVE_ARCH_UNALIGNED_LOAD2_U2 1
#define HAVE_ARCH_UNALIGNED_LOAD2_U4 1
#define HAVE_ARCH_UNALIGNED_LOAD2_U8 1

#ifndef __alpha_bwx__
#define HAVE_ARCH_UNALIGNED_STORE2_U2 1
#endif
#define HAVE_ARCH_UNALIGNED_STORE2_U4 1
#define HAVE_ARCH_UNALIGNED_STORE2_U8 1

#include "config/generic/unaligned.h"

namespace GTM HIDDEN {

template<>
inline uint16_t ALWAYS_INLINE
unaligned_load2<uint16_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint64_t v1 = c1->u64[CACHELINE_SIZE / sizeof(uint64_t) - 1];
  uint64_t v2 = c2->u64[0];

  return __builtin_alpha_extwl (v1, ofs) | __builtin_alpha_extwh (v2, ofs);
}

template<>
inline uint32_t ALWAYS_INLINE
unaligned_load2<uint32_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint64_t v1 = c1->u64[CACHELINE_SIZE / sizeof(uint64_t) - 1];
  uint64_t v2 = c2->u64[0];

  return __builtin_alpha_extll (v1, ofs) + __builtin_alpha_extlh (v2, ofs);
}

template<>
inline uint64_t ALWAYS_INLINE
unaligned_load2<uint64_t>(const gtm_cacheline *c1,
			  const gtm_cacheline *c2, size_t ofs)
{
  uint64_t v1 = c1->u64[CACHELINE_SIZE / sizeof(uint64_t) - 1];
  uint64_t v2 = c2->u64[0];

  return __builtin_alpha_extql (v1, ofs) | __builtin_alpha_extqh (v2, ofs);
}

#ifndef __alpha_bwx__
template<>
inline void
unaligned_store2<uint16_t>(gtm_cacheline *c1, gtm_cacheline *c2,
			   size_t ofs, uint16_t val)
{
  uint32_t vl = (uint32_t)val << 24, vh = val >> 8;

  gtm_cacheline::store_mask (&c1->u32[CACHELINE_SIZE / 4 - 1], vl, 4);
  gtm_cacheline::store_mask (&c2->u32[0], vh, 1);
}
#endif

template<>
inline void
unaligned_store2<uint32_t>(gtm_cacheline *c1, gtm_cacheline *c2,
			   size_t ofs, uint32_t val)
{
  uint64_t vl = __builtin_alpha_insll (val, ofs);
  uint64_t ml = __builtin_alpha_insll (~0u, ofs);
  uint64_t vh = __builtin_alpha_inslh (val, ofs);
  uint64_t mh = __builtin_alpha_inslh (~0u, ofs);

  gtm_cacheline::store_mask (&c1->u64[CACHELINE_SIZE / 8 - 1], vl, ml);
  gtm_cacheline::store_mask (&c2->u64[0], vh, mh);
}

template<>
inline void
unaligned_store2<uint64_t>(gtm_cacheline *c1, gtm_cacheline *c2,
			   size_t ofs, uint64_t val)
{
  uint64_t vl = __builtin_alpha_insql (val, ofs);
  uint64_t ml = __builtin_alpha_insql (~0u, ofs);
  uint64_t vh = __builtin_alpha_insqh (val, ofs);
  uint64_t mh = __builtin_alpha_insqh (~0u, ofs);

  gtm_cacheline::store_mask (&c1->u64[CACHELINE_SIZE / 8 - 1], vl, ml);
  gtm_cacheline::store_mask (&c2->u64[0], vh, mh);
}

} // namespace GTM

#endif // LIBITM_ALPHA_UNALIGNED_H
