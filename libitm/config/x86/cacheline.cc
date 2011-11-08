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

#include "libitm_i.h"

namespace GTM HIDDEN {

uint32_t const gtm_bit_to_byte_mask[16] =
{
  0x00000000,
  0x000000ff,
  0x0000ff00,
  0x0000ffff,
  0x00ff0000,
  0x00ff00ff,
  0x00ffff00,
  0x00ffffff,
  0xff000000,
  0xff0000ff,
  0xff00ff00,
  0xff00ffff,
  0xffff0000,
  0xffff00ff,
  0xffffff00,
  0xffffffff
};

#ifdef __SSE2__
# define MEMBER	m128i
#else
# define MEMBER	w
#endif

void
gtm_cacheline::copy_mask (gtm_cacheline * __restrict d,
			  const gtm_cacheline * __restrict s,
			  gtm_cacheline_mask m)
{
  if (m == (gtm_cacheline_mask)-1)
    {
      *d = *s;
      return;
    }
  if (__builtin_expect (m == 0, 0))
    return;

  size_t n = sizeof(d->MEMBER[0]);
  for (size_t i = 0; i < CACHELINE_SIZE / n; ++i, m >>= n)
    store_mask (&d->MEMBER[i], s->MEMBER[i], m);
}

} // namespace GTM
