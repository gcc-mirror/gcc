/* Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

// This function needs to be noinline because we need to prevent that it gets
// inlined into another function that calls further functions. This could
// break our assumption that we only call memcpy and thus only need to
// additionally protect the memcpy stack (see the hack in mask_stack_bottom()).
// Even if that isn't an issue because those other calls don't happen during
// copying, we still need mask_stack_bottom() to be called "close" to the
// memcpy in terms of stack frames, so just ensure that for now using the
// noinline.
void __attribute__((noinline))
gtm_undolog::rollback (gtm_thread* tx, size_t until_size)
{
  size_t i, n = undolog.size();
  void *top = mask_stack_top(tx);
  void *bot = mask_stack_bottom(tx);

  if (n > 0)
    {
      for (i = n; i-- > until_size; )
	{
          void *ptr = (void *) undolog[i--];
          size_t len = undolog[i];
          size_t words = (len + sizeof(gtm_word) - 1) / sizeof(gtm_word);
          i -= words;
          // Filter out any updates that overlap the libitm stack.  We don't
          // bother filtering out just the overlapping bytes because we don't
          // merge writes and thus any overlapping write is either bogus or
          // would restore data on stack frames that are not in use anymore.
          // FIXME The memcpy can/will end up as another call but we
          // calculated BOT based on the current function.  Can we inline or
          // reimplement this without too much trouble due to unaligned calls
          // and still have good performance, so that we can remove the hack
          // in mask_stack_bottom()?
          if (likely(ptr > top || (uint8_t*)ptr + len <= bot))
            __builtin_memcpy (ptr, &undolog[i], len);
	}
      undolog.set_size(until_size);
    }
}

void ITM_REGPARM
GTM_LB (const void *ptr, size_t len)
{
  gtm_thread *tx = gtm_thr();
  tx->undolog.log(ptr, len);
}

} // namespace GTM

using namespace GTM;

/* ??? Use configure to determine if aliases are supported.  Or convince
   the compiler to not just tail call this, but actually generate the
   same_body_alias itself.  */
void ITM_REGPARM
_ITM_LB (const void *ptr, size_t len)
{
  GTM_LB (ptr, len);
}

#define ITM_LOG_DEF(T) \
void ITM_REGPARM _ITM_L##T (const _ITM_TYPE_##T *ptr) \
{ GTM_LB (ptr, sizeof (*ptr)); }

ITM_LOG_DEF(U1)
ITM_LOG_DEF(U2)
ITM_LOG_DEF(U4)
ITM_LOG_DEF(U8)
ITM_LOG_DEF(F)
ITM_LOG_DEF(D)
ITM_LOG_DEF(E)
ITM_LOG_DEF(CF)
ITM_LOG_DEF(CD)
ITM_LOG_DEF(CE)
