/* Copyright (C) 2008, 2009, 2011, 2012 Free Software Foundation, Inc.
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


void
gtm_undolog::rollback (size_t until_size)
{
  size_t i, n = undolog.size();

  if (n > 0)
    {
      for (i = n; i-- > until_size; )
	{
          void *ptr = (void *) undolog[i--];
          size_t len = undolog[i];
          size_t words = (len + sizeof(gtm_word) - 1) / sizeof(gtm_word);
          i -= words;
          __builtin_memcpy (ptr, &undolog[i], len);
	}
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
