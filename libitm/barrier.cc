/* Copyright (C) 2008-2013 Free Software Foundation, Inc.
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

using namespace GTM;

bool abi_dispatch::memmove_overlap_check(void *dst, const void *src,
    size_t size, ls_modifier dst_mod, ls_modifier src_mod)
{
  if (dst_mod == NONTXNAL || src_mod == NONTXNAL)
    {
      if (((uintptr_t)dst <= (uintptr_t)src ?
	  (uintptr_t)dst + size > (uintptr_t)src :
	  (uintptr_t)src + size > (uintptr_t)dst))
	GTM::GTM_fatal("_ITM_memmove overlapping and t/nt is not allowed");
      return false;
    }
  return true;
}

CREATE_DISPATCH_FUNCTIONS(GTM::abi_disp()->, )

