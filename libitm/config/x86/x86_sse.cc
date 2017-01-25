/* Copyright (C) 2009-2017 Free Software Foundation, Inc.
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
#include "dispatch.h"

// ??? Use memcpy for now, until we have figured out how to best instantiate
// these loads/stores.
CREATE_DISPATCH_FUNCTIONS_T_MEMCPY(M64, GTM::abi_disp()->, )
CREATE_DISPATCH_FUNCTIONS_T_MEMCPY(M128, GTM::abi_disp()->, )

void ITM_REGPARM
_ITM_LM64 (const _ITM_TYPE_M64 *ptr)
{
  GTM::GTM_LB (ptr, sizeof (*ptr));
}

void ITM_REGPARM
_ITM_LM128 (const _ITM_TYPE_M128 *ptr)
{
  GTM::GTM_LB (ptr, sizeof (*ptr));
}
