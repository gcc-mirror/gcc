/* Copyright (C) 2008, 2009, 2011 Free Software Foundation, Inc.
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

static void
do_memset(uintptr_t idst, int c, size_t size, abi_dispatch::lock_type W)
{
  abi_dispatch *disp = abi_disp();
  uintptr_t dofs = idst & (CACHELINE_SIZE - 1);
  abi_dispatch::mask_pair dpair;
  gtm_cacheline *dst
    = reinterpret_cast<gtm_cacheline *>(idst & -CACHELINE_SIZE);

  if (size == 0)
    return;

  if (dofs != 0)
    {
      size_t dleft = CACHELINE_SIZE - dofs;
      size_t min = (size <= dleft ? size : dleft);

      dpair = disp->write_lock(dst, W);
      *dpair.mask |= (((gtm_cacheline_mask)1 << min) - 1) << dofs;
      memset (&dpair.line->b[dofs], c, min);
      dst++;
      size -= min;
    }

  while (size >= CACHELINE_SIZE)
    {
      dpair = disp->write_lock(dst, W);
      *dpair.mask = -1;
      memset (dpair.line, c, CACHELINE_SIZE);
      dst++;
      size -= CACHELINE_SIZE;
    }

  if (size != 0)
    {
      dpair = disp->write_lock(dst, W);
      *dpair.mask |= ((gtm_cacheline_mask)1 << size) - 1;
      memset (dpair.line, c, size);
    }
}

#define ITM_MEM_DEF(WRITE) \
void ITM_REGPARM _ITM_memset##WRITE(void *dst, int c, size_t size)	\
{									\
  do_memset ((uintptr_t)dst, c, size, abi_dispatch::WRITE);		\
}

ITM_MEM_DEF(W)
ITM_MEM_DEF(WaR)
ITM_MEM_DEF(WaW)
