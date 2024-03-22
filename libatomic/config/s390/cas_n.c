/* Copyright (C) 2018-2024 Free Software Foundation, Inc.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include <libatomic_i.h>


/* Analog to config/s390/exch_n.c.  */

#if !DONE && N == 16
bool
SIZE(libat_compare_exchange) (UTYPE *mptr, UTYPE *eptr, UTYPE newval,
			      int smodel, int fmodel UNUSED)
{
  if (!((uintptr_t)mptr & 0xf))
    {
      return __atomic_compare_exchange_n (
	(UTYPE *)__builtin_assume_aligned (mptr, 16), eptr, newval, false,
	__ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
    }
  else
    {
      UTYPE oldval;
      UWORD magic;
      bool ret;

      pre_seq_barrier (smodel);
      magic = protect_start (mptr);

      oldval = *mptr;
      ret = (oldval == *eptr);
      if (ret)
	*mptr = newval;
      else
	*eptr = oldval;

      protect_end (mptr, magic);
      post_seq_barrier (smodel);

      return ret;
    }
}
#define DONE 1
#endif /* N == 16 */

#include "../../cas_n.c"
