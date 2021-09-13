/* Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by Andreas Krebbel <krebbel@linux.vnet.ibm.com>

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


/* The compiler builtin will use the hardware instruction cdsg if the
   memory operand is properly aligned and will fall back to the
   library call otherwise.

   In case the compiler for one part is able to detect that the
   location is aligned and fails to do so for another usage of the hw
   instruction and the sw fall back would be mixed on the same memory
   location.  To avoid this the library fall back also has to use the
   hardware instruction if possible.  */

#if !DONE && N == 16
UTYPE
SIZE(libat_exchange) (UTYPE *mptr, UTYPE newval, int smodel UNUSED)
{
  if (!((uintptr_t)mptr & 0xf))
    {
      /* Use the builtin only if the memory operand is 16 byte
	 aligned.  */
      return __atomic_exchange_n ((UTYPE *)__builtin_assume_aligned (mptr, 16),
				  newval, __ATOMIC_SEQ_CST);
    }
  else
    {
      UTYPE oldval;
      UWORD magic;

      pre_seq_barrier (smodel);
      magic = protect_start (mptr);

      oldval = *mptr;
      *mptr = newval;

      protect_end (mptr, magic);
      post_seq_barrier (smodel);

      return oldval;
    }
}
#define DONE 1
#endif /* N == 16 */

#include "../../exch_n.c"
