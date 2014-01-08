/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

#include "libatomic_i.h"


/* If we use the builtin we'll get __sync_synchronize, not __kernel_dmb.  */
#if !DONE && N <= WORDSIZE && !defined(HAVE_DMB) && !defined(HAVE_DMB_MCR)
UTYPE
SIZE(libat_load) (UTYPE *mptr, int smodel)
{
  UTYPE ret;

  if (maybe_specialcase_relaxed(smodel))
    ret = *mptr;
  else
    {
      __kernel_dmb ();
      ret = *mptr;
      __kernel_dmb ();
    }
  return ret;
}

#define DONE 1
#endif /* !HAVE_DMB */

#include "../../../load_n.c"
