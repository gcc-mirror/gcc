/* Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

#if HAVE_IFUNC

unsigned int __libat_feat1;

unsigned int
__libat_feat1_init (void)
{
  unsigned int eax, ebx, ecx, edx;
  FEAT1_REGISTER = 0;
  __get_cpuid (1, &eax, &ebx, &ecx, &edx);
  /* See the load in load_feat1.  */
  __atomic_store_n (&__libat_feat1, FEAT1_REGISTER, __ATOMIC_RELAXED);
  return FEAT1_REGISTER;
}

#endif /* HAVE_IFUNC */
