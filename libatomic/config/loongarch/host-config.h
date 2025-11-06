/* Copyright (C) 2025 Free Software Foundation, Inc.

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

#if HAVE_IFUNC

/* We can assume Linux and Glibc here: otherwise GCC won't define
   __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16 (see HAVE_IFUNC_FOR_LIBATOMIC_16B
   in gcc/config/loongarch/linux.h) and configure.tgt won't set
   try_ifunc=1.  */

#include <sys/auxv.h>

#ifndef HWCAP_LOONGARCH_LSX
#define HWCAP_LOONGARCH_LSX (1 << 4)
#endif

#define IFUNC_NCOND(N) (N == 16)

/* We cannot rely on the argument of ifunc resolver due to
   https://sourceware.org/bugzilla/show_bug.cgi?id=33610.  Call getauxval
   on our own.  */
#define IFUNC_COND_1 ((getauxval (AT_HWCAP) & HWCAP_LOONGARCH_LSX) && \
		      (__builtin_loongarch_cpucfg (2) & (1 << 30)))

#if IFUNC_ALT == 1
#undef HAVE_ATOMIC_CAS_16
#define HAVE_ATOMIC_CAS_16 1

#undef HAVE_ATOMIC_LDST_16
#define HAVE_ATOMIC_LDST_16 1

#undef HAVE_ATOMIC_FETCH_ADD_16
#define HAVE_ATOMIC_FETCH_ADD_16 1

#undef HAVE_ATOMIC_FETCH_OP_16
#define HAVE_ATOMIC_FETCH_OP_16 1
#endif /* IFUNC_ALT == 1 */

#endif /* HAVE_IFUNC */

#include_next <host-config.h>
