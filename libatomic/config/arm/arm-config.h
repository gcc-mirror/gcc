/* Copyright (C) 2012-2019 Free Software Foundation, Inc.
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


#if __ARM_FEATURE_LDREX & 4
# define HAVE_STREX	1
#endif
#if (__ARM_FEATURE_LDREX & 0xF) == 0xF
# define HAVE_STREXBHD	1
#endif

#if __ARM_ARCH >= 7
# define HAVE_DMB	1
#elif __ARM_ARCH == 6
# define HAVE_DMB_MCR	1
#endif
