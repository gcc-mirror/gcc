/* Support for ARM EABI unwinding in VxWorks Downloadable Kernel Modules.
   Copyright (C) 2017 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if defined(__vxworks) && !defined (__RTP__)
/* Vxworks for ARM uses __gnu_Unwind_Find_exidx to retrieve the exception
   table for downloadable kernel modules.  As those modules are only partially
   linked, the linker won't generate __exidx_start|end, but the two symbols
   are still used in alternate paths from unwind-arm-common.inc.

   As we don't rely on them, but still need the symbols, we define dummy
   values here.  */
void *__exidx_start __attribute__((__visibility__ ("hidden")));
void *__exidx_end __attribute__((__visibility__ ("hidden")));
#endif
