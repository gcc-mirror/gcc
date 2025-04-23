/* Host/device compatibility: Itanium C++ ABI, DSO Object Destruction API

   Copyright (C) 2025 Free Software Foundation, Inc.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include "libgomp.h"

extern void __cxa_finalize (void *);

/* See <https://itanium-cxx-abi.github.io/cxx-abi/abi.html#dso-dtor>.

   Even if the device is '!DEFAULT_USE_CXA_ATEXIT', we may see '__cxa_atexit'
   calls, referencing '__dso_handle', via a 'DEFAULT_USE_CXA_ATEXIT' host.
   '__cxa_atexit' is provided by newlib, but use of '__dso_handle' for nvptx
   results in 'ld' error:

       unresolved symbol __dso_handle
       collect2: error: ld returned 1 exit status
       nvptx mkoffload: fatal error: [...]/x86_64-pc-linux-gnu-accel-nvptx-none-gcc returned 1 exit status

   ..., or for GCN get an implicit definition (running with
   '--trace-symbol=__dso_handle'):

       ./a.xamdgcn-amdhsa.mkoffload.hsaco-a.xamdgcn-amdhsa.mkoffload.2.o: reference to __dso_handle
       <internal>: definition of __dso_handle

   ..., which might be fine, but let's just make it explicit.  */

/* There are no DSOs; this is the main program.  */
attribute_hidden void * const __dso_handle = 0;

/* If this file gets linked in, that means that '__dso_handle' has been
   referenced (for '__cxa_atexit'), and in that case, we also have to run
   '__cxa_finalize'.  Make that happen by overriding the weak libgcc dummy
   function '__GCC_offload___cxa_finalize'.  */

void
__GCC_offload___cxa_finalize (void *dso_handle)
{
  __cxa_finalize (dso_handle);
}
