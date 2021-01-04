/* OpenACC Runtime initialization routines

   Copyright (C) 2014-2021 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

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

#include "openacc.h"

/* For -O and higher, the compiler always attempts to expand acc_on_device, but
   if the user disables the builtin, or calls it via a pointer, we'll need this
   version.

   Compile this with optimization, so that the compiler expands
   this, rather than generating infinitely recursive code.  */

int __attribute__ ((__optimize__ ("O2")))
acc_on_device (acc_device_t dev)
{
  return __builtin_acc_on_device (dev);
}
