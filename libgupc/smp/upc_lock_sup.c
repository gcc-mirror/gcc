/* Copyright (C) 2010-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"

/* On some 32 bit targets 64 bit CSWAP is not available.  */
#if !defined (__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8)
/* Use OS spin lock to protect the critical section.  */
unsigned char
__sync_bool_compare_and_swap_8 (long long *addr, long long exp, long long val)
{
  unsigned char result = 0;
  upc_info_p u = __upc_info;
  __upc_acquire_lock (&u->lock);
  if (*addr == exp)
    {
      *addr = val;
      result = 1;
    }
  __upc_release_lock (&u->lock);
  return result;
}
#endif
 
