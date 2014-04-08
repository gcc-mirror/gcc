/* Copyright (C) 2014 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   It is derived from libatomic/config/posix/lock.c.

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
#include "upc_libat_lock.h"

/* Locking for a typical-sized operation.  */
void
libat_lock_1 (void *ARG_UNUSED (ptr))
{
  upc_info_p u = __upc_info;
  __upc_acquire_lock (&u->lock);
}

void
libat_unlock_1 (void *ARG_UNUSED (ptr))
{
  upc_info_p u = __upc_info;
  __upc_release_lock (&u->lock);
}

/* Locking for a "large" operation.  This should always be some sort of
   test-and-set operation, as we assume that the interrupt latency would
   be unreasonably large.  */
void
libat_lock_n (void *ARG_UNUSED (ptr), size_t ARG_UNUSED (n))
{
  upc_info_p u = __upc_info;
  __upc_acquire_lock (&u->lock);
}

void
libat_unlock_n (void *ARG_UNUSED (ptr), size_t ARG_UNUSED (n))
{
  upc_info_p u = __upc_info;
  __upc_release_lock (&u->lock);
}
