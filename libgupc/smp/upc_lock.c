/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011, 2012
   Free Software Foundation, Inc. 
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
#include "upc_sync.h"
#include "upc_defs.h"
#include "upc_lib.h"
#include "upc_sup.h"

static
void
__upc_lock_init (upc_shared_ptr_t ptr)
{
   upc_lock_p lock = (upc_lock_p) __cvtaddr (ptr);
   if (lock)
     {
       memset(lock, '\0', sizeof(upc_lock_t));
       __upc_init_lock (&lock->os_lock);
     }
}


upc_shared_ptr_t 
upc_global_lock_alloc ()
{
  upc_shared_ptr_t ptr;
  ptr = upc_global_alloc (1, sizeof (upc_lock_t));
  __upc_lock_init (ptr);
  return ptr;
}

void
upc_all_lock_free(upc_shared_ptr_t ptr)
{
  upc_all_free (ptr);
}

void
upc_lock_free(upc_shared_ptr_t ptr)
{
  upc_free (ptr);
}

upc_shared_ptr_t 
upc_all_lock_alloc ()
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_barrier (-1);
  if (MYTHREAD == 0)
   {
     u->all_lock = upc_global_lock_alloc ();
   }
  __upc_barrier (-1);
  return u->all_lock;
}

void
upc_lock (upc_shared_ptr_t ptr)
{
  upc_lock_p lock = __cvtaddr (ptr);
  __upc_acquire_lock (&lock->os_lock);
}

int
upc_lock_attempt (upc_shared_ptr_t ptr)
{
  upc_lock_p lock = __cvtaddr (ptr);
  return __upc_try_acquire_lock (&lock->os_lock);
}

void
upc_unlock (upc_shared_ptr_t ptr)
{
  upc_info_p u = __upc_info;
  upc_lock_p lock;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  lock = __cvtaddr (ptr);
  __upc_release_lock (&lock->os_lock);
}

/* __upc_acquire_alloc_lock() and __upc_release_alloc_lock()
   are used by the dynamic memory manager to serialize
   access to the heap data structures.  They are implemented
   here because they refer to internal runtime data structures
   that cannot easily be made visiable to UPC programs, due
   to type conflicts.  */

void
__upc_acquire_alloc_lock ()
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_acquire_lock (&u->alloc_lock);
}

void
__upc_release_alloc_lock ()
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal ("UPC runtime not initialized");
  __upc_release_lock (&u->alloc_lock);
}
