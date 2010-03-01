/* Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "upc_config.h"
#include "upc_sysdep.h"
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
upc_lock_free(upc_shared_ptr_t ptr)
{
  upc_free (ptr);
}

upc_shared_ptr_t 
upc_all_lock_alloc ()
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
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
  int status;
  status = __upc_try_acquire_lock (&lock->os_lock);
  return status;
}

void
upc_unlock (upc_shared_ptr_t ptr)
{
  upc_info_p u = __upc_info;
  upc_lock_p lock;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
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
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  __upc_acquire_lock (&u->alloc_lock);
}

void
__upc_release_alloc_lock ()
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  __upc_release_lock (&u->alloc_lock);
}
