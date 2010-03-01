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
#include "upc_sup.h"
#include "upc_sync.h"

/* per-thread flag set by upc_notify() and cleared by upc_wait() */
static GUPCR_THREAD_LOCAL int __upc_barrier_active = 0;

void
__upc_notify (int barrier_id)
{
  upc_info_p u = __upc_info;
  upc_barrier_info_p b;
  GUPCR_FENCE();
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if (__upc_barrier_active)
    __upc_fatal (GUPCR_NESTED_NOTIFY_MSG);
  __upc_barrier_active = 1;
  b = &u->barrier;
  b->barrier_id[MYTHREAD] = barrier_id;
}

void
__upc_wait (int barrier_id)
{
  upc_info_p u = __upc_info;
  upc_barrier_info_p b;
  int i;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if (!__upc_barrier_active)
    __upc_fatal (GUPCR_WAIT_WITHOUT_NOTIFY_MSG);
  b = &u->barrier;
  /* A barrier id of INT_MIN is defined as matching all id's */
  if ((barrier_id != INT_MIN && b->barrier_id[MYTHREAD] != INT_MIN)
       && (b->barrier_id[MYTHREAD] != barrier_id))
    __upc_fatal (GUPCR_BARRIER_MISMATCH_MSG);
  for (i = 1; i <=2; ++i)
    {
      const int child = 2 * MYTHREAD + i;
      if (child < THREADS)
	{
	  __upc_spin_until (__upc_atomic_get_bit (b->wait, child));
	  if ((barrier_id != INT_MIN && b->barrier_id[child] != INT_MIN)
	       && (b->barrier_id[child] != barrier_id))
	    __upc_fatal (GUPCR_BARRIER_MISMATCH_MSG);
	}
    }
  if (MYTHREAD == 0)
    {
      for (i = ((THREADS - 1) / OS_BITS_PER_ATOMIC_WORD); i >= 0; --i)
        b->wait[i] = 0;
    }
  else
    {
      __upc_atomic_set_bit (b->wait, MYTHREAD);
      __upc_spin_until (!__upc_atomic_get_bit(b->wait, MYTHREAD));
    }
  __upc_barrier_active = 0;
  GUPCR_FENCE ();
}

void
__upc_barrier (int barrier_id)
{
  __upc_notify (barrier_id);
  __upc_wait (barrier_id);
}
