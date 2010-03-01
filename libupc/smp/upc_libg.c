/* Copyright (C) 2005-2009 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>
   Original Implementation by Adam Leko <adam@leko.org>.

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
#include "upc_lib.h"
#include "gasp_upc.h"
#include "upc_pupc.h"

void
__upc_barrierg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_BARRIER, named, barrier_id);
  __upc_barrier (barrier_id);
  p_end (GASP_UPC_BARRIER, named, barrier_id);
}

void
__upc_notifyg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_NOTIFY, named, barrier_id);
  __upc_notify (barrier_id);
  p_end (GASP_UPC_NOTIFY, named, barrier_id);
}

void
__upc_waitg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_WAIT, named, barrier_id);
  __upc_wait (barrier_id);
  p_end (GASP_UPC_WAIT, named, barrier_id);
}

upc_shared_ptr_t
upc_global_lock_allocg (const char *filename, int linenum)
{
  upc_shared_ptr_t result;
  p_start (GASP_UPC_GLOBAL_LOCK_ALLOC);
  result = upc_global_lock_alloc();
  p_end (GASP_UPC_GLOBAL_LOCK_ALLOC, &result);
  return result;
}

void
upc_lock_freeg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_LOCK_FREE, &ptr);
  upc_lock_free(ptr);
  p_end (GASP_UPC_LOCK_FREE, &ptr);
}

upc_shared_ptr_t
upc_all_lock_allocg (const char *filename, int linenum)
{
  upc_shared_ptr_t result;
  p_start (GASP_UPC_ALL_LOCK_ALLOC);
  result = upc_all_lock_alloc();
  p_end (GASP_UPC_ALL_LOCK_ALLOC, &result);
  return result;
}

void
upc_lockg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_LOCK, &ptr);
  upc_lock(ptr);
  p_end (GASP_UPC_LOCK, &ptr);
}

int
upc_lock_attemptg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  int status;
  p_start (GASP_UPC_LOCK_ATTEMPT, &ptr);
  status = upc_lock_attempt(ptr);
  p_end (GASP_UPC_LOCK_ATTEMPT, &ptr, status);
  return status;
}

void
upc_unlockg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_UNLOCK, &ptr);
  upc_unlock(ptr);
  p_end (GASP_UPC_UNLOCK, &ptr);
}

void
__upc_funcg (int start, const char *funcname,
             const char *filename, const int linenum)
{
  if (start)
    p_start (GASP_C_FUNC, funcname);
  else
    p_end (GASP_C_FUNC, funcname);
}

void
__upc_forallg (int start, const char *filename, const int linenum)
{
  if (start)
    p_start (GASP_UPC_FORALL);
  else
    p_end (GASP_UPC_FORALL);
}

extern void __upc_exitg (int status, const char *filename, int linenum)
             __attribute__ ((__noreturn__));

void
__upc_exitg (int status, const char *filename, int linenum)
{
  p_start (GASP_UPC_COLLECTIVE_EXIT, status);
  p_end (GASP_UPC_COLLECTIVE_EXIT, status);
  __upc_exit (status);
}

void
upc_global_exitg (int status, const char *filename, int linenum)
{
  p_atomic (GASP_UPC_NONCOLLECTIVE_EXIT, status);
  upc_global_exit (status);
}
