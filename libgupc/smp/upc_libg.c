/* Copyright (c) 2009, 2010, 2011
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime library.
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
#include "upc_sup.h"
#include "upc_lib.h"
#include "gasp_upc.h"
#include "upc_pupc.h"

void
__upc_barrierg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_BARRIER, named, barrier_id);
  GUPCR_SET_ERR_LOC();
  __upc_barrier (barrier_id);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_BARRIER, named, barrier_id);
}

void
__upc_notifyg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_NOTIFY, named, barrier_id);
  GUPCR_SET_ERR_LOC();
  __upc_notify (barrier_id);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_NOTIFY, named, barrier_id);
}

void
__upc_waitg (int barrier_id, const char *filename, const int linenum)
{
  int named = (barrier_id != INT_MIN);
  p_start (GASP_UPC_WAIT, named, barrier_id);
  GUPCR_SET_ERR_LOC();
  __upc_wait (barrier_id);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_WAIT, named, barrier_id);
}

upc_shared_ptr_t
upc_global_lock_allocg (const char *filename, int linenum)
{
  upc_shared_ptr_t result;
  p_start (GASP_UPC_GLOBAL_LOCK_ALLOC);
  GUPCR_SET_ERR_LOC();
  result = upc_global_lock_alloc();
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_GLOBAL_LOCK_ALLOC, &result);
  return result;
}

void
upc_lock_freeg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_LOCK_FREE, &ptr);
  GUPCR_SET_ERR_LOC();
  upc_lock_free(ptr);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_LOCK_FREE, &ptr);
}

upc_shared_ptr_t
upc_all_lock_allocg (const char *filename, int linenum)
{
  upc_shared_ptr_t result;
  p_start (GASP_UPC_ALL_LOCK_ALLOC);
  GUPCR_SET_ERR_LOC();
  result = upc_all_lock_alloc();
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_ALL_LOCK_ALLOC, &result);
  return result;
}

void
upc_lockg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_LOCK, &ptr);
  GUPCR_SET_ERR_LOC();
  upc_lock(ptr);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_LOCK, &ptr);
}

int
upc_lock_attemptg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  int status;
  p_start (GASP_UPC_LOCK_ATTEMPT, &ptr);
  GUPCR_SET_ERR_LOC();
  status = upc_lock_attempt(ptr);
  GUPCR_CLEAR_ERR_LOC();
  p_end (GASP_UPC_LOCK_ATTEMPT, &ptr, status);
  return status;
}

void
upc_unlockg (upc_shared_ptr_t ptr, const char *filename, int linenum)
{
  p_start (GASP_UPC_UNLOCK, &ptr);
  GUPCR_SET_ERR_LOC();
  upc_unlock(ptr);
  GUPCR_CLEAR_ERR_LOC();
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

void *
__cvtaddrg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  void *result;
  GUPCR_SET_ERR_LOC();
  result = __cvtaddr (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

void *
__getaddrg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  void *result;
  GUPCR_SET_ERR_LOC();
  result = __getaddr (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

size_t
upc_addrfieldg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  size_t result;
  GUPCR_SET_ERR_LOC();
  result = upc_addrfield (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

size_t
upc_affinitysizeg (size_t totalsize, size_t nbytes, size_t threadid,
                   const char *filename, int linenum)
{
  size_t result;
  GUPCR_SET_ERR_LOC();
  result = upc_affinitysize (totalsize, nbytes, threadid);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

size_t
upc_phaseofg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  size_t result;
  GUPCR_SET_ERR_LOC();
  result = upc_phaseof (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

upc_shared_ptr_t
upc_resetphaseg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  upc_shared_ptr_t result;
  GUPCR_SET_ERR_LOC();
  result = upc_resetphase (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}

size_t
upc_threadofg (upc_shared_ptr_t p, const char *filename, int linenum)
{
  size_t result;
  GUPCR_SET_ERR_LOC();
  result = upc_threadof (p);
  GUPCR_CLEAR_ERR_LOC();
  return result;
}
