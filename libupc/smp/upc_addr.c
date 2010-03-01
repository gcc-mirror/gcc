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

void *
__cvtaddr (upc_shared_ptr_t p)
{
  upc_info_p u = __upc_info;
  void *addr;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if (GUPCR_PTS_IS_NULL (p))
    return (void *) 0;
  addr = __upc_sptr_to_addr (p);
  return addr;
}

void *
__getaddr (upc_shared_ptr_t p)
{
  upc_info_p u = __upc_info;
  void *addr;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if (GUPCR_PTS_IS_NULL (p))
    return (void *) 0;
  if ((int)GUPCR_PTS_THREAD(p) != MYTHREAD)
    __upc_fatal (GUPCR_SHARED_CVT_NO_AFFINITY_MSG);
  addr = __upc_sptr_to_addr (p);
  return addr;
}

size_t
upc_threadof (upc_shared_ptr_t p)
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if ((int)GUPCR_PTS_THREAD(p) >= THREADS)
    __upc_fatal (GUPCR_INVALID_THREAD_IN_ADDR_MSG);
  return GUPCR_PTS_THREAD (p);
}

size_t
upc_phaseof (upc_shared_ptr_t p)
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if ((int)GUPCR_PTS_THREAD(p) >= THREADS)
    __upc_fatal (GUPCR_INVALID_THREAD_IN_ADDR_MSG);
  return GUPCR_PTS_PHASE (p);
}

upc_shared_ptr_t
upc_resetphase (upc_shared_ptr_t p)
{
  upc_shared_ptr_t result;
  result = p;
  GUPCR_PTS_SET_PHASE (result, 0);
  return result;
}

size_t
upc_addrfield (upc_shared_ptr_t p)
{
  upc_info_p u = __upc_info;
  if (!u)
    __upc_fatal (GUPCR_NOT_INIT_MSG);
  if ((int)GUPCR_PTS_THREAD(p) >= THREADS)
    __upc_fatal (GUPCR_INVALID_THREAD_IN_ADDR_MSG);
  return (size_t) GUPCR_PTS_VADDR (p);
}

size_t
upc_affinitysize (size_t totalsize, size_t nbytes, size_t threadid)
{ 
  size_t result;
  if (nbytes == 0 || totalsize == 0 || nbytes >= totalsize)
      result = (size_t) (threadid == 0 ? totalsize : 0);
  else
    {
      size_t const nblocks = (totalsize / nbytes);
      size_t const cutoff = (nblocks % THREADS);
      if (threadid < cutoff)
	result = (size_t) ((nblocks + THREADS - 1) / THREADS) * nbytes;
      else if (threadid > cutoff)
	result = (size_t) (nblocks / THREADS) * nbytes;
      else /* threadid == cutoff */
	result = (size_t) ((nblocks / THREADS) * nbytes)
			   + totalsize - nblocks * nbytes;
    }
  return result;
}
