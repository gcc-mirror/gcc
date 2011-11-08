/* Copyright (C) 2008, 2009, 2011 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include "libitm_i.h"

using namespace GTM;

static void
do_memcpy (uintptr_t idst, uintptr_t isrc, size_t size,
	   abi_dispatch::lock_type W, abi_dispatch::lock_type R)
{
  abi_dispatch *disp = abi_disp();
  // The position in the destination cacheline where *IDST starts.
  uintptr_t dofs = idst & (CACHELINE_SIZE - 1);
  // The position in the source cacheline where *ISRC starts.
  uintptr_t sofs = isrc & (CACHELINE_SIZE - 1);
  const gtm_cacheline *src
    = reinterpret_cast<const gtm_cacheline *>(isrc & -CACHELINE_SIZE);
  gtm_cacheline *dst
    = reinterpret_cast<gtm_cacheline *>(idst & -CACHELINE_SIZE);
  const gtm_cacheline *sline;
  abi_dispatch::mask_pair dpair;

  if (size == 0)
    return;

  // If both SRC and DST data start at the same position in the cachelines,
  // we can easily copy the data in tandem, cacheline by cacheline...
  if (dofs == sofs)
    {
      // We copy the data in three stages:

      // (a) Copy stray bytes at the beginning that are smaller than a
      // cacheline.
      if (sofs != 0)
	{
	  size_t sleft = CACHELINE_SIZE - sofs;
	  size_t min = (size <= sleft ? size : sleft);

	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask |= (((gtm_cacheline_mask)1 << min) - 1) << sofs;
	  memcpy (&dpair.line->b[sofs], &sline->b[sofs], min);
	  dst++;
	  src++;
	  size -= min;
	}

      // (b) Copy subsequent cacheline sized chunks.
      while (size >= CACHELINE_SIZE)
	{
	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask = -1;
	  *dpair.line = *sline;
	  dst++;
	  src++;
	  size -= CACHELINE_SIZE;
	}

      // (c) Copy anything left over.
      if (size != 0)
	{
	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask |= ((gtm_cacheline_mask)1 << size) - 1;
	  memcpy (dpair.line, sline, size);
	}
    }
  // ... otherwise, we must copy the data in disparate hunks using
  // temporary storage.
  else
    {
      gtm_cacheline c;
      size_t sleft = CACHELINE_SIZE - sofs;

      sline = disp->read_lock(src, R);

      // As above, we copy the data in three stages:

      // (a) Copy stray bytes at the beginning that are smaller than a
      // cacheline.
      if (dofs != 0)
	{
	  size_t dleft = CACHELINE_SIZE - dofs;
	  size_t min = (size <= dleft ? size : dleft);

	  dpair = disp->write_lock(dst, W);
	  *dpair.mask |= (((gtm_cacheline_mask)1 << min) - 1) << dofs;

	  // If what's left in the source cacheline will fit in the
	  // rest of the destination cacheline, straight up copy it.
	  if (min <= sleft)
	    {
	      memcpy (&dpair.line->b[dofs], &sline->b[sofs], min);
	      sofs += min;
	    }
	  // Otherwise, we need more bits from the source cacheline
	  // that are available.  Piece together what we need from
	  // contiguous (source) cachelines, into temp space, and copy
	  // it over.
	  else
	    {
	      memcpy (&c, &sline->b[sofs], sleft);
	      sline = disp->read_lock(++src, R);
	      sofs = min - sleft;
	      memcpy (&c.b[sleft], sline, sofs);
	      memcpy (&dpair.line->b[dofs], &c, min);
	    }
	  sleft = CACHELINE_SIZE - sofs;

	  dst++;
	  size -= min;
	}

      // (b) Copy subsequent cacheline sized chunks.
      while (size >= CACHELINE_SIZE)
	{
	  // We have a full (destination) cacheline where to put the
	  // data, but to get to the corresponding cacheline sized
	  // chunk in the source, we have to piece together two
	  // contiguous source cachelines.

	  memcpy (&c, &sline->b[sofs], sleft);
	  sline = disp->read_lock(++src, R);
	  memcpy (&c.b[sleft], sline, sofs);

	  dpair = disp->write_lock(dst, W);
	  *dpair.mask = -1;
	  *dpair.line = c;

	  dst++;
	  size -= CACHELINE_SIZE;
	}

      // (c) Copy anything left over.
      if (size != 0)
	{
	  dpair = disp->write_lock(dst, W);
	  *dpair.mask |= ((gtm_cacheline_mask)1 << size) - 1;
	  // If what's left to copy is entirely in the remaining
	  // source cacheline, do it.
	  if (size <= sleft)
	    memcpy (dpair.line, &sline->b[sofs], size);
	  // Otherwise, piece together the remaining bits, and copy.
	  else
	    {
	      memcpy (&c, &sline->b[sofs], sleft);
	      sline = disp->read_lock(++src, R);
	      memcpy (&c.b[sleft], sline, size - sleft);
	      memcpy (dpair.line, &c, size);
	    }
	}
    }
}

static void
do_memmove (uintptr_t idst, uintptr_t isrc, size_t size,
	    abi_dispatch::lock_type W, abi_dispatch::lock_type R)
{
  abi_dispatch *disp = abi_disp();
  uintptr_t dleft, sleft, sofs, dofs;
  const gtm_cacheline *sline;
  abi_dispatch::mask_pair dpair;

  if (size == 0)
    return;

  /* The co-aligned memmove below doesn't work for DST == SRC, so filter
     that out.  It's tempting to just return here, as this is a no-op move.
     However, our caller has the right to expect the locks to be acquired
     as advertized.  */
  if (__builtin_expect (idst == isrc, 0))
    {
      /* If the write lock is already acquired, nothing to do.  */
      if (W == abi_dispatch::WaW)
	return;
      /* If the destination is protected, acquire a write lock.  */
      if (W != abi_dispatch::NOLOCK)
	R = abi_dispatch::RfW;
      /* Notice serial mode, where we don't acquire locks at all.  */
      if (R == abi_dispatch::NOLOCK)
	return;

      idst = isrc + size;
      for (isrc &= -CACHELINE_SIZE; isrc < idst; isrc += CACHELINE_SIZE)
	disp->read_lock(reinterpret_cast<const gtm_cacheline *>(isrc), R);
      return;
    }

  /* Fall back to memcpy if the implementation above can handle it.  */
  if (idst < isrc || isrc + size <= idst)
    {
      do_memcpy (idst, isrc, size, W, R);
      return;
    }

  /* What remains requires a backward copy from the end of the blocks.  */
  idst += size;
  isrc += size;
  dofs = idst & (CACHELINE_SIZE - 1);
  sofs = isrc & (CACHELINE_SIZE - 1);
  dleft = CACHELINE_SIZE - dofs;
  sleft = CACHELINE_SIZE - sofs;

  gtm_cacheline *dst
    = reinterpret_cast<gtm_cacheline *>(idst & -CACHELINE_SIZE);
  const gtm_cacheline *src
    = reinterpret_cast<const gtm_cacheline *>(isrc & -CACHELINE_SIZE);
  if (dofs == 0)
    dst--;
  if (sofs == 0)
    src--;

  if (dofs == sofs)
    {
      /* Since DST and SRC are co-aligned, and we didn't use the memcpy
	 optimization above, that implies that SIZE > CACHELINE_SIZE.  */
      if (sofs != 0)
	{
	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask |= ((gtm_cacheline_mask)1 << sleft) - 1;
	  memcpy (dpair.line, sline, sleft);
	  dst--;
	  src--;
	  size -= sleft;
	}

      while (size >= CACHELINE_SIZE)
	{
	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask = -1;
	  *dpair.line = *sline;
	  dst--;
	  src--;
	  size -= CACHELINE_SIZE;
	}

      if (size != 0)
	{
	  size_t ofs = CACHELINE_SIZE - size;
	  dpair = disp->write_lock(dst, W);
	  sline = disp->read_lock(src, R);
	  *dpair.mask |= (((gtm_cacheline_mask)1 << size) - 1) << ofs;
	  memcpy (&dpair.line->b[ofs], &sline->b[ofs], size);
	}
    }
  else
    {
      gtm_cacheline c;

      sline = disp->read_lock(src, R);
      if (dofs != 0)
	{
	  size_t min = (size <= dofs ? size : dofs);

	  if (min <= sofs)
	    {
	      sofs -= min;
	      memcpy (&c, &sline->b[sofs], min);
	    }
	  else
	    {
	      size_t min_ofs = min - sofs;
	      memcpy (&c.b[min_ofs], sline, sofs);
	      sline = disp->read_lock(--src, R);
	      sofs = CACHELINE_SIZE - min_ofs;
	      memcpy (&c, &sline->b[sofs], min_ofs);
	    }

	  dofs = dleft - min;
	  dpair = disp->write_lock(dst, W);
	  *dpair.mask |= (((gtm_cacheline_mask)1 << min) - 1) << dofs;
	  memcpy (&dpair.line->b[dofs], &c, min);

	  sleft = CACHELINE_SIZE - sofs;
	  dst--;
	  size -= min;
	}

      while (size >= CACHELINE_SIZE)
	{
	  memcpy (&c.b[sleft], sline, sofs);
	  sline = disp->read_lock(--src, R);
	  memcpy (&c, &sline->b[sofs], sleft);

	  dpair = disp->write_lock(dst, W);
	  *dpair.mask = -1;
	  *dpair.line = c;

	  dst--;
	  size -= CACHELINE_SIZE;
	}

      if (size != 0)
	{
	  dofs = CACHELINE_SIZE - size;

	  memcpy (&c.b[sleft], sline, sofs);
	  if (sleft > dofs)
	    {
	      sline = disp->read_lock(--src, R);
	      memcpy (&c, &sline->b[sofs], sleft);
	    }

	  dpair = disp->write_lock(dst, W);
	  *dpair.mask |= (gtm_cacheline_mask)-1 << dofs;
	  memcpy (&dpair.line->b[dofs], &c.b[dofs], size);
	}
    }
}

#define ITM_MEM_DEF(NAME, READ, WRITE) \
void ITM_REGPARM _ITM_memcpy##NAME(void *dst, const void *src, size_t size)  \
{									     \
  do_memcpy ((uintptr_t)dst, (uintptr_t)src, size,			     \
	     abi_dispatch::WRITE, abi_dispatch::READ);			     \
}									     \
void ITM_REGPARM _ITM_memmove##NAME(void *dst, const void *src, size_t size) \
{									     \
  do_memmove ((uintptr_t)dst, (uintptr_t)src, size,			     \
	      abi_dispatch::WRITE, abi_dispatch::READ);			     \
}

ITM_MEM_DEF(RnWt,	NOLOCK,		W)
ITM_MEM_DEF(RnWtaR,	NOLOCK,		WaR)
ITM_MEM_DEF(RnWtaW,	NOLOCK,		WaW)

ITM_MEM_DEF(RtWn,	R,		NOLOCK)
ITM_MEM_DEF(RtWt,	R,		W)
ITM_MEM_DEF(RtWtaR,	R,		WaR)
ITM_MEM_DEF(RtWtaW,	R,		WaW)

ITM_MEM_DEF(RtaRWn,	RaR,		NOLOCK)
ITM_MEM_DEF(RtaRWt,	RaR,		W)
ITM_MEM_DEF(RtaRWtaR,	RaR,		WaR)
ITM_MEM_DEF(RtaRWtaW,	RaR,		WaW)

ITM_MEM_DEF(RtaWWn,	RaW,		NOLOCK)
ITM_MEM_DEF(RtaWWt,	RaW,		W)
ITM_MEM_DEF(RtaWWtaR,	RaW,		WaR)
ITM_MEM_DEF(RtaWWtaW,	RaW,		WaW)
