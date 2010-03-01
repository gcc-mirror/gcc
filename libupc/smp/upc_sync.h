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

#ifndef _UPC_SYNC_H_
#define _UPC_SYNC_H_

//begin lib_fence_defs

/*

The following table is excerpted from
"Implementing the UPC memory consistency model for
shared-memory architectures", Dan Bonachea et al.

CPU		Write fence		Read fence
--------------------------------------------------
Power/PowerPC	sync			isync
Alpha		wmb			mb
x86		lock; addl $0,0(%%esp)  none reqd.
Athlon/Opteron	mfence			none reqd.
Itanium		mf			none reqd.
SPARC		stbar			none reqd.
MIPS		sync			none reqd.
PA-RISC		SYNC			none reqd. */

#define GUPCR_FENCE() { GUPCR_READ_FENCE (); GUPCR_WRITE_FENCE (); }

#if defined (PPC)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("sync":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("isync":::"memory")
#elif defined (alpha)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("wmb":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("mb":::"memory")
#elif defined (__x86_64__)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("mfence":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#elif defined (__ia64__)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("mf":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#elif defined (i386)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("lock; addl $0,0(%%esp)":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#elif defined (sparc)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("stbar":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#elif defined (mips)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("sync":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#elif defined (hppa)
#define GUPCR_WRITE_FENCE() asm __volatile__ ("SYNC":::"memory")
#define GUPCR_READ_FENCE() asm __volatile__ ("":::"memory")
#else
# error "No memory fence  operations provided for this cpu."
#endif
//end lib_fence_defs

#endif /* _UPC_SYNC_H_ */
