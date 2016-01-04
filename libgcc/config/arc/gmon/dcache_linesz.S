/* This file contains code to do profiling.

   Copyright (C) 2007-2016 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "../asm.h"
#include "auxreg.h"
/*  This file contains code to do profiling.  */
	.weak	__profile_timer_cycles
	.global __profile_timer_cycles
	.set	__profile_timer_cycles, 200
	.text
	; For Arctangent-A5, if no data cache is present, a read of the
	; cache build register returns the ID register.  For ARC600 and
	; later, the version field will be zero.
	.global	__dcache_linesz
	.balign	4
__dcache_linesz:
#if !defined (__EM__) && !defined (__HS__)
	lr	r12,[D_CACHE_BUILD]
	extb_s	r0,r12
	breq_s	r0,0,.Lsz_nocache
	brge	r0,0x20,.Lsz_havecache
	lr	r0,[IDENTITY]
	breq	r12,r0,.Lsz_nocache
.Lsz_havecache:
	lsr_s	r12,r12,16
	mov_s	r0,16
	bmsk_s	r12,r12,3
	asl_s	r0,r0,r12
	j_s	[blink]
.Lsz_nocache:
#endif /* !__EM__  && !__HS__ */
	mov_s	r0,1
	j_s	[blink]
