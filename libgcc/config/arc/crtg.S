/* Code to start and stop profiling for the Synopsys DesignWare ARC CPU.

   Copyright (C) 1994, 1995, 1997, 2004, 2007-2012
   Free Software Foundation, Inc.
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

	.section .init
	.global _init
	.global _fini
	.global __monstartup
	mov_s	r0,_init
	mov_s	r1,_fini
	jl	__monstartup

	.section .__arc_profile_desc, "a"
	.global __arc_profile_desc_secstart
	.balign	4
__arc_profile_desc_secstart:
	.section .__arc_profile_forward, "a"
	.global __arc_profile_forward_secstart
	.balign 4
__arc_profile_forward_secstart:
	.section .__arc_profile_counters, "aw"
	.global __arc_profile_counters_secstart
	.balign	4
__arc_profile_counters_secstart:

	.section .fini
	.global _mcleanup
	jl	_mcleanup
