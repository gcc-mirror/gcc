; newlib tls glue code for Synopsys DesignWare ARC cpu.

/* Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifdef __ARC_RF16__
	/* Use object attributes to inform other tools this file is
	safe for RF16 configuration.  */
	.arc_attribute Tag_ARC_ABI_rf16, 1
#endif

#if (__ARC_TLS_REGNO__ != -1)
 /* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

 /* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

#define FUNC(X)         .type SYM(X),@function
#define ENDFUNC0(X)     .Lfe_##X: .size X,.Lfe_##X-X
#define ENDFUNC(X)      ENDFUNC0(X)

	.global SYM(__read_tp)
SYM(__read_tp):
	FUNC(__read_tp)
	mov r0, CONCAT1 (r, __ARC_TLS_REGNO__)
	nop
	j [blink]
	ENDFUNC(__read_tp)

	.section .init
	mov CONCAT1 (r, __ARC_TLS_REGNO__),__main_tcb_end+256

	.section .tbss
__main_tcb:
	.long 0
	.long 0
__main_tcb_end:

#endif /*__ARC_TLS_REGNO__ != -1 */
