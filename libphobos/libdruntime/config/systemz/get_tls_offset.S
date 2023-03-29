/* IBM Z support code for TLS offsets.
   Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

/* __tls_get_offset expects the GOT pointer in %r12 and adds
   it to its argument in %r2.  In order to get the offset of
   ti, we load it into %r2, subtract the current %r12 from %r2
   so the addition will result in a nop (%r2 - %r12 + %r12).

   Note: This file is shared between systemz and s390 as the
	 same file is used in multilib case for s390x and s390.  */

#include "../common/threadasm.S"

    .text
    .globl CSYM(__ibmz_get_tls_offset)
    .type CSYM(__ibmz_get_tls_offset), @function
    .align 8
CSYM(__ibmz_get_tls_offset):
    .cfi_startproc

#ifdef __s390x__
     .machinemode zarch
     stmg	%r14,%r15,112(%r15)
     .cfi_offset 14, -48
     .cfi_offset 15, -40
     aghi	%r15,-160
     .cfi_def_cfa_offset 320
     sgr	%r2,%r12
     brasl	%r14,__tls_get_offset@PLT
     lg		%r4,272(%r15)
     lmg	%r14,%r15,272(%r15)
     .cfi_restore 15
     .cfi_restore 14
     .cfi_def_cfa_offset 160
     br	%r4
#else
    .machinemode esa
    stm	%r12,%r15,48(%r15)
    .cfi_offset 12, -48
    .cfi_offset 13, -44
    .cfi_offset 14, -40
    .cfi_offset 15, -36
    ahi	%r15,-96
    .cfi_def_cfa_offset 192
    larl	%r12,_GLOBAL_OFFSET_TABLE_
    sr		%r2,%r12
    brasl	%r14,__tls_get_offset@PLT
    l		%r4,152(%r15)
    lm		%r12,%r15,144(%r15)
    .cfi_restore 15
    .cfi_restore 14
    .cfi_restore 13
    .cfi_restore 12
    .cfi_def_cfa_offset 96
    br	%r4
#endif
     .cfi_endproc
     .size	__ibmz_get_tls_offset, .-__ibmz_get_tls_offset
