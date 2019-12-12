/* Prologue stub for 64-bit ms/sysv clobbers: save
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Daniel Santos <daniel.santos@pobox.com>

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

#include <cet.h>

#ifdef __x86_64__
#include "i386-asm.h"

/* Prologue routine for saving 64-bit ms/sysv registers.  */

	.text
MS2SYSV_STUB_BEGIN(savms64_18)
	mov	%r15,-0x70(%rax)
MS2SYSV_STUB_BEGIN(savms64_17)
	mov	%r14,-0x68(%rax)
MS2SYSV_STUB_BEGIN(savms64_16)
	mov	%r13,-0x60(%rax)
MS2SYSV_STUB_BEGIN(savms64_15)
	mov	%r12,-0x58(%rax)
MS2SYSV_STUB_BEGIN(savms64_14)
	mov	%rbp,-0x50(%rax)
MS2SYSV_STUB_BEGIN(savms64_13)
	mov	%rbx,-0x48(%rax)
MS2SYSV_STUB_BEGIN(savms64_12)
	mov	%rdi,-0x40(%rax)
	mov	%rsi,-0x38(%rax)
	SSE_SAVE
	ret
MS2SYSV_STUB_END(savms64_12)
MS2SYSV_STUB_END(savms64_13)
MS2SYSV_STUB_END(savms64_14)
MS2SYSV_STUB_END(savms64_15)
MS2SYSV_STUB_END(savms64_16)
MS2SYSV_STUB_END(savms64_17)
MS2SYSV_STUB_END(savms64_18)

#endif /* __x86_64__ */
