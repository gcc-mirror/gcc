/* Epilogue stub for 64-bit ms/sysv clobbers: restore, leave and return
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

/* Epilogue routine for 64-bit ms/sysv registers when hard frame pointer
 * used -- restores registers, restores frame pointer and then returns
 * from the function.  */

	.text
	cfi_startproc()
	cfi_offset(%rbp, -16)
	cfi_def_cfa(%rbp, 16)
MS2SYSV_STUB_BEGIN(resms64fx_17)
	mov	-0x68(%rsi),%r15
MS2SYSV_STUB_BEGIN(resms64fx_16)
	mov	-0x60(%rsi),%r14
MS2SYSV_STUB_BEGIN(resms64fx_15)
	mov	-0x58(%rsi),%r13
MS2SYSV_STUB_BEGIN(resms64fx_14)
	mov	-0x50(%rsi),%r12
MS2SYSV_STUB_BEGIN(resms64fx_13)
	mov	-0x48(%rsi),%rbx
MS2SYSV_STUB_BEGIN(resms64fx_12)
	mov	-0x40(%rsi),%rdi
	SSE_RESTORE
	mov	-0x38(%rsi),%rsi
	leaveq
	cfi_def_cfa(%rsp, 8)
	ret
	cfi_endproc()
MS2SYSV_STUB_END(resms64fx_12)
MS2SYSV_STUB_END(resms64fx_13)
MS2SYSV_STUB_END(resms64fx_14)
MS2SYSV_STUB_END(resms64fx_15)
MS2SYSV_STUB_END(resms64fx_16)
MS2SYSV_STUB_END(resms64fx_17)

#endif /* __x86_64__ */
