/*  Copyright (C) 2014-2024 Free Software Foundation, Inc.
    Contributed by Dimitar Dimitrov <dimitar@dinux.eu>

  This file is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 3, or (at your option) any
  later version.

  This file is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  Under Section 7 of GPL version 3, you are granted additional
  permissions described in the GCC Runtime Library Exception, version
  3.1, as published by the Free Software Foundation.

  You should have received a copy of the GNU General Public License and
  a copy of the GCC Runtime Library Exception along with this program;
  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  <http://www.gnu.org/licenses/>.

      (al + C * ah) * (bl + C * bh) =    al * bl
				       + C * ah * bl
				       + C * al * bh
				       + C * C * ah * bh  -> discard, overflow
	Where C=(1 << 32)
  */

#include "pru-asm.h"

	.section .text.__pruabi_mpyll, "ax"

	.global	SYM(__pruabi_mpyll)
	FUNC(__pruabi_mpyll)
SYM(__pruabi_mpyll):
	/* + C * ah * bl */
	mov	r28, r15
	mov	r29, r16
	nop
	xin	0, r26, 8

	/* + C * al * bh */
	mov	r28, r14
	mov	r29, r17
	mov	r15, r26		/* "Loose" ah, record only reslo.  */
	xin	0, r26, 8

	/* + al * bl */
	/* mov	r28, r14		-> No need, already loaded.  */
	mov	r29, r16
	add	r15, r15, r26
	xin	0, r26, 8
	mov	r14, r26
	add	r15, r15, r27

	ret

	ENDFUNC(__pruabi_mpyll)
