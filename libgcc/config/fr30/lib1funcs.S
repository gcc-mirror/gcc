/* libgcc routines for the FR30.
   Copyright (C) 1998-2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
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
<http://www.gnu.org/licenses/>.  */

	.macro FUNC_START name
	.text
	.globl __\name
	.type  __\name, @function
__\name:
	.endm

	.macro FUNC_END name
	.size  __\name, . - __\name
	.endm

	.macro DIV_BODY reg number
	.if \number
	DIV_BODY  \reg, "\number - 1"
	div1	\reg
	.endif
	.endm
	
#ifdef L_udivsi3
FUNC_START udivsi3
	;; Perform an unsiged division of r4 / r5 and place the result in r4.
	;; Does not handle overflow yet...
	mov	r4, mdl
	div0u	r5
	DIV_BODY r5 32
	mov	mdl, r4
	ret
FUNC_END udivsi3
#endif /* L_udivsi3 */

#ifdef L_divsi3
FUNC_START divsi3
	;; Perform a siged division of r4 / r5 and place the result in r4.
	;; Does not handle overflow yet...
	mov	r4, mdl
	div0s	r5
	DIV_BODY r5 32
	div2    r5
	div3
	div4s
	mov	mdl, r4
	ret
FUNC_END divsi3
#endif /* L_divsi3 */

#ifdef L_umodsi3
FUNC_START umodsi3
	;; Perform an unsiged division of r4 / r5 and places the remainder in r4.
	;; Does not handle overflow yet...
	mov	r4, mdl
	div0u	r5
	DIV_BODY r5 32
	mov	mdh, r4
	ret
FUNC_END umodsi3
#endif /* L_umodsi3 */

#ifdef L_modsi3
FUNC_START modsi3
	;; Perform a siged division of r4 / r5 and place the remainder in r4.
	;; Does not handle overflow yet...
	mov	r4, mdl
	div0s	r5
	DIV_BODY r5 32
	div2    r5
	div3
	div4s
	mov	mdh, r4
	ret
FUNC_END modsi3
#endif /* L_modsi3 */

#ifdef L_negsi2
FUNC_START negsi2
	ldi:8	#0, r0
	sub	r4, r0
	mov	r0, r4
	ret
FUNC_END negsi2
#endif /* L_negsi2 */

#ifdef L_one_cmplsi2
FUNC_START one_cmplsi2
	ldi:8	#0xff, r0
	extsb	r0
	eor	r0, r4
	ret
FUNC_END one_cmplsi2
#endif /* L_one_cmplsi2 */


