;   Copyright (C) 2012-2024 Free Software Foundation, Inc.
;   Contributed by Red Hat.
; 
; This file is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by the
; Free Software Foundation; either version 3, or (at your option) any
; later version.
; 
; This file is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
; 
; Under Section 7 of GPL version 3, you are granted additional
; permissions described in the GCC Runtime Library Exception, version
; 3.1, as published by the Free Software Foundation.
;
; You should have received a copy of the GNU General Public License and
; a copy of the GCC Runtime Library Exception along with this program;
; see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
; <http://www.gnu.org/licenses/>.

#include "vregs.h"

START_FUNC	___clzhi2
	;; Argument is in [SP+4], return in R8.
	movw	ax, [SP+4]

	.global __clzhi2_internal
__clzhi2_internal:
	movw	r8, #16
	cmpw	ax, #0
	bz	$clzhi2_is_zero
	mov	e, #0xff
1:
	inc	e
	shlw	ax, 1
	bnc	$1b
	mov	a, e
	mov	r8, a
clzhi2_is_zero:
	ret
END_FUNC	___clzhi2


START_FUNC	___clzsi2
	;; Argument is in [SP+6]:[SP+4], return in R8.
	movw	ax, [SP+6]
	cmpw	ax, #0
	bnz	$__clzhi2_internal
	movw	ax, [SP+4]
	call	!__clzhi2_internal
	movw	ax, r8
	addw	ax, #16
	movw	r8, ax
	ret
END_FUNC	___clzsi2


START_FUNC	___ctzhi2
	;; Argument is in [SP+4], return in R8.
	movw	ax, [SP+4]

	.global __ctzhi2_internal
__ctzhi2_internal:
	movw	r8, #16
	cmpw	ax, #0
	bz	$ctzhi2_is_zero
	mov	e, #0xff
1:
	inc	e
	shrw	ax, 1
	bnc	$1b
	mov	a, e
	mov	r8, a
ctzhi2_is_zero:
	ret
END_FUNC	___ctzhi2


START_FUNC	___ctzsi2
	;; Argument is in [SP+6]:[SP+4], return in R8.
	movw	ax, [SP+4]
	cmpw	ax, #0
	bnz	$__ctzhi2_internal
	movw	ax, [SP+6]
	call	!__ctzhi2_internal
	movw	ax, r8
	addw	ax, #16
	movw	r8, ax
	ret
END_FUNC	___ctzsi2


START_FUNC	___ffshi2
	;; Argument is in [SP+4], return in R8.
	movw	ax, [SP+4]

	.global __ffshi2_internal
__ffshi2_internal:
	movw	r8, #0
	cmpw	ax, #0
	bz	$ffshi2_is_zero
	mov	e, #0
1:
	inc	e
	shrw	ax, 1
	bnc	$1b
	mov	a, e
	mov	r8, a
ffshi2_is_zero:
	ret
END_FUNC	___ffshi2


START_FUNC	___ffssi2
	;; Argument is in [SP+6]:[SP+4], return in R8.
	movw	ax, [SP+4]
	cmpw	ax, #0
	bnz	$__ffshi2_internal
	movw	ax, [SP+6]
	cmpw	ax, #0
	bz	$1f
	call	!__ffshi2_internal
	movw	ax, r8
	addw	ax, #16
1:	
	movw	r8, ax
	ret
END_FUNC	___ffssi2


START_FUNC	___parityqi_internal
	mov1	cy, a.0
	xor1	cy, a.1
	xor1	cy, a.2
	xor1	cy, a.3
	xor1	cy, a.4
	xor1	cy, a.5
	xor1	cy, a.6
	xor1	cy, a.7
	movw	ax, #0
	bnc	$1f
	incw	ax
1:
	movw	r8, ax
	ret
END_FUNC	___parityqi_internal


START_FUNC	___parityhi2
	;; Argument is in [SP+4], return in R8.
	movw	ax, [SP+4]
	xor	a, x
	br	$___parityqi_internal
END_FUNC	___parityhi2


START_FUNC	___paritysi2
	;; Argument is in [SP+6]:[SP+4], return in R8.
	movw	ax, [SP+4]
	xor	a, x
	mov	b, a
	movw	ax, [SP+6]
	xor	a, x
	xor	a, b
	br	$___parityqi_internal
END_FUNC	___paritysi2



START_FUNC	___popcounthi2
	;; Argument is in [SP+4], return in R8.
	mov	d, #2
	br	$___popcountqi_internal
END_FUNC	___popcounthi2


START_FUNC	___popcountsi2
	;; Argument is in [SP+6]:[SP+4], return in R8.
	mov	d, #4
	br	$___popcountqi_internal
END_FUNC	___popcountsi2


START_FUNC	___popcountqi_internal
	;; There are D bytes starting at [HL]
	;; store count in R8.

	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	mov	a, #0
1:
	xch	a, b
	mov	a, [hl]
	xch	a, b
	mov	e, #8
2:	
	shl	b,1
	addc	a, #0
	dec	e
	bnz	$2b

	incw	hl
	dec	d
	bnz	$1b

	mov	x, a
	mov	a, #0
	movw	r8, ax
	ret	
END_FUNC	___popcountqi_internal
