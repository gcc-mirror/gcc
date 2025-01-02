;   Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
	
START_FUNC ___lshrsi3
	;; input:
	;; 
	;; [zero]
	;; [count]   <= $sp+8
	;; [in MSB]
	;; [in]
	;; [in]
	;; [in LSB]  <- $sp+4

	;; output:
	;; 
	;; [r8..r11] result

	;; registers:
	;;
	;; AX - temp for shift/rotate
	;; B  - count

	mov	a, [sp+8]	; A now contains the count
	cmp	a, #0x20
	bc	$.Lcount_is_normal

	;; count is out of bounds, just return zero.
	movw	r8, #0
	movw	r10, #0
	ret

.Lcount_is_normal:
	cmp0	a
	bnz	$.Lcount_is_nonzero

	;; count is zero, just copy IN to OUT
	movw	ax, [sp+4]
	movw	r8, ax
	movw	ax, [sp+6]
	movw	r10, ax
	ret

.Lcount_is_nonzero:
	mov	b, a		; B now contains the count also
	bf	a.4, $.Lcount_lt_16

	;; count >= 16, shift 16 at a time.
	movw	r10, #0
	movw	ax, [sp+6]
	movw	r8, ax
	mov	a, b
	and	a, #0x0f
	sknz
	ret

	mov	b, a		; B now contains the remaining count
	inc	b
	br	$.Lloop_top

.Lcount_lt_16:	
	;; count is nonzero.  Do one 
	movw	ax, [sp+6]
	shrw	ax,1
	movw	r10, ax
	mov	a, [sp+5]
	rorc	a,1
	mov	r9, a
	mov	a, [sp+4]
	rorc	a,1
	mov	r8, a

	;; we did one shift above; do as many more as we need now.
.Lloop_top:	
	dec	b
	sknz
	ret

	movw	ax, r10
	shrw	ax,1
	movw	r10, ax
	mov	a, r9
	rorc	a,1
	mov	r9, a
	mov	a, r8
	rorc	a,1
	mov	r8, a

	br	$.Lloop_top

END_FUNC ___lshrsi3
