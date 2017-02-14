;   Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

;; 32x32=32 multiply

#include "vregs.h"
 
;----------------------------------------------------------------------

; Register use:
;	RB0	RB1	RB2
; AX	op2L	res32L	res32H
; BC	op2H	(resH)	op1
; DE	count	(resL-tmp)
; HL	[sp+4]

; Register use (G10):
;	
; AX	    op2L
; BC	    op2H
; DE	    count
; HL	    [sp+4]
; r8/r9	    res32L
; r10/r11   (resH)
; r12/r13   (resL-tmp)
; r16/r17   res32H
; r18/r19   op1

START_FUNC ___mulsi3
	;; A is at [sp+4]
	;; B is at [sp+8]
	;; result is in R8..R11

#ifdef __RL78_G10__
	movw	ax, r16
	push	ax
	movw	ax, r18
	push	ax
#else
	sel	rb2
	push	ax
	push	bc
	sel	rb0
#endif
	clrw	ax
	movw	r8, ax
	movw	r16, ax

	movw	ax, [sp+14]
	cmpw	ax, #0
	bz	$1f
	cmpw	ax, #0xffff
	bnz	$2f
	movw	ax, [sp+8]
#ifdef __RL78_G10__
	push    bc
	movw    bc, r8
	xchw	ax, bc
	subw    ax, bc
	movw	r8, ax
	movw    ax, bc
	pop     bc
#else
	sel	rb1
	subw	ax, r_0
	sel	rb0
#endif
	br	$1f
2:	
	movw	bc, ax
	movw	ax, [sp+8]
	cmpw	ax, #0
	skz
	call	!.Lmul_hi
1:	

	movw	ax, [sp+10]
	cmpw	ax, #0
	bz	$1f
	cmpw	ax, #0xffff
	bnz	$2f
	movw	ax, [sp+12]
#ifdef __RL78_G10__
	push    bc
	movw    bc, r8
	xchw	ax, bc
	subw    ax, bc
	movw	r8, ax
	movw    ax, bc
	pop     bc
#else
	sel	rb1
	subw	ax, r_0
	sel	rb0
#endif
	br	$1f
2:	
	movw	bc, ax
	movw	ax, [sp+12]
	cmpw	ax, #0
	skz
	call	!.Lmul_hi
1:	

	movw	ax, r8
	movw	r16, ax
	clrw	ax
	movw	r8, ax

	;; now do R16:R8 += op1L * op2L

	;; op1 is in AX.0 (needs to shrw)
	;; op2 is in BC.2 and BC.1 (bc can shlw/rolcw)
	;; res is in AX.2 and AX.1 (needs to addw)

	movw	ax, [sp+8]
	movw	r10, ax		; BC.1
	movw	ax, [sp+12]

	cmpw	ax, r10
	bc	$.Lmul_hisi_top
	movw	bc, r10
	movw	r10, ax
	movw	ax, bc

.Lmul_hisi_top:
	movw	bc, #0

.Lmul_hisi_loop:
	shrw	ax, 1
#ifdef __RL78_G10__
	push	ax
	bnc	$.Lmul_hisi_no_add_g10
	movw	ax, r8
	addw	ax, r10
	movw	r8, ax
	sknc
	incw	r16
	movw	ax, r16
	addw	ax, r_2
	movw	r16, ax
.Lmul_hisi_no_add_g10:
	movw	ax, r10
	shlw	ax, 1
	movw	r10, ax
	pop	ax
#else
	bnc	$.Lmul_hisi_no_add
	sel	rb1
	addw	ax, bc
	sel	rb2
	sknc
	incw	ax
	addw	ax, r_2
.Lmul_hisi_no_add:
	sel	rb1
	shlw	bc, 1
	sel	rb0
#endif
	rolwc	bc, 1
	cmpw	ax, #0
	bz	$.Lmul_hisi_done

	shrw	ax, 1
#ifdef __RL78_G10__
	push	ax
	bnc	$.Lmul_hisi_no_add2_g10
	movw	ax, r8
	addw	ax, r10
	movw	r8, ax
	movw	ax, r16
	sknc
	incw	ax
	addw	ax, r_2
	movw	r16, ax
.Lmul_hisi_no_add2_g10:
	movw	ax, r10
	shlw	ax, 1
	movw	r10, ax
	pop	ax
#else
	bnc	$.Lmul_hisi_no_add2
	sel	rb1
	addw	ax, bc
	sel	rb2
	sknc
	incw	ax
	addw	ax, r_2
.Lmul_hisi_no_add2:
	sel	rb1
	shlw	bc, 1
	sel	rb0
#endif
	rolwc	bc, 1
	cmpw	ax, #0
	bnz	$.Lmul_hisi_loop

.Lmul_hisi_done:

	movw	ax, r16
	movw	r10, ax

#ifdef __RL78_G10__
	pop	ax
	movw	r18, ax
	pop	ax
	movw	r16, ax
#else
	sel	rb2
	pop	bc
	pop	ax
	sel	rb0
#endif

	ret
END_FUNC ___mulsi3

;----------------------------------------------------------------------

START_FUNC ___mulhi3
	movw	r8, #0
	movw	ax, [sp+6]
	movw	bc, ax
	movw	ax, [sp+4]

	;; R8 += AX * BC
.Lmul_hi:
	cmpw	ax, bc
	skc
	xchw	ax, bc
	br	$.Lmul_hi_loop
	
.Lmul_hi_top:
#ifdef __RL78_G10__
	push	ax
	movw	ax, r8
	addw	ax, r_2
	movw	r8, ax
	pop	ax
#else
	sel	rb1
	addw	ax, r_2
	sel	rb0
#endif

.Lmul_hi_no_add:	
	shlw	bc, 1
.Lmul_hi_loop:
	shrw	ax, 1
	bc	$.Lmul_hi_top
	cmpw	ax, #0
	bz	$.Lmul_hi_done

	shlw	bc, 1
	shrw	ax, 1
	bc	$.Lmul_hi_top
	cmpw	ax, #0
	bnz	$.Lmul_hi_no_add

.Lmul_hi_done:
	ret
END_FUNC ___mulhi3

;;; --------------------------------------
#ifdef __RL78_G10__
	START_FUNC ___mulqi3

       	mov	a, [sp+4]
       	mov	r9, a
       	mov	a, [sp+6]
       	mov	r10, a
       	mov	a, #9
       	mov	r11, a
       	clrb	a
       	mov	r8, a
.L2:
    	cmp0	r10
    	skz
    	dec	r11
    	sknz
    	ret
    	mov	a, r10
    	and	a, #1
    	mov	r12, a
    	cmp0	r12
    	sknz
    	br	!!.L3
    	mov	a, r9
    	mov	l, a
    	mov	a, r8
    	add	a, l
    	mov	r8, a
.L3:
       	mov	a, r9
       	add	a, a
       	mov	r9, a
       	mov	a, r10
       	shr	a, 1
       	mov	r10, a
       	br	!!.L2

	END_FUNC   ___mulqi3
#endif	

