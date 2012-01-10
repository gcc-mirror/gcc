;   Copyright (C) 2011 Free Software Foundation, Inc.
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

; real
; GAS defines r0..r7 as aliases for real registers; we want the saddr
; forms here.
r_0	=	0xffef8
r_1	=	0xffef9
r_2	=	0xffefa
r_3	=	0xffefb
r_4	=	0xffefc
r_5	=	0xffefd
r_6	=	0xffefe
r_7	=	0xffeff
; clobberable
r8	=	0xffef0
r9	=	0xffef1
r10	=	0xffef2
r11	=	0xffef3
r12	=	0xffef4
r13	=	0xffef5
r14	=	0xffef6
r15	=	0xffef7
; preserved
r16	=	0xffee8
r17	=	0xffee9
r18	=	0xffeea
r19	=	0xffeeb
r20	=	0xffeec
r21	=	0xffeed
r22	=	0xffeee
r23	=	0xffeef


;----------------------------------------------------------------------

; Register use:
;	RB0	RB1	RB2
; AX	op2L	res32L	res32H
; BC	op2H	(resH)	op1
; DE	count	(resL-tmp)
; HL	[sp+4]

	.text
	nop
	.global	___mulsi3		; (USI a, USI b)
___mulsi3:
	;; A is at [sp+4]
	;; B is at [sp+8]
	;; result is in R8..R11

	movw	ax, sp
	addw	ax, #4
	movw	hl, ax

	sel	rb2
	push	ax
	push	bc
	sel	rb0

	clrw	ax
	movw	r8, ax
	movw	r16, ax

	movw	ax, [hl+6]
	cmpw	ax, #0
	bz	$1f
	cmpw	ax, #0xffff
	bnz	$2f
	movw	ax, [hl]
	sel	rb1
	subw	ax, r_0
	sel	rb0
	br	$1f
2:	
	movw	bc, ax
	movw	ax, [hl]
	cmpw	ax, #0
	skz
	call	!.Lmul_hi
1:	

	movw	ax, [hl+2]
	cmpw	ax, #0
	bz	$1f
	cmpw	ax, #0xffff
	bnz	$2f
	movw	ax, [hl+4]
	sel	rb1
	subw	ax, r_0
	sel	rb0
	br	$1f
2:	
	movw	bc, ax
	movw	ax, [hl+4]
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

	movw	ax, [hl]
	movw	r10, ax		; BC.1
	movw	ax, [hl+4]

	cmpw	ax, r10
	bc	$.Lmul_hisi_top
	movw	bc, r10
	movw	r10, ax
	movw	ax, bc


.Lmul_hisi_top:
	movw	bc, #0

.Lmul_hisi_loop:
	shrw	ax, 1
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
	rolwc	bc, 1
	cmpw	ax, #0
	bz	$.Lmul_hisi_done

	shrw	ax, 1
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
	rolwc	bc, 1
	cmpw	ax, #0
	bnz	$.Lmul_hisi_loop

.Lmul_hisi_done:

	movw	ax, r16
	movw	r10, ax
	
	sel	rb2
	pop	bc
	pop	ax
	sel	rb0

	ret

;----------------------------------------------------------------------

	;; R8 += AX * BC
.Lmul_hi:
	cmpw	ax, bc
	skc
	xchw	ax, bc
	br	$.Lmul_hi_loop
	
.Lmul_hi_top:
	sel	rb1
	addw	ax, r_2
	sel	rb0
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

;----------------------------------------------------------------------

	.global	___mulhi3
___mulhi3:
	sel	rb1
	clrw	ax
	sel	rb0
	movw	ax, sp
	addw	ax, #4
	movw	hl, ax
	movw	ax, [hl+2]
	movw	bc, ax
	movw	ax, [hl]
	br	$.Lmul_hi
