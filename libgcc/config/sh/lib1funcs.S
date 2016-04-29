/* Copyright (C) 1994-2016 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.  */


!! libgcc routines for the Renesas / SuperH SH CPUs.
!! Contributed by Steve Chamberlain.
!! sac@cygnus.com

!! ashiftrt_r4_x, ___ashrsi3, ___ashlsi3, ___lshrsi3 routines
!! recoded in assembly by Toshiyasu Morita
!! tm@netcom.com

#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

/* SH2 optimizations for ___ashrsi3, ___ashlsi3, ___lshrsi3 and
   ELF local label prefixes by J"orn Rennecke
   amylaar@cygnus.com  */

#include "lib1funcs.h"

/* t-vxworks needs to build both PIC and non-PIC versions of libgcc,
   so it is more convenient to define NO_FPSCR_VALUES here than to
   define it on the command line.  */
#if defined __vxworks && defined __PIC__
#define NO_FPSCR_VALUES
#endif
	
#ifdef L_ashiftrt
	.global	GLOBAL(ashiftrt_r4_0)
	.global	GLOBAL(ashiftrt_r4_1)
	.global	GLOBAL(ashiftrt_r4_2)
	.global	GLOBAL(ashiftrt_r4_3)
	.global	GLOBAL(ashiftrt_r4_4)
	.global	GLOBAL(ashiftrt_r4_5)
	.global	GLOBAL(ashiftrt_r4_6)
	.global	GLOBAL(ashiftrt_r4_7)
	.global	GLOBAL(ashiftrt_r4_8)
	.global	GLOBAL(ashiftrt_r4_9)
	.global	GLOBAL(ashiftrt_r4_10)
	.global	GLOBAL(ashiftrt_r4_11)
	.global	GLOBAL(ashiftrt_r4_12)
	.global	GLOBAL(ashiftrt_r4_13)
	.global	GLOBAL(ashiftrt_r4_14)
	.global	GLOBAL(ashiftrt_r4_15)
	.global	GLOBAL(ashiftrt_r4_16)
	.global	GLOBAL(ashiftrt_r4_17)
	.global	GLOBAL(ashiftrt_r4_18)
	.global	GLOBAL(ashiftrt_r4_19)
	.global	GLOBAL(ashiftrt_r4_20)
	.global	GLOBAL(ashiftrt_r4_21)
	.global	GLOBAL(ashiftrt_r4_22)
	.global	GLOBAL(ashiftrt_r4_23)
	.global	GLOBAL(ashiftrt_r4_24)
	.global	GLOBAL(ashiftrt_r4_25)
	.global	GLOBAL(ashiftrt_r4_26)
	.global	GLOBAL(ashiftrt_r4_27)
	.global	GLOBAL(ashiftrt_r4_28)
	.global	GLOBAL(ashiftrt_r4_29)
	.global	GLOBAL(ashiftrt_r4_30)
	.global	GLOBAL(ashiftrt_r4_31)
	.global	GLOBAL(ashiftrt_r4_32)

	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_0))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_1))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_2))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_3))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_4))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_5))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_6))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_7))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_8))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_9))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_10))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_11))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_12))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_13))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_14))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_15))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_16))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_17))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_18))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_19))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_20))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_21))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_22))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_23))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_24))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_25))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_26))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_27))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_28))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_29))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_30))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_31))
	HIDDEN_FUNC(GLOBAL(ashiftrt_r4_32))

	.align	1
GLOBAL(ashiftrt_r4_32):
GLOBAL(ashiftrt_r4_31):
	rotcl	r4
	rts
	subc	r4,r4

GLOBAL(ashiftrt_r4_30):
	shar	r4
GLOBAL(ashiftrt_r4_29):
	shar	r4
GLOBAL(ashiftrt_r4_28):
	shar	r4
GLOBAL(ashiftrt_r4_27):
	shar	r4
GLOBAL(ashiftrt_r4_26):
	shar	r4
GLOBAL(ashiftrt_r4_25):
	shar	r4
GLOBAL(ashiftrt_r4_24):
	shlr16	r4
	shlr8	r4
	rts
	exts.b	r4,r4

GLOBAL(ashiftrt_r4_23):
	shar	r4
GLOBAL(ashiftrt_r4_22):
	shar	r4
GLOBAL(ashiftrt_r4_21):
	shar	r4
GLOBAL(ashiftrt_r4_20):
	shar	r4
GLOBAL(ashiftrt_r4_19):
	shar	r4
GLOBAL(ashiftrt_r4_18):
	shar	r4
GLOBAL(ashiftrt_r4_17):
	shar	r4
GLOBAL(ashiftrt_r4_16):
	shlr16	r4
	rts
	exts.w	r4,r4

GLOBAL(ashiftrt_r4_15):
	shar	r4
GLOBAL(ashiftrt_r4_14):
	shar	r4
GLOBAL(ashiftrt_r4_13):
	shar	r4
GLOBAL(ashiftrt_r4_12):
	shar	r4
GLOBAL(ashiftrt_r4_11):
	shar	r4
GLOBAL(ashiftrt_r4_10):
	shar	r4
GLOBAL(ashiftrt_r4_9):
	shar	r4
GLOBAL(ashiftrt_r4_8):
	shar	r4
GLOBAL(ashiftrt_r4_7):
	shar	r4
GLOBAL(ashiftrt_r4_6):
	shar	r4
GLOBAL(ashiftrt_r4_5):
	shar	r4
GLOBAL(ashiftrt_r4_4):
	shar	r4
GLOBAL(ashiftrt_r4_3):
	shar	r4
GLOBAL(ashiftrt_r4_2):
	shar	r4
GLOBAL(ashiftrt_r4_1):
	rts
	shar	r4

GLOBAL(ashiftrt_r4_0):
	rts
	nop

	ENDFUNC(GLOBAL(ashiftrt_r4_0))
	ENDFUNC(GLOBAL(ashiftrt_r4_1))
	ENDFUNC(GLOBAL(ashiftrt_r4_2))
	ENDFUNC(GLOBAL(ashiftrt_r4_3))
	ENDFUNC(GLOBAL(ashiftrt_r4_4))
	ENDFUNC(GLOBAL(ashiftrt_r4_5))
	ENDFUNC(GLOBAL(ashiftrt_r4_6))
	ENDFUNC(GLOBAL(ashiftrt_r4_7))
	ENDFUNC(GLOBAL(ashiftrt_r4_8))
	ENDFUNC(GLOBAL(ashiftrt_r4_9))
	ENDFUNC(GLOBAL(ashiftrt_r4_10))
	ENDFUNC(GLOBAL(ashiftrt_r4_11))
	ENDFUNC(GLOBAL(ashiftrt_r4_12))
	ENDFUNC(GLOBAL(ashiftrt_r4_13))
	ENDFUNC(GLOBAL(ashiftrt_r4_14))
	ENDFUNC(GLOBAL(ashiftrt_r4_15))
	ENDFUNC(GLOBAL(ashiftrt_r4_16))
	ENDFUNC(GLOBAL(ashiftrt_r4_17))
	ENDFUNC(GLOBAL(ashiftrt_r4_18))
	ENDFUNC(GLOBAL(ashiftrt_r4_19))
	ENDFUNC(GLOBAL(ashiftrt_r4_20))
	ENDFUNC(GLOBAL(ashiftrt_r4_21))
	ENDFUNC(GLOBAL(ashiftrt_r4_22))
	ENDFUNC(GLOBAL(ashiftrt_r4_23))
	ENDFUNC(GLOBAL(ashiftrt_r4_24))
	ENDFUNC(GLOBAL(ashiftrt_r4_25))
	ENDFUNC(GLOBAL(ashiftrt_r4_26))
	ENDFUNC(GLOBAL(ashiftrt_r4_27))
	ENDFUNC(GLOBAL(ashiftrt_r4_28))
	ENDFUNC(GLOBAL(ashiftrt_r4_29))
	ENDFUNC(GLOBAL(ashiftrt_r4_30))
	ENDFUNC(GLOBAL(ashiftrt_r4_31))
	ENDFUNC(GLOBAL(ashiftrt_r4_32))
#endif

#ifdef L_ashiftrt_n

!
! GLOBAL(ashrsi3)
!
! Entry:
!
! r4: Value to shift
! r5: Shift count
!
! Exit:
!
! r0: Result
!
! Destroys:
!
! T bit, r5
!

	.global	GLOBAL(ashrsi3)
	HIDDEN_FUNC(GLOBAL(ashrsi3))
	.align	2
GLOBAL(ashrsi3):
	mov	#31,r0
	and	r0,r5
	mova	LOCAL(ashrsi3_table),r0
	mov.b	@(r0,r5),r5
#ifdef __sh1__
	add	r5,r0
	jmp	@r0
#else
	braf	r5
#endif
	mov	r4,r0

	.align	2
LOCAL(ashrsi3_table):
	.byte		LOCAL(ashrsi3_0)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_1)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_2)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_3)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_4)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_5)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_6)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_7)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_8)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_9)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_10)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_11)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_12)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_13)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_14)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_15)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_16)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_17)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_18)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_19)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_20)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_21)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_22)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_23)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_24)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_25)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_26)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_27)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_28)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_29)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_30)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_31)-LOCAL(ashrsi3_table)

LOCAL(ashrsi3_31):
	rotcl	r0
	rts
	subc	r0,r0

LOCAL(ashrsi3_30):
	shar	r0
LOCAL(ashrsi3_29):
	shar	r0
LOCAL(ashrsi3_28):
	shar	r0
LOCAL(ashrsi3_27):
	shar	r0
LOCAL(ashrsi3_26):
	shar	r0
LOCAL(ashrsi3_25):
	shar	r0
LOCAL(ashrsi3_24):
	shlr16	r0
	shlr8	r0
	rts
	exts.b	r0,r0

LOCAL(ashrsi3_23):
	shar	r0
LOCAL(ashrsi3_22):
	shar	r0
LOCAL(ashrsi3_21):
	shar	r0
LOCAL(ashrsi3_20):
	shar	r0
LOCAL(ashrsi3_19):
	shar	r0
LOCAL(ashrsi3_18):
	shar	r0
LOCAL(ashrsi3_17):
	shar	r0
LOCAL(ashrsi3_16):
	shlr16	r0
	rts
	exts.w	r0,r0

LOCAL(ashrsi3_15):
	shar	r0
LOCAL(ashrsi3_14):
	shar	r0
LOCAL(ashrsi3_13):
	shar	r0
LOCAL(ashrsi3_12):
	shar	r0
LOCAL(ashrsi3_11):
	shar	r0
LOCAL(ashrsi3_10):
	shar	r0
LOCAL(ashrsi3_9):
	shar	r0
LOCAL(ashrsi3_8):
	shar	r0
LOCAL(ashrsi3_7):
	shar	r0
LOCAL(ashrsi3_6):
	shar	r0
LOCAL(ashrsi3_5):
	shar	r0
LOCAL(ashrsi3_4):
	shar	r0
LOCAL(ashrsi3_3):
	shar	r0
LOCAL(ashrsi3_2):
	shar	r0
LOCAL(ashrsi3_1):
	rts
	shar	r0

LOCAL(ashrsi3_0):
	rts
	nop

	ENDFUNC(GLOBAL(ashrsi3))
#endif

#ifdef L_ashiftlt

!
! GLOBAL(ashlsi3)
! (For compatibility with older binaries, not used by compiler)
!
! Entry:
!	r4: Value to shift
!	r5: Shift count
!
! Exit:
!	r0: Result
!
! Destroys:
!	T bit
!
!
! GLOBAL(ashlsi3_r0)
!
! Entry:
!	r4: Value to shift
!	r0: Shift count
!
! Exit:
!	r0: Result
!
! Destroys:
!	T bit

	.global	GLOBAL(ashlsi3)
	.global GLOBAL(ashlsi3_r0)
	HIDDEN_FUNC(GLOBAL(ashlsi3))
	HIDDEN_FUNC(GLOBAL(ashlsi3_r0))
GLOBAL(ashlsi3):
	mov	r5,r0
	.align	2
GLOBAL(ashlsi3_r0):

#ifdef __sh1__
	and	#31,r0
	shll2	r0
	mov.l	r4,@-r15
	mov	r0,r4
	mova	LOCAL(ashlsi3_table),r0
	add	r4,r0
	mov.l	@r15+,r4
	jmp	@r0
	mov	r4,r0
	.align 2
#else
	and	#31,r0
	shll2	r0
	braf	r0
	mov	r4,r0
#endif

LOCAL(ashlsi3_table):
	rts				// << 0
	nop
LOCAL(ashlsi_1):
	rts				// << 1
	shll	r0
LOCAL(ashlsi_2):			// << 2
	rts
	shll2	r0
	bra	LOCAL(ashlsi_1)		// << 3
	shll2	r0
	bra	LOCAL(ashlsi_2)		// << 4
	shll2	r0
	bra	LOCAL(ashlsi_5)		// << 5
	shll	r0
	bra	LOCAL(ashlsi_6)		// << 6
	shll2	r0
	bra	LOCAL(ashlsi_7)		// << 7
	shll	r0
LOCAL(ashlsi_8):			// << 8
	rts
	shll8	r0
	bra	LOCAL(ashlsi_8)		// << 9
	shll	r0
	bra	LOCAL(ashlsi_8)		// << 10
	shll2	r0
	bra	LOCAL(ashlsi_11)	// << 11
	shll	r0
	bra	LOCAL(ashlsi_12)	// << 12
	shll2	r0
	bra	LOCAL(ashlsi_13)	// << 13
	shll	r0
	bra	LOCAL(ashlsi_14)	// << 14
	shll8	r0
	bra	LOCAL(ashlsi_15)	// << 15
	shll8	r0
LOCAL(ashlsi_16):			// << 16
	rts
	shll16	r0
	bra	LOCAL(ashlsi_16)	// << 17
	shll	r0
	bra	LOCAL(ashlsi_16)	// << 18
	shll2	r0
	bra	LOCAL(ashlsi_19)	// << 19
	shll	r0
	bra	LOCAL(ashlsi_20)	// << 20
	shll2	r0
	bra	LOCAL(ashlsi_21)	// << 21
	shll	r0
	bra	LOCAL(ashlsi_22)	// << 22
	shll16	r0
	bra	LOCAL(ashlsi_23)	// << 23
	shll16	r0
	bra	LOCAL(ashlsi_16)	// << 24
	shll8	r0
	bra	LOCAL(ashlsi_25)	// << 25
	shll	r0
	bra	LOCAL(ashlsi_26)	// << 26
	shll2	r0
	bra	LOCAL(ashlsi_27)	// << 27
	shll	r0
	bra	LOCAL(ashlsi_28)	// << 28
	shll2	r0
	bra	LOCAL(ashlsi_29)	// << 29
	shll16	r0
	bra	LOCAL(ashlsi_30)	// << 30
	shll16	r0
	and	#1,r0			// << 31
	rts
	rotr	r0

LOCAL(ashlsi_7):
	shll2	r0
LOCAL(ashlsi_5):
LOCAL(ashlsi_6):
	shll2	r0
	rts
LOCAL(ashlsi_13):
	shll2	r0
LOCAL(ashlsi_12):
LOCAL(ashlsi_11):
	shll8	r0
	rts
LOCAL(ashlsi_21):
	shll2	r0
LOCAL(ashlsi_20):
LOCAL(ashlsi_19):
	shll16	r0
	rts
LOCAL(ashlsi_28):
LOCAL(ashlsi_27):
	shll2	r0
LOCAL(ashlsi_26):
LOCAL(ashlsi_25):
	shll16	r0
	rts
	shll8	r0

LOCAL(ashlsi_22):
LOCAL(ashlsi_14):
	shlr2	r0
	rts
	shll8	r0

LOCAL(ashlsi_23):
LOCAL(ashlsi_15):
	shlr	r0
	rts
	shll8	r0

LOCAL(ashlsi_29):
	shlr	r0
LOCAL(ashlsi_30):
	shlr2	r0
	rts
	shll16	r0	

	ENDFUNC(GLOBAL(ashlsi3))
	ENDFUNC(GLOBAL(ashlsi3_r0))
#endif

#ifdef L_lshiftrt

!
! GLOBAL(lshrsi3)
! (For compatibility with older binaries, not used by compiler)
!
! Entry:
!	r4: Value to shift
!	r5: Shift count
!
! Exit:
!	r0: Result
!
! Destroys:
!	T bit
!
!
! GLOBAL(lshrsi3_r0)
!
! Entry:
!	r4: Value to shift
!	r0: Shift count
!
! Exit:
!	r0: Result
!
! Destroys:
!	T bit

	.global	GLOBAL(lshrsi3)
	.global	GLOBAL(lshrsi3_r0)
	HIDDEN_FUNC(GLOBAL(lshrsi3))
	HIDDEN_FUNC(GLOBAL(lshrsi3_r0))
GLOBAL(lshrsi3):
	mov	r5,r0
	.align	2
GLOBAL(lshrsi3_r0):

#ifdef __sh1__
	and	#31,r0
	shll2	r0
	mov.l	r4,@-r15
	mov	r0,r4
	mova	LOCAL(lshrsi3_table),r0
	add	r4,r0
	mov.l	@r15+,r4
	jmp	@r0
	mov	r4,r0
	.align 2
#else
	and	#31,r0
	shll2	r0
	braf	r0
	mov	r4,r0
#endif
LOCAL(lshrsi3_table):
	rts				// >> 0
	nop
LOCAL(lshrsi_1):			// >> 1
	rts
	shlr	r0
LOCAL(lshrsi_2):			// >> 2
	rts
	shlr2	r0
	bra	LOCAL(lshrsi_1)		// >> 3
	shlr2	r0
	bra	LOCAL(lshrsi_2)		// >> 4
	shlr2	r0
	bra	LOCAL(lshrsi_5)		// >> 5
	shlr	r0
	bra	LOCAL(lshrsi_6)		// >> 6
	shlr2	r0
	bra	LOCAL(lshrsi_7)		// >> 7
	shlr	r0
LOCAL(lshrsi_8):			// >> 8
	rts
	shlr8	r0
	bra	LOCAL(lshrsi_8)		// >> 9
	shlr	r0
	bra	LOCAL(lshrsi_8)		// >> 10
	shlr2	r0
	bra	LOCAL(lshrsi_11)	// >> 11
	shlr	r0
	bra	LOCAL(lshrsi_12)	// >> 12
	shlr2	r0
	bra	LOCAL(lshrsi_13)	// >> 13
	shlr	r0
	bra	LOCAL(lshrsi_14)	// >> 14
	shlr8	r0
	bra	LOCAL(lshrsi_15)	// >> 15
	shlr8	r0
LOCAL(lshrsi_16):			// >> 16
	rts
	shlr16	r0
	bra	LOCAL(lshrsi_16)	// >> 17
	shlr	r0
	bra	LOCAL(lshrsi_16)	// >> 18
	shlr2	r0
	bra	LOCAL(lshrsi_19)	// >> 19
	shlr	r0
	bra	LOCAL(lshrsi_20)	// >> 20
	shlr2	r0
	bra	LOCAL(lshrsi_21)	// >> 21
	shlr	r0
	bra	LOCAL(lshrsi_22)	// >> 22
	shlr16	r0
	bra	LOCAL(lshrsi_23)	// >> 23
	shlr16	r0
	bra	LOCAL(lshrsi_16)	// >> 24
	shlr8	r0
	bra	LOCAL(lshrsi_25)	// >> 25
	shlr	r0
	bra	LOCAL(lshrsi_26)	// >> 26
	shlr2	r0
	bra	LOCAL(lshrsi_27)	// >> 27
	shlr	r0
	bra	LOCAL(lshrsi_28)	// >> 28
	shlr2	r0
	bra	LOCAL(lshrsi_29)	// >> 29
	shlr16	r0
	bra	LOCAL(lshrsi_30)	// >> 30
	shlr16	r0
	shll	r0			// >> 31
	rts
	movt	r0

LOCAL(lshrsi_7):
	shlr2	r0
LOCAL(lshrsi_5):
LOCAL(lshrsi_6):
	shlr2	r0
	rts
LOCAL(lshrsi_13):
	shlr2	r0
LOCAL(lshrsi_12):
LOCAL(lshrsi_11):
	shlr8	r0
	rts
LOCAL(lshrsi_21):
	shlr2	r0
LOCAL(lshrsi_20):
LOCAL(lshrsi_19):
	shlr16	r0
	rts
LOCAL(lshrsi_28):
LOCAL(lshrsi_27):
	shlr2	r0
LOCAL(lshrsi_26):
LOCAL(lshrsi_25):
	shlr16	r0
	rts
	shlr8	r0

LOCAL(lshrsi_22):
LOCAL(lshrsi_14):
	shll2	r0
	rts
	shlr8	r0

LOCAL(lshrsi_23):
LOCAL(lshrsi_15):
	shll	r0
	rts
	shlr8	r0

LOCAL(lshrsi_29):
	shll	r0
LOCAL(lshrsi_30):
	shll2	r0
	rts
	shlr16	r0	

	ENDFUNC(GLOBAL(lshrsi3))
	ENDFUNC(GLOBAL(lshrsi3_r0))
#endif

#ifdef L_movmem
	.text
	.balign	4
	.global	GLOBAL(movmem)
	HIDDEN_FUNC(GLOBAL(movmem))
	HIDDEN_ALIAS(movstr,movmem)
	/* This would be a lot simpler if r6 contained the byte count
	   minus 64, and we wouldn't be called here for a byte count of 64.  */
GLOBAL(movmem):
	sts.l	pr,@-r15
	shll2	r6
	bsr	GLOBAL(movmemSI52+2)
	mov.l	@(48,r5),r0
	.balign	4
LOCAL(movmem_loop): /* Reached with rts */
	mov.l	@(60,r5),r0
	add	#-64,r6
	mov.l	r0,@(60,r4)
	tst	r6,r6
	mov.l	@(56,r5),r0
	bt	LOCAL(movmem_done)
	mov.l	r0,@(56,r4)
	cmp/pl	r6
	mov.l	@(52,r5),r0
	add	#64,r5
	mov.l	r0,@(52,r4)
	add	#64,r4
	bt	GLOBAL(movmemSI52)
! done all the large groups, do the remainder
! jump to movmem+
	mova	GLOBAL(movmemSI4)+4,r0
	add	r6,r0
	jmp	@r0
LOCAL(movmem_done): ! share slot insn, works out aligned.
	lds.l	@r15+,pr
	mov.l	r0,@(56,r4)
	mov.l	@(52,r5),r0
	rts
	mov.l	r0,@(52,r4)
	.balign	4
! ??? We need aliases movstr* for movmem* for the older libraries.  These
! aliases will be removed at the some point in the future.
	.global	GLOBAL(movmemSI64)
	HIDDEN_FUNC(GLOBAL(movmemSI64))
	HIDDEN_ALIAS(movstrSI64,movmemSI64)
GLOBAL(movmemSI64):
	mov.l	@(60,r5),r0
	mov.l	r0,@(60,r4)
	.global	GLOBAL(movmemSI60)
	HIDDEN_FUNC(GLOBAL(movmemSI60))
	HIDDEN_ALIAS(movstrSI60,movmemSI60)
GLOBAL(movmemSI60):
	mov.l	@(56,r5),r0
	mov.l	r0,@(56,r4)
	.global	GLOBAL(movmemSI56)
	HIDDEN_FUNC(GLOBAL(movmemSI56))
	HIDDEN_ALIAS(movstrSI56,movmemSI56)
GLOBAL(movmemSI56):
	mov.l	@(52,r5),r0
	mov.l	r0,@(52,r4)
	.global	GLOBAL(movmemSI52)
	HIDDEN_FUNC(GLOBAL(movmemSI52))
	HIDDEN_ALIAS(movstrSI52,movmemSI52)
GLOBAL(movmemSI52):
	mov.l	@(48,r5),r0
	mov.l	r0,@(48,r4)
	.global	GLOBAL(movmemSI48)
	HIDDEN_FUNC(GLOBAL(movmemSI48))
	HIDDEN_ALIAS(movstrSI48,movmemSI48)
GLOBAL(movmemSI48):
	mov.l	@(44,r5),r0
	mov.l	r0,@(44,r4)
	.global	GLOBAL(movmemSI44)
	HIDDEN_FUNC(GLOBAL(movmemSI44))
	HIDDEN_ALIAS(movstrSI44,movmemSI44)
GLOBAL(movmemSI44):
	mov.l	@(40,r5),r0
	mov.l	r0,@(40,r4)
	.global	GLOBAL(movmemSI40)
	HIDDEN_FUNC(GLOBAL(movmemSI40))
	HIDDEN_ALIAS(movstrSI40,movmemSI40)
GLOBAL(movmemSI40):
	mov.l	@(36,r5),r0
	mov.l	r0,@(36,r4)
	.global	GLOBAL(movmemSI36)
	HIDDEN_FUNC(GLOBAL(movmemSI36))
	HIDDEN_ALIAS(movstrSI36,movmemSI36)
GLOBAL(movmemSI36):
	mov.l	@(32,r5),r0
	mov.l	r0,@(32,r4)
	.global	GLOBAL(movmemSI32)
	HIDDEN_FUNC(GLOBAL(movmemSI32))
	HIDDEN_ALIAS(movstrSI32,movmemSI32)
GLOBAL(movmemSI32):
	mov.l	@(28,r5),r0
	mov.l	r0,@(28,r4)
	.global	GLOBAL(movmemSI28)
	HIDDEN_FUNC(GLOBAL(movmemSI28))
	HIDDEN_ALIAS(movstrSI28,movmemSI28)
GLOBAL(movmemSI28):
	mov.l	@(24,r5),r0
	mov.l	r0,@(24,r4)
	.global	GLOBAL(movmemSI24)
	HIDDEN_FUNC(GLOBAL(movmemSI24))
	HIDDEN_ALIAS(movstrSI24,movmemSI24)
GLOBAL(movmemSI24):
	mov.l	@(20,r5),r0
	mov.l	r0,@(20,r4)
	.global	GLOBAL(movmemSI20)
	HIDDEN_FUNC(GLOBAL(movmemSI20))
	HIDDEN_ALIAS(movstrSI20,movmemSI20)
GLOBAL(movmemSI20):
	mov.l	@(16,r5),r0
	mov.l	r0,@(16,r4)
	.global	GLOBAL(movmemSI16)
	HIDDEN_FUNC(GLOBAL(movmemSI16))
	HIDDEN_ALIAS(movstrSI16,movmemSI16)
GLOBAL(movmemSI16):
	mov.l	@(12,r5),r0
	mov.l	r0,@(12,r4)
	.global	GLOBAL(movmemSI12)
	HIDDEN_FUNC(GLOBAL(movmemSI12))
	HIDDEN_ALIAS(movstrSI12,movmemSI12)
GLOBAL(movmemSI12):
	mov.l	@(8,r5),r0
	mov.l	r0,@(8,r4)
	.global	GLOBAL(movmemSI8)
	HIDDEN_FUNC(GLOBAL(movmemSI8))
	HIDDEN_ALIAS(movstrSI8,movmemSI8)
GLOBAL(movmemSI8):
	mov.l	@(4,r5),r0
	mov.l	r0,@(4,r4)
	.global	GLOBAL(movmemSI4)
	HIDDEN_FUNC(GLOBAL(movmemSI4))
	HIDDEN_ALIAS(movstrSI4,movmemSI4)
GLOBAL(movmemSI4):
	mov.l	@(0,r5),r0
	rts
	mov.l	r0,@(0,r4)

	ENDFUNC(GLOBAL(movmemSI64))
	ENDFUNC(GLOBAL(movmemSI60))
	ENDFUNC(GLOBAL(movmemSI56))
	ENDFUNC(GLOBAL(movmemSI52))
	ENDFUNC(GLOBAL(movmemSI48))
	ENDFUNC(GLOBAL(movmemSI44))
	ENDFUNC(GLOBAL(movmemSI40))
	ENDFUNC(GLOBAL(movmemSI36))
	ENDFUNC(GLOBAL(movmemSI32))
	ENDFUNC(GLOBAL(movmemSI28))
	ENDFUNC(GLOBAL(movmemSI24))
	ENDFUNC(GLOBAL(movmemSI20))
	ENDFUNC(GLOBAL(movmemSI16))
	ENDFUNC(GLOBAL(movmemSI12))
	ENDFUNC(GLOBAL(movmemSI8))
	ENDFUNC(GLOBAL(movmemSI4))
	ENDFUNC(GLOBAL(movmem))
#endif

#ifdef L_movmem_i4
	.text
	.global	GLOBAL(movmem_i4_even)
	.global	GLOBAL(movmem_i4_odd)
	.global	GLOBAL(movmemSI12_i4)

	HIDDEN_FUNC(GLOBAL(movmem_i4_even))
	HIDDEN_FUNC(GLOBAL(movmem_i4_odd))
	HIDDEN_FUNC(GLOBAL(movmemSI12_i4))

	HIDDEN_ALIAS(movstr_i4_even,movmem_i4_even)
	HIDDEN_ALIAS(movstr_i4_odd,movmem_i4_odd)
	HIDDEN_ALIAS(movstrSI12_i4,movmemSI12_i4)

	.p2align	5
L_movmem_2mod4_end:
	mov.l	r0,@(16,r4)
	rts
	mov.l	r1,@(20,r4)

	.p2align	2

GLOBAL(movmem_i4_even):
	mov.l	@r5+,r0
	bra	L_movmem_start_even
	mov.l	@r5+,r1

GLOBAL(movmem_i4_odd):
	mov.l	@r5+,r1
	add	#-4,r4
	mov.l	@r5+,r2
	mov.l	@r5+,r3
	mov.l	r1,@(4,r4)
	mov.l	r2,@(8,r4)

L_movmem_loop:
	mov.l	r3,@(12,r4)
	dt	r6
	mov.l	@r5+,r0
	bt/s	L_movmem_2mod4_end
	mov.l	@r5+,r1
	add	#16,r4
L_movmem_start_even:
	mov.l	@r5+,r2
	mov.l	@r5+,r3
	mov.l	r0,@r4
	dt	r6
	mov.l	r1,@(4,r4)
	bf/s	L_movmem_loop
	mov.l	r2,@(8,r4)
	rts
	mov.l	r3,@(12,r4)

	ENDFUNC(GLOBAL(movmem_i4_even))
	ENDFUNC(GLOBAL(movmem_i4_odd))

	.p2align	4
GLOBAL(movmemSI12_i4):
	mov.l	@r5,r0
	mov.l	@(4,r5),r1
	mov.l	@(8,r5),r2
	mov.l	r0,@r4
	mov.l	r1,@(4,r4)
	rts
	mov.l	r2,@(8,r4)

	ENDFUNC(GLOBAL(movmemSI12_i4))
#endif

#ifdef L_mulsi3


	.global	GLOBAL(mulsi3)
	HIDDEN_FUNC(GLOBAL(mulsi3))

! r4 =       aabb
! r5 =       ccdd
! r0 = aabb*ccdd  via partial products
!
! if aa == 0 and cc = 0
! r0 = bb*dd
!
! else
! aa = bb*dd + (aa*dd*65536) + (cc*bb*65536)
!

GLOBAL(mulsi3):
	mulu.w  r4,r5		! multiply the lsws  macl=bb*dd
	mov     r5,r3		! r3 = ccdd
	swap.w  r4,r2		! r2 = bbaa
	xtrct   r2,r3		! r3 = aacc
	tst  	r3,r3		! msws zero ?
	bf      hiset
	rts			! yes - then we have the answer
	sts     macl,r0

hiset:	sts	macl,r0		! r0 = bb*dd
	mulu.w	r2,r5		! brewing macl = aa*dd
	sts	macl,r1
	mulu.w	r3,r4		! brewing macl = cc*bb
	sts	macl,r2
	add	r1,r2
	shll16	r2
	rts
	add	r2,r0

	ENDFUNC(GLOBAL(mulsi3))
#endif

/*------------------------------------------------------------------------------
  32 bit signed integer division that uses FPU double precision division.  */

#ifdef L_sdivsi3_i4
	.title "SH DIVIDE"

#if defined (__SH4__) || defined (__SH2A__)
/* This variant is used when FPSCR.PR = 1 (double precision) is the default
   setting.
   Args in r4 and r5, result in fpul, clobber dr0, dr2.  */

	.global	GLOBAL(sdivsi3_i4)
	HIDDEN_FUNC(GLOBAL(sdivsi3_i4))
GLOBAL(sdivsi3_i4):
	lds r4,fpul
	float fpul,dr0
	lds r5,fpul
	float fpul,dr2
	fdiv dr2,dr0
	rts
	ftrc dr0,fpul

	ENDFUNC(GLOBAL(sdivsi3_i4))

#elif defined (__SH2A_SINGLE__) || defined (__SH2A_SINGLE_ONLY__) || defined(__SH4_SINGLE__) || defined(__SH4_SINGLE_ONLY__)
/* This variant is used when FPSCR.PR = 0 (sigle precision) is the default
   setting.
   Args in r4 and r5, result in fpul, clobber r2, dr0, dr2.
   For this to work, we must temporarily switch the FPU do double precision,
   but we better do not touch FPSCR.FR.  See PR 6526.  */

	.global	GLOBAL(sdivsi3_i4)
	HIDDEN_FUNC(GLOBAL(sdivsi3_i4))
GLOBAL(sdivsi3_i4):

#ifndef __SH4A__
	mov.l	r3,@-r15
	sts	fpscr,r2
	mov	#8,r3
	swap.w	r3,r3		// r3 = 1 << 19 (FPSCR.PR bit)
	or	r2,r3
	lds	r3,fpscr	// Set FPSCR.PR = 1.
	lds	r4,fpul
	float	fpul,dr0
	lds	r5,fpul
	float	fpul,dr2
	fdiv	dr2,dr0
	ftrc	dr0,fpul
	lds	r2,fpscr
	rts
	mov.l	@r15+,r3
#else
/* On SH4A we can use the fpchg instruction to flip the FPSCR.PR bit.  */
	fpchg
	lds	r4,fpul
	float	fpul,dr0
	lds	r5,fpul
	float	fpul,dr2
	fdiv	dr2,dr0
	ftrc	dr0,fpul
	rts
	fpchg	

#endif /* __SH4A__  */

	ENDFUNC(GLOBAL(sdivsi3_i4))
#endif /* ! __SH4__ || __SH2A__  */
#endif /* L_sdivsi3_i4  */

//------------------------------------------------------------------------------
#ifdef L_sdivsi3
/* __SH4_SINGLE_ONLY__ keeps this part for link compatibility with
   sh2e/sh3e code.  */
!!
!! Steve Chamberlain
!! sac@cygnus.com
!!
!!

!! args in r4 and r5, result in r0 clobber r1, r2, r3, and t bit

	.global	GLOBAL(sdivsi3)
	.align	2

	FUNC(GLOBAL(sdivsi3))
GLOBAL(sdivsi3):
	mov	r4,r1
	mov	r5,r0

	tst	r0,r0
	bt	div0
	mov	#0,r2
	div0s	r2,r1
	subc	r3,r3
	subc	r2,r1
	div0s	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	addc	r2,r1
	rts
	mov	r1,r0


div0:	rts
	mov	#0,r0

	ENDFUNC(GLOBAL(sdivsi3))
#endif /* L_sdivsi3  */

/*------------------------------------------------------------------------------
  32 bit unsigned integer division that uses FPU double precision division.  */

#ifdef L_udivsi3_i4
	.title "SH DIVIDE"

#if defined (__SH4__) || defined (__SH2A__)
/* This variant is used when FPSCR.PR = 1 (double precision) is the default
   setting.
   Args in r4 and r5, result in fpul,
   clobber r0, r1, r4, r5, dr0, dr2, dr4, and t bit  */

	.global	GLOBAL(udivsi3_i4)
	HIDDEN_FUNC(GLOBAL(udivsi3_i4))
GLOBAL(udivsi3_i4):
	mov	#1,r1
	cmp/hi	r1,r5
	bf/s	trivial
	rotr	r1
	xor	r1,r4
	lds	r4,fpul
	mova	L1,r0
#ifdef FMOVD_WORKS
	fmov.d	@r0+,dr4
#else
	fmov.s	@r0+,DR40
	fmov.s	@r0,DR41
#endif
	float	fpul,dr0
	xor	r1,r5
	lds	r5,fpul
	float	fpul,dr2
	fadd	dr4,dr0
	fadd	dr4,dr2
	fdiv	dr2,dr0
	rts
	ftrc	dr0,fpul

trivial:
	rts
	lds	r4,fpul

	.align 2
#ifdef FMOVD_WORKS
	.align 3	// Make the double below 8 byte aligned.
#endif
L1:
	.double 2147483648

	ENDFUNC(GLOBAL(udivsi3_i4))

#elif defined (__SH2A_SINGLE__) || defined (__SH2A_SINGLE_ONLY__) || defined(__SH4_SINGLE__) || defined(__SH4_SINGLE_ONLY__)
/* This variant is used when FPSCR.PR = 0 (sigle precision) is the default
   setting.
   Args in r4 and r5, result in fpul,
   clobber r0, r1, r4, r5, dr0, dr2, dr4.
   For this to work, we must temporarily switch the FPU do double precision,
   but we better do not touch FPSCR.FR.  See PR 6526.  */

	.global	GLOBAL(udivsi3_i4)
	HIDDEN_FUNC(GLOBAL(udivsi3_i4))
GLOBAL(udivsi3_i4):

#ifndef __SH4A__
	mov	#1,r1
	cmp/hi	r1,r5
	bf/s	trivial
	rotr	r1		// r1 = 1 << 31
	sts.l	fpscr,@-r15
	xor	r1,r4
	mov.l	@(0,r15),r0
	xor	r1,r5
	mov.l	L2,r1
	lds	r4,fpul
	or	r0,r1
	mova	L1,r0
	lds	r1,fpscr
#ifdef FMOVD_WORKS
	fmov.d	@r0+,dr4
#else
	fmov.s	@r0+,DR40
	fmov.s	@r0,DR41
#endif
	float	fpul,dr0
	lds	r5,fpul
	float	fpul,dr2
	fadd	dr4,dr0
	fadd	dr4,dr2
	fdiv	dr2,dr0
	ftrc	dr0,fpul
	rts
	lds.l	@r15+,fpscr

#ifdef FMOVD_WORKS
	.align 3	// Make the double below 8 byte aligned.
#endif
trivial:
	rts
	lds	r4,fpul

	.align 2
L2:
#ifdef FMOVD_WORKS
	.long 0x180000	// FPSCR.PR = 1, FPSCR.SZ = 1
#else
	.long 0x80000	// FPSCR.PR = 1
#endif
L1:
	.double 2147483648

#else
/* On SH4A we can use the fpchg instruction to flip the FPSCR.PR bit.
   Although on SH4A fmovd usually works, it would require either additional
   two fschg instructions or an FPSCR push + pop.  It's not worth the effort
   for loading only one double constant.  */
	mov	#1,r1
	cmp/hi	r1,r5
	bf/s	trivial
	rotr	r1		// r1 = 1 << 31
	fpchg
	mova	L1,r0
	xor	r1,r4
	fmov.s	@r0+,DR40
	lds	r4,fpul
	fmov.s	@r0,DR41
	xor	r1,r5
	float	fpul,dr0
	lds	r5,fpul
	float	fpul,dr2
	fadd	dr4,dr0
	fadd	dr4,dr2
	fdiv	dr2,dr0
	ftrc	dr0,fpul
	rts
	fpchg

trivial:
	rts
	lds	r4,fpul

	.align 2
L1:
	.double 2147483648

#endif /* __SH4A__  */


	ENDFUNC(GLOBAL(udivsi3_i4))
#endif /* ! __SH4__ */
#endif /* L_udivsi3_i4  */

#ifdef L_udivsi3
/* __SH4_SINGLE_ONLY__ keeps this part for link compatibility with
   sh2e/sh3e code.  */

!! args in r4 and r5, result in r0, clobbers r4, pr, and t bit
	.global	GLOBAL(udivsi3)
	HIDDEN_FUNC(GLOBAL(udivsi3))

LOCAL(div8):
 div1 r5,r4
LOCAL(div7):
 div1 r5,r4; div1 r5,r4; div1 r5,r4
 div1 r5,r4; div1 r5,r4; div1 r5,r4; rts; div1 r5,r4

LOCAL(divx4):
 div1 r5,r4; rotcl r0
 div1 r5,r4; rotcl r0
 div1 r5,r4; rotcl r0
 rts; div1 r5,r4

GLOBAL(udivsi3):
 sts.l pr,@-r15
 extu.w r5,r0
 cmp/eq r5,r0
#ifdef __sh1__
 bf LOCAL(large_divisor)
#else
 bf/s LOCAL(large_divisor)
#endif
 div0u
 swap.w r4,r0
 shlr16 r4
 bsr LOCAL(div8)
 shll16 r5
 bsr LOCAL(div7)
 div1 r5,r4
 xtrct r4,r0
 xtrct r0,r4
 bsr LOCAL(div8)
 swap.w r4,r4
 bsr LOCAL(div7)
 div1 r5,r4
 lds.l @r15+,pr
 xtrct r4,r0
 swap.w r0,r0
 rotcl r0
 rts
 shlr16 r5

LOCAL(large_divisor):
#ifdef __sh1__
 div0u
#endif
 mov #0,r0
 xtrct r4,r0
 xtrct r0,r4
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 lds.l @r15+,pr
 rts
 rotcl r0

	ENDFUNC(GLOBAL(udivsi3))
#endif /* L_udivsi3 */

#ifdef L_set_fpscr
#if !defined (__SH2A_NOFPU__)
#if defined (__SH2E__) || defined (__SH2A__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
	.global GLOBAL(set_fpscr)
	HIDDEN_FUNC(GLOBAL(set_fpscr))
GLOBAL(set_fpscr):
	lds r4,fpscr
#ifdef __PIC__
	mov.l	r12,@-r15
#ifdef __vxworks
	mov.l	LOCAL(set_fpscr_L0_base),r12
	mov.l	LOCAL(set_fpscr_L0_index),r0
	mov.l	@r12,r12
	mov.l	@(r0,r12),r12
#else
	mova	LOCAL(set_fpscr_L0),r0
	mov.l	LOCAL(set_fpscr_L0),r12
	add	r0,r12
#endif
	mov.l	LOCAL(set_fpscr_L1),r0
	mov.l	@(r0,r12),r1
	mov.l	@r15+,r12
#else
	mov.l LOCAL(set_fpscr_L1),r1
#endif
	swap.w r4,r0
	or #24,r0
#ifndef FMOVD_WORKS
	xor #16,r0
#endif
#if defined(__SH4__) || defined (__SH2A_DOUBLE__)
	swap.w r0,r3
	mov.l r3,@(4,r1)
#else /* defined (__SH2E__) || defined(__SH3E__) || defined(__SH4_SINGLE*__) */
	swap.w r0,r2
	mov.l r2,@r1
#endif
#ifndef FMOVD_WORKS
	xor #8,r0
#else
	xor #24,r0
#endif
#if defined(__SH4__) || defined (__SH2A_DOUBLE__)
	swap.w r0,r2
	rts
	mov.l r2,@r1
#else /* defined(__SH2E__) || defined(__SH3E__) || defined(__SH4_SINGLE*__) */
	swap.w r0,r3
	rts
	mov.l r3,@(4,r1)
#endif
	.align 2
#ifdef __PIC__
#ifdef __vxworks
LOCAL(set_fpscr_L0_base):
	.long ___GOTT_BASE__
LOCAL(set_fpscr_L0_index):
	.long ___GOTT_INDEX__
#else
LOCAL(set_fpscr_L0):
	.long _GLOBAL_OFFSET_TABLE_
#endif
LOCAL(set_fpscr_L1):
	.long GLOBAL(fpscr_values@GOT)
#else
LOCAL(set_fpscr_L1):
	.long GLOBAL(fpscr_values)
#endif

	ENDFUNC(GLOBAL(set_fpscr))
#ifndef NO_FPSCR_VALUES
#ifdef __ELF__
        .comm   GLOBAL(fpscr_values),8,4
#else
        .comm   GLOBAL(fpscr_values),8
#endif /* ELF */
#endif /* NO_FPSCR_VALUES */
#endif /* SH2E / SH3E / SH4 */
#endif /* __SH2A_NOFPU__ */
#endif /* L_set_fpscr */
#ifdef L_ic_invalidate

#if defined(__SH4A__)
	.global GLOBAL(ic_invalidate)
	HIDDEN_FUNC(GLOBAL(ic_invalidate))
GLOBAL(ic_invalidate):
	ocbwb	@r4
	synco
	icbi	@r4
	rts
	  nop
	ENDFUNC(GLOBAL(ic_invalidate))
#elif defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) || defined(__SH4_NOFPU__)
	/* For system code, we use ic_invalidate_line_i, but user code
	   needs a different mechanism.  A kernel call is generally not
	   available, and it would also be slow.  Different SH4 variants use
	   different sizes and associativities of the Icache.  We use a small
	   bit of dispatch code that can be put hidden in every shared object,
	   which calls the actual processor-specific invalidation code in a
	   separate module.
	   Or if you have operating system support, the OS could mmap the
	   procesor-specific code from a single page, since it is highly
	   repetitive.  */
	.global GLOBAL(ic_invalidate)
	HIDDEN_FUNC(GLOBAL(ic_invalidate))
GLOBAL(ic_invalidate):
#ifdef __pic__
#ifdef __vxworks
	mov.l	1f,r1
	mov.l	2f,r0
	mov.l	@r1,r1
	mov.l	0f,r2
	mov.l	@(r0,r1),r0
#else
	mov.l	1f,r1
	mova	1f,r0
	mov.l	0f,r2
	add	r1,r0
#endif
	mov.l	@(r0,r2),r1
#else
	mov.l	0f,r1
#endif
	ocbwb	@r4
	mov.l	@(8,r1),r0
	sub	r1,r4
	and	r4,r0
	add	r1,r0
	jmp	@r0
	mov.l	@(4,r1),r0
	.align	2
#ifndef __pic__
0:	.long   GLOBAL(ic_invalidate_array)
#else /* __pic__ */
	.global GLOBAL(ic_invalidate_array)
0:	.long   GLOBAL(ic_invalidate_array)@GOT
#ifdef __vxworks
1:	.long	___GOTT_BASE__
2:	.long	___GOTT_INDEX__
#else
1:	.long   _GLOBAL_OFFSET_TABLE_
#endif
	ENDFUNC(GLOBAL(ic_invalidate))
#endif /* __pic__ */
#endif /* SH4 */
#endif /* L_ic_invalidate */

#ifdef L_ic_invalidate_array
#if defined(__SH4A__) || (defined (__FORCE_SH4A__) && (defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) || defined(__SH4_NOFPU__)))
	.global GLOBAL(ic_invalidate_array)
	/* This is needed when an SH4 dso with trampolines is used on SH4A.  */
	.global GLOBAL(ic_invalidate_array)
	FUNC(GLOBAL(ic_invalidate_array))
GLOBAL(ic_invalidate_array):
	add	r1,r4
	synco
	icbi	@r4
	rts
	  nop
	.align 2
	.long	0
	ENDFUNC(GLOBAL(ic_invalidate_array))
#elif defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) || defined(__SH4_NOFPU__)
	.global GLOBAL(ic_invalidate_array)
	.p2align 5
	FUNC(GLOBAL(ic_invalidate_array))
/* This must be aligned to the beginning of a cache line.  */
GLOBAL(ic_invalidate_array):
#ifndef WAYS
#define WAYS 4
#define WAY_SIZE 0x4000
#endif
#if WAYS == 1
	.rept	WAY_SIZE * WAYS / 32
	rts
	nop
	.rept	7
	.long	WAY_SIZE - 32
	.endr
	.endr
#elif WAYS <= 6
	.rept	WAY_SIZE * WAYS / 32
	braf	r0
	add	#-8,r0
	.long	WAY_SIZE + 8
	.long	WAY_SIZE - 32
	.rept	WAYS-2
	braf	r0
	nop
	.endr
	.rept	7 - WAYS
	rts
	nop
	.endr
	.endr
#else /* WAYS > 6 */
	/* This variant needs two different pages for mmap-ing.  */
 	.rept	WAYS-1
	.rept	WAY_SIZE / 32
	braf	r0
	nop
	.long	WAY_SIZE
	.rept 6
	.long	WAY_SIZE - 32
	.endr
	.endr
	.endr
	.rept	WAY_SIZE / 32
	rts
	.rept	15
	nop
	.endr
	.endr
#endif /* WAYS */
	ENDFUNC(GLOBAL(ic_invalidate_array))
#endif /* SH4 */
#endif /* L_ic_invalidate_array */


#ifdef L_div_table

#if defined (__SH2A__) || defined (__SH3__) || defined (__SH3E__) || defined (__SH4__) || defined (__SH4_SINGLE__) || defined (__SH4_SINGLE_ONLY__) || defined (__SH4_NOFPU__)
/* This code uses shld, thus is not suitable for SH1 / SH2.  */

/* Signed / unsigned division without use of FPU, optimized for SH4.
   Uses a lookup table for divisors in the range -128 .. +128, and
   div1 with case distinction for larger divisors in three more ranges.
   The code is lumped together with the table to allow the use of mova.  */
#ifdef __LITTLE_ENDIAN__
#define L_LSB 0
#define L_LSWMSB 1
#define L_MSWLSB 2
#else
#define L_LSB 3
#define L_LSWMSB 2
#define L_MSWLSB 1
#endif

	.balign 4
	.global	GLOBAL(udivsi3_i4i)
	FUNC(GLOBAL(udivsi3_i4i))
GLOBAL(udivsi3_i4i):
	mov.w LOCAL(c128_w), r1
	div0u
	mov r4,r0
	shlr8 r0
	cmp/hi r1,r5
	extu.w r5,r1
	bf LOCAL(udiv_le128)
	cmp/eq r5,r1
	bf LOCAL(udiv_ge64k)
	shlr r0
	mov r5,r1
	shll16 r5
	mov.l r4,@-r15
	div1 r5,r0
	mov.l r1,@-r15
	div1 r5,r0
	div1 r5,r0
	bra LOCAL(udiv_25)
	div1 r5,r0

LOCAL(div_le128):
	mova LOCAL(div_table_ix),r0
	bra LOCAL(div_le128_2)
	mov.b @(r0,r5),r1
LOCAL(udiv_le128):
	mov.l r4,@-r15
	mova LOCAL(div_table_ix),r0
	mov.b @(r0,r5),r1
	mov.l r5,@-r15
LOCAL(div_le128_2):
	mova LOCAL(div_table_inv),r0
	mov.l @(r0,r1),r1
	mov r5,r0
	tst #0xfe,r0
	mova LOCAL(div_table_clz),r0
	dmulu.l r1,r4
	mov.b @(r0,r5),r1
	bt/s LOCAL(div_by_1)
	mov r4,r0
	mov.l @r15+,r5
	sts mach,r0
	/* clrt */
	addc r4,r0
	mov.l @r15+,r4
	rotcr r0
	rts
	shld r1,r0

LOCAL(div_by_1_neg):
	neg r4,r0
LOCAL(div_by_1):
	mov.l @r15+,r5
	rts
	mov.l @r15+,r4

LOCAL(div_ge64k):
	bt/s LOCAL(div_r8)
	div0u
	shll8 r5
	bra LOCAL(div_ge64k_2)
	div1 r5,r0
LOCAL(udiv_ge64k):
	cmp/hi r0,r5
	mov r5,r1
	bt LOCAL(udiv_r8)
	shll8 r5
	mov.l r4,@-r15
	div1 r5,r0
	mov.l r1,@-r15
LOCAL(div_ge64k_2):
	div1 r5,r0
	mov.l LOCAL(zero_l),r1
	.rept 4
	div1 r5,r0
	.endr
	mov.l r1,@-r15
	div1 r5,r0
	mov.w LOCAL(m256_w),r1
	div1 r5,r0
	mov.b r0,@(L_LSWMSB,r15)
	xor r4,r0
	and r1,r0
	bra LOCAL(div_ge64k_end)
	xor r4,r0
	
LOCAL(div_r8):
	shll16 r4
	bra LOCAL(div_r8_2)
	shll8 r4
LOCAL(udiv_r8):
	mov.l r4,@-r15
	shll16 r4
	clrt
	shll8 r4
	mov.l r5,@-r15
LOCAL(div_r8_2):
	rotcl r4
	mov r0,r1
	div1 r5,r1
	mov r4,r0
	rotcl r0
	mov r5,r4
	div1 r5,r1
	.rept 5
	rotcl r0; div1 r5,r1
	.endr
	rotcl r0
	mov.l @r15+,r5
	div1 r4,r1
	mov.l @r15+,r4
	rts
	rotcl r0

	ENDFUNC(GLOBAL(udivsi3_i4i))

	.global	GLOBAL(sdivsi3_i4i)
	FUNC(GLOBAL(sdivsi3_i4i))
	/* This is link-compatible with a GLOBAL(sdivsi3) call,
	   but we effectively clobber only r1.  */
GLOBAL(sdivsi3_i4i):
	mov.l r4,@-r15
	cmp/pz r5
	mov.w LOCAL(c128_w), r1
	bt/s LOCAL(pos_divisor)
	cmp/pz r4
	mov.l r5,@-r15
	neg r5,r5
	bt/s LOCAL(neg_result)
	cmp/hi r1,r5
	neg r4,r4
LOCAL(pos_result):
	extu.w r5,r0
	bf LOCAL(div_le128)
	cmp/eq r5,r0
	mov r4,r0
	shlr8 r0
	bf/s LOCAL(div_ge64k)
	cmp/hi r0,r5
	div0u
	shll16 r5
	div1 r5,r0
	div1 r5,r0
	div1 r5,r0
LOCAL(udiv_25):
	mov.l LOCAL(zero_l),r1
	div1 r5,r0
	div1 r5,r0
	mov.l r1,@-r15
	.rept 3
	div1 r5,r0
	.endr
	mov.b r0,@(L_MSWLSB,r15)
	xtrct r4,r0
	swap.w r0,r0
	.rept 8
	div1 r5,r0
	.endr
	mov.b r0,@(L_LSWMSB,r15)
LOCAL(div_ge64k_end):
	.rept 8
	div1 r5,r0
	.endr
	mov.l @r15+,r4 ! zero-extension and swap using LS unit.
	extu.b r0,r0
	mov.l @r15+,r5
	or r4,r0
	mov.l @r15+,r4
	rts
	rotcl r0

LOCAL(div_le128_neg):
	tst #0xfe,r0
	mova LOCAL(div_table_ix),r0
	mov.b @(r0,r5),r1
	mova LOCAL(div_table_inv),r0
	bt/s LOCAL(div_by_1_neg)
	mov.l @(r0,r1),r1
	mova LOCAL(div_table_clz),r0
	dmulu.l r1,r4
	mov.b @(r0,r5),r1
	mov.l @r15+,r5
	sts mach,r0
	/* clrt */
	addc r4,r0
	mov.l @r15+,r4
	rotcr r0
	shld r1,r0
	rts
	neg r0,r0

LOCAL(pos_divisor):
	mov.l r5,@-r15
	bt/s LOCAL(pos_result)
	cmp/hi r1,r5
	neg r4,r4
LOCAL(neg_result):
	extu.w r5,r0
	bf LOCAL(div_le128_neg)
	cmp/eq r5,r0
	mov r4,r0
	shlr8 r0
	bf/s LOCAL(div_ge64k_neg)
	cmp/hi r0,r5
	div0u
	mov.l LOCAL(zero_l),r1
	shll16 r5
	div1 r5,r0
	mov.l r1,@-r15
	.rept 7
	div1 r5,r0
	.endr
	mov.b r0,@(L_MSWLSB,r15)
	xtrct r4,r0
	swap.w r0,r0
	.rept 8
	div1 r5,r0
	.endr
	mov.b r0,@(L_LSWMSB,r15)
LOCAL(div_ge64k_neg_end):
	.rept 8
	div1 r5,r0
	.endr
	mov.l @r15+,r4 ! zero-extension and swap using LS unit.
	extu.b r0,r1
	mov.l @r15+,r5
	or r4,r1
LOCAL(div_r8_neg_end):
	mov.l @r15+,r4
	rotcl r1
	rts
	neg r1,r0

LOCAL(div_ge64k_neg):
	bt/s LOCAL(div_r8_neg)
	div0u
	shll8 r5
	mov.l LOCAL(zero_l),r1
	.rept 6
	div1 r5,r0
	.endr
	mov.l r1,@-r15
	div1 r5,r0
	mov.w LOCAL(m256_w),r1
	div1 r5,r0
	mov.b r0,@(L_LSWMSB,r15)
	xor r4,r0
	and r1,r0
	bra LOCAL(div_ge64k_neg_end)
	xor r4,r0

LOCAL(c128_w):
	.word 128

LOCAL(div_r8_neg):
	clrt
	shll16 r4
	mov r4,r1
	shll8 r1
	mov r5,r4
	.rept 7
	rotcl r1; div1 r5,r0
	.endr
	mov.l @r15+,r5
	rotcl r1
	bra LOCAL(div_r8_neg_end)
	div1 r4,r0

LOCAL(m256_w):
	.word 0xff00
/* This table has been generated by divtab-sh4.c.  */
	.balign 4
LOCAL(div_table_clz):
	.byte	0
	.byte	1
	.byte	0
	.byte	-1
	.byte	-1
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-2
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-3
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-4
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-5
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
	.byte	-6
/* Lookup table translating positive divisor to index into table of
   normalized inverse.  N.B. the '0' entry is also the last entry of the
 previous table, and causes an unaligned access for division by zero.  */
LOCAL(div_table_ix):
	.byte	-6
	.byte	-128
	.byte	-128
	.byte	0
	.byte	-128
	.byte	-64
	.byte	0
	.byte	64
	.byte	-128
	.byte	-96
	.byte	-64
	.byte	-32
	.byte	0
	.byte	32
	.byte	64
	.byte	96
	.byte	-128
	.byte	-112
	.byte	-96
	.byte	-80
	.byte	-64
	.byte	-48
	.byte	-32
	.byte	-16
	.byte	0
	.byte	16
	.byte	32
	.byte	48
	.byte	64
	.byte	80
	.byte	96
	.byte	112
	.byte	-128
	.byte	-120
	.byte	-112
	.byte	-104
	.byte	-96
	.byte	-88
	.byte	-80
	.byte	-72
	.byte	-64
	.byte	-56
	.byte	-48
	.byte	-40
	.byte	-32
	.byte	-24
	.byte	-16
	.byte	-8
	.byte	0
	.byte	8
	.byte	16
	.byte	24
	.byte	32
	.byte	40
	.byte	48
	.byte	56
	.byte	64
	.byte	72
	.byte	80
	.byte	88
	.byte	96
	.byte	104
	.byte	112
	.byte	120
	.byte	-128
	.byte	-124
	.byte	-120
	.byte	-116
	.byte	-112
	.byte	-108
	.byte	-104
	.byte	-100
	.byte	-96
	.byte	-92
	.byte	-88
	.byte	-84
	.byte	-80
	.byte	-76
	.byte	-72
	.byte	-68
	.byte	-64
	.byte	-60
	.byte	-56
	.byte	-52
	.byte	-48
	.byte	-44
	.byte	-40
	.byte	-36
	.byte	-32
	.byte	-28
	.byte	-24
	.byte	-20
	.byte	-16
	.byte	-12
	.byte	-8
	.byte	-4
	.byte	0
	.byte	4
	.byte	8
	.byte	12
	.byte	16
	.byte	20
	.byte	24
	.byte	28
	.byte	32
	.byte	36
	.byte	40
	.byte	44
	.byte	48
	.byte	52
	.byte	56
	.byte	60
	.byte	64
	.byte	68
	.byte	72
	.byte	76
	.byte	80
	.byte	84
	.byte	88
	.byte	92
	.byte	96
	.byte	100
	.byte	104
	.byte	108
	.byte	112
	.byte	116
	.byte	120
	.byte	124
	.byte	-128
/* 1/64 .. 1/127, normalized.  There is an implicit leading 1 in bit 32.  */
	.balign 4
LOCAL(zero_l):
	.long	0x0
	.long	0xF81F81F9
	.long	0xF07C1F08
	.long	0xE9131AC0
	.long	0xE1E1E1E2
	.long	0xDAE6076C
	.long	0xD41D41D5
	.long	0xCD856891
	.long	0xC71C71C8
	.long	0xC0E07039
	.long	0xBACF914D
	.long	0xB4E81B4F
	.long	0xAF286BCB
	.long	0xA98EF607
	.long	0xA41A41A5
	.long	0x9EC8E952
	.long	0x9999999A
	.long	0x948B0FCE
	.long	0x8F9C18FA
	.long	0x8ACB90F7
	.long	0x86186187
	.long	0x81818182
	.long	0x7D05F418
	.long	0x78A4C818
	.long	0x745D1746
	.long	0x702E05C1
	.long	0x6C16C16D
	.long	0x68168169
	.long	0x642C8591
	.long	0x60581606
	.long	0x5C9882BA
	.long	0x58ED2309
LOCAL(div_table_inv):
	.long	0x55555556
	.long	0x51D07EAF
	.long	0x4E5E0A73
	.long	0x4AFD6A06
	.long	0x47AE147B
	.long	0x446F8657
	.long	0x41414142
	.long	0x3E22CBCF
	.long	0x3B13B13C
	.long	0x38138139
	.long	0x3521CFB3
	.long	0x323E34A3
	.long	0x2F684BDB
	.long	0x2C9FB4D9
	.long	0x29E4129F
	.long	0x27350B89
	.long	0x24924925
	.long	0x21FB7813
	.long	0x1F7047DD
	.long	0x1CF06ADB
	.long	0x1A7B9612
	.long	0x18118119
	.long	0x15B1E5F8
	.long	0x135C8114
	.long	0x11111112
	.long	0xECF56BF
	.long	0xC9714FC
	.long	0xA6810A7
	.long	0x8421085
	.long	0x624DD30
	.long	0x4104105
	.long	0x2040811
	/* maximum error: 0.987342 scaled: 0.921875*/

	ENDFUNC(GLOBAL(sdivsi3_i4i))
#endif /* SH3 / SH4 */

#endif /* L_div_table */

#ifdef L_udiv_qrnnd_16
	HIDDEN_FUNC(GLOBAL(udiv_qrnnd_16))
	/* r0: rn r1: qn */ /* r0: n1 r4: n0 r5: d r6: d1 */ /* r2: __m */
	/* n1 < d, but n1 might be larger than d1.  */
	.global GLOBAL(udiv_qrnnd_16)
	.balign 8
GLOBAL(udiv_qrnnd_16):
	div0u
	cmp/hi r6,r0
	bt .Lots
	.rept 16
	div1 r6,r0 
	.endr
	extu.w r0,r1
	bt 0f
	add r6,r0
0:	rotcl r1
	mulu.w r1,r5
	xtrct r4,r0
	swap.w r0,r0
	sts macl,r2
	cmp/hs r2,r0
	sub r2,r0
	bt 0f
	addc r5,r0
	add #-1,r1
	bt 0f
1:	add #-1,r1
	rts
	add r5,r0
	.balign 8
.Lots:
	sub r5,r0
	swap.w r4,r1
	xtrct r0,r1
	clrt
	mov r1,r0
	addc r5,r0
	mov #-1,r1
	SL1(bf, 1b,
	shlr16 r1)
0:	rts
	nop
	ENDFUNC(GLOBAL(udiv_qrnnd_16))
#endif /* L_udiv_qrnnd_16 */
