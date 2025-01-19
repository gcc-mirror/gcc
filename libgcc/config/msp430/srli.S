;   Copyright (C) 2012-2025 Free Software Foundation, Inc.
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
	
	.text

	.section	.text.__mspabi_srli_n
	.macro	_srli n
	.global __mspabi_srli_\n
__mspabi_srli_\n:
	CLRC
	RRC.W	R12
	.endm

/* Logical Right Shift - R12 -> R12.  */
	_srli	15
	_srli	14
	_srli	13
	_srli	12
	_srli	11
	_srli	10
	_srli	9
	_srli	8
	_srli	7
	_srli	6
	_srli	5
	_srli	4
	_srli	3
	_srli	2
	_srli	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

	.section	.text.__mspabi_srli
1:	ADD.W	#-1,R13
	CLRC
	RRC.W	R12,R12
	.global	__mspabi_srli
__mspabi_srli:
	CMP	#0,R13
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

#ifdef __MSP430X__
	.section	.text.__gnu_mspabi_srlp
1:	ADDA	#-1,R13
	CLRC
	RRCX.A	R12,R12
	.global	__gnu_mspabi_srlp
__gnu_mspabi_srlp:
	CMP	#0,R13
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif /* __MSP430X_LARGE__ */
#endif /* __MSP430X__ */

/* Logical Right Shift - R12:R13 -> R12:R13.  */

	.section	.text.__mspabi_srll_n
	.macro	_srll	n
	.global	__mspabi_srll_\n
__mspabi_srll_\n:
	CLRC
	RRC.W	R13
	RRC.W	R12
	.endm

	_srll	15
	_srll	14
	_srll	13
	_srll	12
	_srll	11
	_srll	10
	_srll	9
	_srll	8
	_srll	7
	_srll	6
	_srll	5
	_srll	4
	_srll	3
	_srll	2
	_srll	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

	.section	.text.__mspabi_srll
1:	ADD.W	#-1,R14
	CLRC
	RRC.W	R13
	RRC.W	R12
	.global	__mspabi_srll
__mspabi_srll:
	CMP	#0,R14
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

/* Logical Right Shift - R8:R11 -> R12:R15
   A 64-bit argument would normally be passed in R12:R15, but __mspabi_srlll has
   special conventions, so the 64-bit value to shift is passed in R8:R11.
   According to the MSPABI, the shift amount is a 64-bit value in R12:R15, but
   we only use the low word in R12.  */

	.section	.text.__mspabi_srlll
	.global __mspabi_srlll
__mspabi_srlll:
	MOV R11, R15 ; Free up R11 first
	MOV R12, R11 ; Save the shift amount in R11
	MOV R10, R14
	MOV R9, R13
	MOV R8, R12
	CMP #0,R11
	JNZ 1f
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif
1:
	CLRC
	RRC R15
	RRC R14
	RRC R13
	RRC R12
	ADD #-1,R11
	JNZ 1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif
