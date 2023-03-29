;   Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

/* Logical Left Shift - R12 -> R12.  */

	.section	.text.__mspabi_slli_n
	.macro	_slli n
	.global __mspabi_slli_\n
__mspabi_slli_\n:
	ADD.W	R12,R12
	.endm

	_slli	15
	_slli	14
	_slli	13
	_slli	12
	_slli	11
	_slli	10
	_slli	9
	_slli	8
	_slli	7
	_slli	6
	_slli	5
	_slli	4
	_slli	3
	_slli	2
	_slli	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

	.section	.text.__mspabi_slli
1:	ADD.W	#-1,R13
	ADD.W	R12,R12
	.global	__mspabi_slli
__mspabi_slli:
	CMP	#0,R13
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

#ifdef __MSP430X__
	.section	.text.__gnu_mspabi_sllp
1:	ADDA	#-1,R13
	ADDA	R12,R12
	.global	__gnu_mspabi_sllp
__gnu_mspabi_sllp:
	CMP	#0,R13
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif /* __MSP430X_LARGE__ */
#endif /* __MSP430X__ */

/* Logical Left Shift - R12:R13 -> R12:R13.  */

	.section	.text.__mspabi_slll_n
	.macro	_slll	n
	.global	__mspabi_slll_\n
__mspabi_slll_\n:
	ADD.W	R12,R12
	ADDC.W	R13,R13
	.endm

	_slll	15
	_slll	14
	_slll	13
	_slll	12
	_slll	11
	_slll	10
	_slll	9
	_slll	8
	_slll	7
	_slll	6
	_slll	5
	_slll	4
	_slll	3
	_slll	2
	_slll	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

	.section	.text.__mspabi_slll
1:	ADD.W	#-1,R14
	ADD.W	R12,R12
	ADDC.W	R13,R13
	.global	__mspabi_slll
__mspabi_slll:
	CMP	#0,R14
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

/* Logical Left Shift - R8:R11 -> R12:R15
   A 64-bit argument would normally be passed in R12:R15, but __mspabi_sllll has
   special conventions, so the 64-bit value to shift is passed in R8:R11.
   According to the MSPABI, the shift amount is a 64-bit value in R12:R15, but
   we only use the low word in R12.  */

	.section	.text.__mspabi_sllll
	.global __mspabi_sllll
__mspabi_sllll:
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
	RLA R12
	RLC R13
	RLC R14
	RLC R15
	ADD #-1,R11
	JNZ 1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif
