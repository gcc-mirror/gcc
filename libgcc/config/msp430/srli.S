;   Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

	.macro	_srli n
	.global __mspabi_srli_\n
__mspabi_srli_\n:
	CLRC
	RRC.W	R12
	.endm

/* Logical Right Shift - R12 -> R12 */
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

/* Logical Right Shift - R12:R13 -> R12:R13 */

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
