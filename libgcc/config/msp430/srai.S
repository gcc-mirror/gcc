;   Copyright (C) 2012, 2013 Free Software Foundation, Inc.
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

	.macro	_srai n
	.global __mspabi_srai_\n
__mspabi_srai_\n:
	RRA.W	R12
	.endm

/* Logical Right Shift - R12 -> R12 */
	_srai	15
	_srai	14
	_srai	13
	_srai	12
	_srai	11
	_srai	10
	_srai	9
	_srai	8
	_srai	7
	_srai	6
	_srai	5
	_srai	4
	_srai	3
	_srai	2
	_srai	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

1:	ADD.W	#-1,R13
	RRA.W	R12,R12
	.global	__mspabi_srai
__mspabi_srai:
	CMP	#0,R13
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

/* Logical Right Shift - R12:R13 -> R12:R13 */

	.macro	_sral	n
	.global	__mspabi_sral_\n
__mspabi_sral_\n:
	RRA.W	R13
	RRC.W	R12
	.endm

	_sral	15
	_sral	14
	_sral	13
	_sral	12
	_sral	11
	_sral	10
	_sral	9
	_sral	8
	_sral	7
	_sral	6
	_sral	5
	_sral	4
	_sral	3
	_sral	2
	_sral	1
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif

1:	ADD.W	#-1,R14
	RRA.W	R13
	RRC.W	R12
	.global	__mspabi_sral
__mspabi_sral:
	CMP	#0,R14
	JNZ	1b
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif
