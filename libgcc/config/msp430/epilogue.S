;   Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

	.global	__mspabi_func_epilog_7
	.global	__mspabi_func_epilog_6
	.global	__mspabi_func_epilog_5
	.global	__mspabi_func_epilog_4
	.global	__mspabi_func_epilog_3
	.global	__mspabi_func_epilog_2
	.global	__mspabi_func_epilog_1

__mspabi_func_epilog_7:
	POP	R4
__mspabi_func_epilog_6:
	POP	R5
__mspabi_func_epilog_5:
	POP	R6
__mspabi_func_epilog_4:
	POP	R7
__mspabi_func_epilog_3:
	POP	R8
__mspabi_func_epilog_2:
	POP	R9
__mspabi_func_epilog_1:
	POP	R10
#ifdef __MSP430X_LARGE__
	RETA
#else
	RET
#endif
