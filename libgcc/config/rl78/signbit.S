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

#include "vregs.h"
	
;; int signbitf (float X)
;; int signbit  (double X)
;; int signbitl (long double X)
;;
;;     `signbit' returns a nonzero value if the value of X has its sign
;;     bit set.
;;
;;     This is not the same as `x < 0.0', because IEEE 754 floating point
;;     allows zero to be signed.  The comparison `-0.0 < 0.0' is false,
;;     but `signbit (-0.0)' will return a nonzero value.

;----------------------------------------------------------------------

	.text

START_FUNC		_signbit
START_ANOTHER_FUNC	_signbitf
	;; X is at [sp+4]..[SP+7]
	;; result is in R8..R9

	movw	r8, #0
	mov	a, [sp+7]
	mov1	cy, a.7
	sknc
	movw	r8, #1
	ret
END_ANOTHER_FUNC	_signbitf
END_FUNC		_signbit


START_FUNC	_signbitl
	;; X is at [sp+4]..[SP+7]
	;; result is in R8..R9

	movw	r8, #0
	mov	a, [sp+11]
	mov1	cy, a.7
	sknc
	movw	r8, #1
	ret
END_FUNC 	_signbitl
