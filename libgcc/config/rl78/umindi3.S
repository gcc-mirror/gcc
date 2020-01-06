;   Copyright (C) 2017-2020 Free Software Foundation, Inc.
;   Contributed by Sebastian Perta.
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

    .text

START_FUNC ___umindi3

    ; copy first argument/operand to the output registers
    movw   ax, [sp+4]
    movw   r8, ax
    movw   ax, [sp+6]
    movw   r10, ax
    movw   ax, [sp+8]
    movw   r12, ax
    movw   ax, [sp+10]
    movw   r14, ax

    ; use 16-bit compares from the most significant words downto the least significant ones
    movw   ax, [sp+18]
    cmpw   ax, r14
    bc     $.L1
    bnz    $.L2

    movw   ax, [sp+16]
    cmpw   ax, r12
    bc     $.L1
    bnz    $.L2

    movw   ax, [sp+14]
    cmpw   ax, r10
    bc     $.L1
    bnz    $.L2

    movw   ax, [sp+12]
    cmpw   ax, r8
    bc     $.L1
    ret

.L1:
    ; copy second argument/operand to the output registers
    movw   ax, [sp+12]
    movw   r8, ax
    movw   ax, [sp+14]
    movw   r10, ax
    movw   ax, [sp+16]
    movw   r12, ax
    movw   ax, [sp+18]
    movw   r14, ax
.L2:
    ret

END_FUNC ___umindi3
