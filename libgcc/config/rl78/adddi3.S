;   Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

START_FUNC ___adddi3

    movw  hl, sp       ; use HL-based addressing (allows for direct addw)

    movw  ax, [hl+4]
    addw  ax, [hl+12]
    movw  r8, ax

    mov   a, [hl+6]    ; middle bytes of the result are determined using 8-bit
    addc  a, [hl+14]   ; ADDC insns which both account for and update the carry bit
    mov   r10, a       ; (no ADDWC instruction is available)
    mov   a, [hl+7]
    addc  a, [hl+15]
    mov   r11, a

    mov   a, [hl+8]
    addc  a, [hl+16]
    mov   r12, a
    mov   a, [hl+9]
    addc  a, [hl+17]
    mov   r13, a

    movw  ax, [hl+10]
    sknc               ; account for the possible carry from the
    incw  ax           ; latest 8-bit operation
    addw  ax, [hl+18]
    movw  r14, ax

    ret

END_FUNC ___adddi3
