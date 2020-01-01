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

START_FUNC ___anddi3

    movw  hl, sp

    mov   a, [hl+4]
    and   a, [hl+12]
    mov   r8, a

    mov   a, [hl+5]
    and   a, [hl+13]
    mov   r9, a

    mov   a, [hl+6]
    and   a, [hl+14]
    mov   r10, a

    mov   a, [hl+7]
    and   a, [hl+15]
    mov   r11, a

    mov   a, [hl+8]
    and   a, [hl+16]
    mov   r12, a

    mov   a, [hl+9]
    and   a, [hl+17]
    mov   r13, a

    mov   a, [hl+10]
    and   a, [hl+18]
    mov   r14, a

    mov   a, [hl+11]
    and   a, [hl+19]
    mov   r15, a

    ret

END_FUNC ___anddi3
