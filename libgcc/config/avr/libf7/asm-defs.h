/* Copyright (C) 2019-2024 Free Software Foundation, Inc.

   This file is part of LIBF7, which is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef ASM_DEFS_H
#define ASM_DEFS_H

#ifdef __AVR__
#ifdef __ASSEMBLER__
/*****************************************************************/
/* Stuff for Assembler-only                                      */
/*****************************************************************/

#if defined (__AVR_TINY__)
    #define __tmp_reg__  16
    #define __zero_reg__ 17
#else
    #define __tmp_reg__  0
    #define __zero_reg__ 1
#endif /* AVR_TINY */

#define __SREG__ 0x3f
#define __SP_L__ 0x3d
#if defined (__AVR_HAVE_SPH__)
#define __SP_H__ 0x3e
#endif

#if !defined ASM_DEFS_HAVE_DEFUN
.macro DEFUN name
    .global \name
    .func \name
    \name:
.endm

.macro ENDF name
    .size \name, .-\name
    .endfunc
.endm

.macro LABEL name
    .global \name
    \name:
.endm
#endif /* HAVE_DEFUN */


#if defined (__AVR_HAVE_JMP_CALL__)
    #define XCALL call
    #define XJMP  jmp
#else
    #define XCALL rcall
    #define XJMP  rjmp
#endif

#if defined (__AVR_HAVE_EIJMP_EICALL__)
    #define XICALL  eicall
    #define XIJMP   eijmp
    #define PC_SIZE 3
#else
    #define XICALL  icall
    #define XIJMP   ijmp
    #define PC_SIZE 2
#endif

.macro skipnext
    cpse r16, r16
.endm

/*
    Factor out support of MOVW.  Usage is like

        wmov  30, 24

    to move R25:R24 to R31:R30, i.e. plain register numbers
    are required and no register prefix 'R'.
*/

#if defined (__AVR_HAVE_MOVW__)
#define wmov    movw
#else
    .macro  wmov dst src
    ..dst = \dst
    ..src = \src
    ..regno = 0
    .irp    reg,                                                \
            r0,  r1,  r2,  r3,  r4,  r5,  r6,   r7,  r8,  r9,   \
            r10, r11, r12, r13, r14, r15, r16, r17, r18, r19,   \
            r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,   \
            r30, r31
        .ifc  \reg,\dst
            ..dst = ..regno
        .endif
        .ifc  \reg,\src
            ..src = ..regno
        .endif
        ..regno = ..regno + 1
    .endr

    ..regno = 0

    .irp    reg,                                                \
            R0,  R1,  R2,  R3,  R4,  R5,  R6,   R7,  R8,  R9,   \
            R10, R11, R12, R13, R14, R15, R16, R17, R18, R19,   \
            R20, R21, R22, R23, R24, R25, R26, R27, R28, R29,   \
            R30, R31
        .ifc  \reg,\dst
            ..dst = ..regno
        .endif
        .ifc  \reg,\src
            ..src = ..regno
        .endif
        ..regno = ..regno + 1
    .endr

    ..regno = 0

    .irp    reg,                        \
            X, x, XL, xl, Xl, xL, x, x, \
            Y, y, YL, yl, Yl, yL, y, y, \
            Z, z, ZL, zl, Zl, zL, z, z
        .ifc  \reg,\dst
            ..dst = 2 * (..regno / 8) + 26
        .endif
        .ifc  \reg,\src
            ..src = 2 * (..regno / 8) + 26
        .endif
        ..regno = ..regno + 1
    .endr

    mov     ..dst+0, ..src+0
    mov     ..dst+1, ..src+1
    .endm
#endif /* MOVW */


#if !defined (__AVR_TINY__)
/*
    Convenience macro for easy use of __prologue_saves__ from libgcc.
    Push the N_PUSHED callee-saved registers  Y, R17, R16, R15, ...
    with 0 <= N_PUSHED <= 18.  The frame pointer (Y) is set up according
    to a frame size of N_FRAME.  Clobbers TMP_REG.
    For the code of __prologue_saves__ from libgcc see
    http://gcc.gnu.org/viewcvs/gcc/trunk/libgcc/config/avr/lib1funcs.S?revision=267494&view=markup#l2159
*/

.macro do_prologue_saves n_pushed n_frame=0
    ldi r26, lo8(\n_frame)
    ldi r27, hi8(\n_frame)
    ldi r30, lo8(gs(.L_prologue_saves.\@))
    ldi r31, hi8(gs(.L_prologue_saves.\@))
    XJMP __prologue_saves__ + ((18 - (\n_pushed)) * 2)
.L_prologue_saves.\@:
.endm

/*
    Convenience macro for easy use of __epilogue_restores__ from libgcc.
    Undo the effect of __prologue_saves__.  Clobbers TMP_REG.
    For the code of __epilogue_restores__ from libgcc see
    http://gcc.gnu.org/viewcvs/gcc/trunk/libgcc/config/avr/lib1funcs.S?revision=267494&view=markup#l2216
*/

.macro do_epilogue_restores n_pushed n_frame=0
    in      r28, __SP_L__
#ifdef __AVR_HAVE_SPH__
    in      r29, __SP_H__
.if \n_frame > 63
    subi    r28, lo8(-\n_frame)
    sbci    r29, hi8(-\n_frame)
.elseif \n_frame > 0
    adiw    r28, \n_frame
.endif
#else
    clr     r29
.if \n_frame > 0
    subi    r28, lo8(-\n_frame)
.endif
#endif /* HAVE SPH */
    ldi     r30, \n_pushed
    XJMP __epilogue_restores__ + ((18 - (\n_pushed)) * 2)
.endm

#endif /* AVR_TINY */

#else /* Assembler */
/*****************************************************************/
/* Space for C/C++ only Stuff                                    */
/*****************************************************************/
#endif /* Assembler */

/*****************************************************************/
/* Space for Generic Stuff (Assembler, C, C++)                   */
/*****************************************************************/

#ifdef __AVR_PM_BASE_ADDRESS__
    /*
        Devices with a linear address space:  Flash memory is seen in the
        RAM address space at an offset of __AVR_PM_BASE_ADDRESS__ and can
        be accessed by LD*.  This is the case for devices like ATtiny40
        (avrtiny) or ATtiny1616 and ATmega4808 (avrxmega3).  The default
        linker script locates .rodata in the .text output section and
        at the required offset.
    */
    #define RODATA_SECTION  .rodata.asm
    #define USE_LD  1
    #define USE_LPM 0
#else /* PM_BASE_ADDRESS */
    /*
        No linear address space.  As .rodata is located in RAM, we have to
        use .progmem.data (located in flash) and LPM to read the data.
        This will also work for devices from avrxmega3.
    */
    #define RODATA_SECTION  .progmem.data.asm
    #define USE_LD  0
    #define USE_LPM 1
#endif /* PM_BASE_ADDRESS */

#endif /* target AVR */
#endif /* ASM_DEFS_H */
