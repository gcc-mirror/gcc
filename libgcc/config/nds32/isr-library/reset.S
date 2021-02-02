/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

	.section .nds32_isr, "ax"	/* Put it in the section of 1st level handler.  */
	.align	1
	.weak	_SDA_BASE_	/* For reset handler only.  */
	.weak	_nds32_init_mem	/* User defined memory initialization function.  */
	.globl	_start
	.globl	_nds32_reset
	.type	_nds32_reset, @function
_nds32_reset:
_start:
	/* Handle NMI and warm boot if any of them exists.  */
	beqz	$sp, 1f		/* Reset, NMI or warm boot?  */
	/* Either NMI or warm boot; save all regs.  */

	/* Preserve registers for context-switching.  */
#if __NDS32_REDUCED_REGS__ || __NDS32_REDUCE_REGS
	/* For 16-reg mode.  */
	smw.adm $r0, [$sp], $r10, #0x0
	smw.adm $r15, [$sp], $r15, #0xf
#else
	/* For 32-reg mode.  */
	smw.adm $r0, [$sp], $r27, #0xf
#endif
#if __NDS32_EXT_IFC__
	mfusr   $r1, $IFC_LP
	smw.adm $r1, [$sp], $r2, #0x0	/* Save extra $r2 to keep stack 8-byte alignment.  */
#endif

	la	$gp, _SDA_BASE_	/* Init GP for small data access.  */
	move	$r0, $sp	/* Init parameter.  */
	mfsr	$r1, $ITYPE	/* Check ITYPE for NMI or warm boot.  */
	andi	$r1, $r1, #0xf
	addi	$r1, $r1, #-1
	beqz	$r1, 2f		/* Warm boot if true.  */
	l.w	$r15, _nds32_nmih	/* Load NMI handler.  */
	j	3f
2:
	l.w	$r15, _nds32_wrh	/* Load warm boot handler.  */
3:
	beqz    $r15, 1f	/* If no handler, do cold boot.  */
	jral    $r15		/* Call handler.  */
	bnez    $r0, 1f		/* If fail to resume, do cold boot.  */

	/* Restore registers for context-switching.  */
#if __NDS32_EXT_IFC__
	lmw.bim	$r1, [$sp], $r2, #0x0	/* Restore extra $r2 to keep stack 8-byte alignment.  */
	mtusr   $r1, $IFC_LP
#endif
#if __NDS32_REDUCED_REGS__ || __NDS32_REDUCE_REGS
	/* For 16-reg mode.  */
	lmw.bim	$r15, [$sp], $r15, #0xf
	lmw.bim	$r0, [$sp], $r10, #0x0
#else
	/* For 32-reg mode.  */
	lmw.bim $r0, [$sp], $r27, #0xf
#endif
	iret	/* Resume operation.  */


1:	/* Cold boot.  */
#if __NDS32_ISR_VECTOR_SIZE_4__
	/* With vector ID feature for v3 architecture, default vector size is 4-byte.  */
	/* Set IVB.ESZ = 0 (vector table entry size = 4 bytes)  */
	mfsr    $r0, $IVB
	li      $r1, #0xc000
	or      $r0, $r0, $r1
	xor     $r0, $r0, $r1
	mtsr    $r0, $IVB
	dsb
#else
	/* There is no vector ID feature, so the vector size must be 16-byte.  */
	/* Set IVB.ESZ = 1 (vector table entry size = 16 bytes)  */
	mfsr    $r0, $IVB
	li	$r1, #0xffff3fff
	and	$r0, $r0, $r1
	ori	$r0, $r0, #0x4000
	mtsr    $r0, $IVB
	dsb
#endif

	la	$gp, _SDA_BASE_		/* Init $gp.  */
	la	$sp, _stack		/* Init $sp.  */

#if __NDS32_EXT_EX9__
.L_init_itb:
	/* Initialization for Instruction Table Base (ITB).
	   The symbol _ITB_BASE_ is determined by Linker.
	   Set $ITB only if MSC_CFG.EIT (cr4.b'24) is set.  */
	mfsr    $r0, $MSC_CFG
	srli	$r0, $r0, 24
	andi	$r0, $r0, 0x1
	beqz	$r0, 4f		/* Fall through ?  */
	la      $r0, _ITB_BASE_
	mtusr   $r0, $ITB
4:
#endif

#if __NDS32_EXT_FPU_SP__ || __NDS32_EXT_FPU_DP__
.L_init_fpu:
	/* Initialize FPU
	   Set FUCOP_CTL.CP0EN (fucpr.b'0).  */
	mfsr    $r0, $FUCOP_CTL
	ori     $r0, $r0, 0x1
	mtsr    $r0, $FUCOP_CTL
	dsb
	/* According to [bugzilla #9425], set flush-to-zero mode.
	   That is, set $FPCSR.DNZ(b'12) = 1.  */
	FMFCSR	$r0
	ori	$r0, $r0, 0x1000
	FMTCSR	$r0
	dsb
#endif

	/* Call DRAM init. _nds32_init_mem may written by C language.  */
	la	$r15, _nds32_init_mem
	beqz	$r15, 6f
	jral	$r15
6:
	l.w	$r15, _nds32_jmptbl_00	/* Load reset handler.  */
	jral	$r15

	/* Reset handler() should never return in a RTOS or non-OS system.
	   In case it does return, an exception will be generated.
	   This exception will be caught either by default break handler or by EDM.
	   Default break handle may just do an infinite loop.
	   EDM will notify GDB and GDB will regain control when the ID is 0x7fff.  */
5:
	break    #0x7fff
	.size	_nds32_reset, .-_nds32_reset
