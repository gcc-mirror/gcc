/* The startup code sample of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

!!==============================================================================
!!
!!      crtzero.S
!!
!!      This is JUST A SAMPLE of nds32 startup code !!
!!      You can refer this content and implement
!!      the actual one in newlib/mculib.
!!
!!==============================================================================

!!------------------------------------------------------------------------------
!! Jump to start up code
!!------------------------------------------------------------------------------
	.section	.nds32_init, "ax"
	j	_start

!!------------------------------------------------------------------------------
!! Startup code implementation
!!------------------------------------------------------------------------------
	.section	.text
	.global	_start
	.weak	_SDA_BASE_
	.weak	_FP_BASE_
	.align	2
	.func	_start
	.type	_start, @function
_start:
.L_fp_gp_lp_init:
	la	$fp, _FP_BASE_		! init $fp
	la	$gp, _SDA_BASE_		! init $gp for small data access
	movi	$lp, 0			! init $lp

.L_stack_init:
	la	$sp, _stack		! init $sp
	movi	$r0, -8			! align $sp to 8-byte (use 0xfffffff8)
	and	$sp, $sp, $r0		! align $sp to 8-byte (filter out lower 3-bit)

.L_bss_init:
	! clear BSS, this process can be 4 time faster if data is 4 byte aligned
	! if so, use swi.p instead of sbi.p
	! the related stuff are defined in linker script
	la	$r0, _edata		! get the starting addr of bss
	la	$r2, _end		! get ending addr of bss
	beq	$r0, $r2, .L_call_main	! if no bss just do nothing
	movi	$r1, 0			! should be cleared to 0
.L_clear_bss:
	sbi.p	$r1, [$r0], 1		! Set 0 to bss
	bne	$r0, $r2, .L_clear_bss	! Still bytes left to set

!.L_stack_heap_check:
!	la	$r0, _end		! init heap_end
!	s.w	$r0, heap_end		! save it


!.L_init_argc_argv:
!	! argc/argv initialization if necessary; default implementation is in crt1.o
!	la	$r9, _arg_init		! load address of _arg_init?
!	beqz	$r9, .L4		! has _arg_init? no, go check main()
!	addi	$sp, $sp, -512		! allocate space for command line + arguments
!	move	$r6, $sp		! r6 = buffer addr of cmd line
!	move	$r0, $r6		! r0 = buffer addr of cmd line
!	syscall	6002			! get cmd line
!	move	$r0, $r6		! r0 = buffer addr of cmd line
!	addi	$r1, $r6, 256		! r1 = argv
!	jral	$r9			! init argc/argv
!	addi	$r1, $r6, 256		! r1 = argv

.L_call_main:
	! call main() if main() is provided
	la	$r15, main		! load address of main
	jral	$r15			! call main

.L_terminate_program:
	syscall	0x1			! use syscall 0x1 to terminate program
	.size	_start, .-_start
	.end

!! ------------------------------------------------------------------------
