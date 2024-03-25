/* Copyright (C) 2000-2024 Free Software Foundation, Inc.
   This file was pretty much copied from newlib.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "crt.h"

#ifdef MMU_SUPPORT
	/* Section used for exception/timer interrupt stack area */
	.section .data.vbr.stack,"aw"
	.align 4
	.global __ST_VBR
__ST_VBR:
	.zero 1024 * 2          /* ; 2k for VBR handlers */
/* Label at the highest stack address where the stack grows from */
__timer_stack:
#endif /* MMU_SUPPORT */
	
	/* ;----------------------------------------
	Normal newlib crt1.S */

	! make a place to keep any previous value of the vbr register
	! this will only have a value if it has been set by redboot (for example)
	.section .bss
old_vbr:
	.long 0
#ifdef PROFILE
profiling_enabled:
	.long 0
#endif


	.section .text
	.global	start
	.import ___rtos_profiler_start_timer
	.weak   ___rtos_profiler_start_timer
start:
	mov.l	stack_k,r15

#if defined (__SH3__) || (defined (__SH_FPU_ANY__) && ! defined (__SH2E__) && ! defined (__SH2A__)) || defined (__SH4_NOFPU__)
#define VBR_SETUP
	! before zeroing the bss ...
	! if the vbr is already set to vbr_start then the program has been restarted
	! (i.e. it is not the first time the program has been run since reset)
	! reset the vbr to its old value before old_vbr (in bss) is wiped
	! this ensures that the later code does not create a circular vbr chain
	stc	vbr, r1
	mov.l	vbr_start_k, r2
	cmp/eq	r1, r2
	bf	0f
	! reset the old vbr value
	mov.l	old_vbr_k, r1
	mov.l	@r1, r2
	ldc	r2, vbr
0:	
#endif /* VBR_SETUP */
	
	! zero out bss
	mov.l	edata_k,r0
	mov.l	end_k,r1
	mov	#0,r2
start_l:
	mov.l	r2,@r0
	add	#4,r0
	cmp/ge	r0,r1
	bt	start_l

#if defined (__SH_FPU_ANY__)
	mov.l set_fpscr_k, r1
	mov #4,r4
	jsr @r1
	shll16 r4	! Set DN bit (flush denormal inputs to zero)
	lds r3,fpscr	! Switch to default precision
#endif /* defined (__SH_FPU_ANY__) */

#ifdef VBR_SETUP
	! save the existing contents of the vbr
	! there will only be a prior value when using something like redboot
	! otherwise it will be zero
	stc	vbr, r1
	mov.l	old_vbr_k, r2
	mov.l	r1, @r2
	! setup vbr
	mov.l	vbr_start_k, r1
	ldc	r1,vbr
#endif /* VBR_SETUP */

	! if an rtos is exporting a timer start fn,
	! then pick up an SR which does not enable ints
	! (the rtos will take care of this)
	mov.l rtos_start_fn, r0
	mov.l sr_initial_bare, r1
	tst	r0, r0
	bt	set_sr

	mov.l sr_initial_rtos, r1

set_sr:
	! Set status register (sr)
	ldc	r1, sr

	! arrange for exit to call fini
	mov.l	atexit_k,r0
	mov.l	fini_k,r4
	jsr	@r0
	nop

#ifdef PROFILE
	! arrange for exit to call _mcleanup (via stop_profiling)
	mova    stop_profiling,r0
	mov.l   atexit_k,r1
	jsr     @r1
	mov	r0, r4

	! Call profiler startup code
	mov.l monstartup_k, r0
	mov.l start_k, r4
	mov.l etext_k, r5
	jsr @r0
	nop

	! enable profiling trap
	! until now any trap 33s will have been ignored
	! This means that all library functions called before this point
	! (directly or indirectly) may have the profiling trap at the start.
	! Therefore, only mcount itself may not have the extra header.
	mov.l	profiling_enabled_k2, r0
	mov	#1, r1
	mov.l	r1, @r0
#endif /* PROFILE */

	! call init
	mov.l	init_k,r0
	jsr	@r0
	nop

	! call the mainline	
	mov.l	main_k,r0
	jsr	@r0
	nop

	! call exit
	mov	r0,r4
	mov.l	exit_k,r0
	jsr	@r0
	nop
	
		.balign 4
#ifdef PROFILE
stop_profiling:
	# stop mcount counting
	mov.l	profiling_enabled_k2, r0
	mov	#0, r1
	mov.l	r1, @r0

	# call mcleanup
	mov.l	mcleanup_k, r0
	jmp	@r0
	nop
		
		.balign 4
mcleanup_k:
	.long __mcleanup
monstartup_k:
	.long ___monstartup
profiling_enabled_k2:
	.long profiling_enabled
start_k:
	.long _start
etext_k:
	.long __etext
#endif /* PROFILE */

	.align 2
#if defined (__SH_FPU_ANY__)
set_fpscr_k:
	.long	___set_fpscr
#endif /*  defined (__SH_FPU_ANY__) */

stack_k:
	.long	_stack	
edata_k:
	.long	_edata
end_k:
	.long	_end
main_k:
	.long	___setup_argv_and_call_main
exit_k:
	.long	_exit
atexit_k:
	.long	_atexit
init_k:
	.long	GLOBAL(_init)
fini_k:
	.long	GLOBAL(_fini)
#ifdef VBR_SETUP
old_vbr_k:
	.long	old_vbr
vbr_start_k:
	.long	vbr_start
#endif /* VBR_SETUP */
	
sr_initial_rtos:
	! Privileged mode RB 1 BL 0. Keep BL 0 to allow default trap handlers to work.
	! Whether profiling or not, keep interrupts masked,
	! the RTOS will enable these if required.
	.long 0x600000f1 

rtos_start_fn:
	.long ___rtos_profiler_start_timer
	
#ifdef PROFILE
sr_initial_bare:
	! Privileged mode RB 1 BL 0. Keep BL 0 to allow default trap handlers to work.
	! For bare machine, we need to enable interrupts to get profiling working
	.long 0x60000001
#else

sr_initial_bare:
	! Privileged mode RB 1 BL 0. Keep BL 0 to allow default trap handlers to work.
	! Keep interrupts disabled - the application will enable as required.
	.long 0x600000f1
#endif

	! supplied for backward compatibility only, in case of linking
	! code whose main() was compiled with an older version of GCC.
	.global ___main
___main:
	rts
	nop
#ifdef VBR_SETUP
! Exception handlers	
	.section .text.vbr, "ax"
vbr_start:

	.org 0x100
vbr_100:
#ifdef PROFILE
	! Note on register usage.
	! we use r0..r3 as scratch in this code. If we are here due to a trapa for profiling
	! then this is OK as we are just before executing any function code.
	! The other r4..r7 we save explicityl on the stack
	! Remaining registers are saved by normal ABI conventions and we assert we do not
	! use floating point registers.
	mov.l expevt_k1, r1
	mov.l @r1, r1
	mov.l event_mask, r0
	and r0,r1
	mov.l trapcode_k, r2
	cmp/eq r1,r2
	bt 1f
	bra handler_100   ! if not a trapa, go to default handler
	nop
1:	
	mov.l trapa_k, r0
	mov.l @r0, r0
	shlr2 r0      ! trapa code is shifted by 2.
	cmp/eq #33, r0
	bt 2f
	bra handler_100
	nop
2:	
	
	! If here then it looks like we have trap #33
	! Now we need to call mcount with the following convention
	! Save and restore r4..r7
	mov.l	r4,@-r15
	mov.l	r5,@-r15
	mov.l	r6,@-r15
	mov.l	r7,@-r15
	sts.l	pr,@-r15

	! r4 is frompc.
	! r5 is selfpc
	! r0 is the branch back address.
	! The code sequence emitted by gcc for the profiling trap is
	! .align 2
	! trapa #33
	! .align 2
	! .long lab Where lab is planted by the compiler. This is the address
	! of a datum that needs to be incremented. 
	sts pr,  r4     ! frompc
	stc spc, r5	! selfpc
	mov #2, r2
	not r2, r2      ! pattern to align to 4
	and r2, r5      ! r5 now has aligned address
!	add #4, r5      ! r5 now has address of address
	mov r5, r2      ! Remember it.
!	mov.l @r5, r5   ! r5 has value of lable (lab in above example)
	add #8, r2
	ldc r2, spc     ! our return address avoiding address word

	! only call mcount if profiling is enabled
	mov.l profiling_enabled_k, r0
	mov.l @r0, r0
	cmp/eq #0, r0
	bt 3f
	! call mcount
	mov.l mcount_k, r2
	jsr @r2
	nop
3:
	lds.l @r15+,pr
	mov.l @r15+,r7
	mov.l @r15+,r6
	mov.l @r15+,r5
	mov.l @r15+,r4
	rte
	nop
	.balign 4
event_mask:
	.long 0xfff
trapcode_k:	
	.long 0x160
expevt_k1:
	.long 0xff000024 ! Address of expevt
trapa_k:	
	.long 0xff000020
mcount_k:
	.long __call_mcount
profiling_enabled_k:
	.long profiling_enabled
#endif
	! Non profiling case.
handler_100:
	mov.l 2f, r0     ! load the old vbr setting (if any)
	mov.l @r0, r0
	cmp/eq #0, r0
	bf 1f
	! no previous vbr - jump to own generic handler
	bra handler
	nop	
1:	! there was a previous handler - chain them
	add #0x7f, r0	 ! 0x7f
	add #0x7f, r0	 ! 0xfe
	add #0x2, r0     ! add 0x100 without corrupting another register
	jmp @r0
	nop
	.balign 4
2:	
	.long old_vbr

	.org 0x400
vbr_400:	! Should be at vbr+0x400
	mov.l 2f, r0     ! load the old vbr setting (if any)
	mov.l @r0, r0
	cmp/eq #0, r0
	! no previous vbr - jump to own generic handler
	bt handler
	! there was a previous handler - chain them
	rotcr r0
	rotcr r0
	add #0x7f, r0	 ! 0x1fc
	add #0x7f, r0	 ! 0x3f8
	add #0x02, r0	 ! 0x400
	rotcl r0
	rotcl r0	 ! Add 0x400 without corrupting another register
	jmp @r0
	nop
	.balign 4
2:
	.long old_vbr
handler:
	/* If the trap handler is there call it */
	mov.l	superh_trap_handler_k, r0
	cmp/eq	#0, r0       ! True if zero.
	bf 3f
	bra   chandler
	nop
3:	
	! Here handler available, call it. 
	/* Now call the trap handler with as much of the context unchanged as possible.
	   Move trapping address into PR to make it look like the trap point */
	stc spc, r1
	lds r1, pr
	mov.l expevt_k, r4
	mov.l @r4, r4 ! r4 is value of expevt, first parameter.
	mov r1, r5   ! Remember trapping pc.
	mov r1, r6   ! Remember trapping pc.
	mov.l chandler_k, r1
	mov.l superh_trap_handler_k, r2
	! jmp to trap handler to avoid disturbing pr. 
	jmp @r2
	nop

	.org 0x600
vbr_600:
#ifdef PROFILE	
	! Should be at vbr+0x600
	! Now we are in the land of interrupts so need to save more state. 
	! Save register state
	mov.l interrupt_stack_k, r15 ! r15 has been saved to sgr.
	mov.l	r0,@-r15	
	mov.l	r1,@-r15
	mov.l	r2,@-r15
	mov.l	r3,@-r15
	mov.l	r4,@-r15
	mov.l	r5,@-r15
	mov.l	r6,@-r15
	mov.l	r7,@-r15
	sts.l	pr,@-r15
	sts.l	mach,@-r15
	sts.l	macl,@-r15
#if defined(__SH_FPU_ANY__)
	! Save fpul and fpscr, save fr0-fr7 in 64 bit mode
	! and set the pervading precision for the timer_handler
	mov	#0,r0
	sts.l	fpul,@-r15
	sts.l	fpscr,@-r15
	lds	r0,fpscr	! Clear fpscr
	fmov	fr0,@-r15
	fmov	fr1,@-r15
	fmov	fr2,@-r15
	fmov	fr3,@-r15
	mov.l	pervading_precision_k,r0
	fmov	fr4,@-r15
	fmov	fr5,@-r15
	mov.l	@r0,r0
	fmov	fr6,@-r15
	fmov	fr7,@-r15
	lds	r0,fpscr
#endif /* __SH_FPU_ANY__ */
	! Pass interrupted pc to timer_handler as first parameter (r4).
	stc    spc, r4
	mov.l timer_handler_k, r0
	jsr @r0
	nop
#if defined(__SH_FPU_ANY__)
	mov	#0,r0
	lds	r0,fpscr	! Clear the fpscr
	fmov	@r15+,fr7
	fmov	@r15+,fr6
	fmov	@r15+,fr5
	fmov	@r15+,fr4
	fmov	@r15+,fr3
	fmov	@r15+,fr2
	fmov	@r15+,fr1
	fmov	@r15+,fr0
	lds.l	@r15+,fpscr
	lds.l	@r15+,fpul
#endif /* __SH_FPU_ANY__ */
	lds.l @r15+,macl
	lds.l @r15+,mach
	lds.l @r15+,pr
	mov.l @r15+,r7
	mov.l @r15+,r6
	mov.l @r15+,r5
	mov.l @r15+,r4
	mov.l @r15+,r3
	mov.l @r15+,r2
	mov.l @r15+,r1
	mov.l @r15+,r0
	stc sgr, r15    ! Restore r15, destroyed by this sequence. 
	rte
	nop
#if defined(__SH_FPU_ANY__)
	.balign 4
pervading_precision_k:
	.long GLOBAL(__fpscr_values)+4
#endif
#else
	mov.l 2f, r0     ! Load the old vbr setting (if any).
	mov.l @r0, r0
	cmp/eq #0, r0
	! no previous vbr - jump to own handler
	bt chandler
	! there was a previous handler - chain them
	rotcr r0
	rotcr r0
	add #0x7f, r0	 ! 0x1fc
	add #0x7f, r0	 ! 0x3f8
	add #0x7f, r0	 ! 0x5f4
	add #0x03, r0	 ! 0x600
	rotcl r0
	rotcl r0	 ! Add 0x600 without corrupting another register
	jmp @r0
	nop
	.balign 4
2:
	.long old_vbr
#endif	 /* PROFILE code */
chandler:
	mov.l expevt_k, r4
	mov.l @r4, r4 ! r4 is value of expevt hence making this the return code
	mov.l handler_exit_k,r0
	jsr   @r0
	nop
	! We should never return from _exit but in case we do we would enter the
	! the following tight loop
limbo:
	bra limbo
	nop
	.balign 4
#ifdef PROFILE
interrupt_stack_k:
	.long __timer_stack	! The high end of the stack
timer_handler_k:
	.long __profil_counter
#endif
expevt_k:
	.long 0xff000024 ! Address of expevt
chandler_k:	
	.long chandler	
superh_trap_handler_k:
	.long	__superh_trap_handler
handler_exit_k:
	.long _exit
	.align 2
! Simulated compile of trap handler.
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.text
.Ltext0:
	.align 5
	.type	__superh_trap_handler,@function
__superh_trap_handler:
.LFB1:
	mov.l	r14,@-r15
.LCFI0:
	add	#-4,r15
.LCFI1:
	mov	r15,r14
.LCFI2:
	mov.l	r4,@r14
	lds	r1, pr
	add	#4,r14
	mov	r14,r15
	mov.l	@r15+,r14
	rts	
	nop
.LFE1:
.Lfe1:
	.size	__superh_trap_handler,.Lfe1-__superh_trap_handler
	.section	.debug_frame,"",@progbits
.Lframe0:
	.ualong	.LECIE0-.LSCIE0
.LSCIE0:
	.ualong	0xffffffff
	.byte	0x1
	.string	""
	.uleb128 0x1
	.sleb128 -4
	.byte	0x11
	.byte	0xc
	.uleb128 0xf
	.uleb128 0x0
	.align 2
.LECIE0:
.LSFDE0:
	.ualong	.LEFDE0-.LASFDE0
.LASFDE0:
	.ualong	.Lframe0
	.ualong	.LFB1
	.ualong	.LFE1-.LFB1
	.byte	0x4
	.ualong	.LCFI0-.LFB1
	.byte	0xe
	.uleb128 0x4
	.byte	0x4
	.ualong	.LCFI1-.LCFI0
	.byte	0xe
	.uleb128 0x8
	.byte	0x8e
	.uleb128 0x1
	.byte	0x4
	.ualong	.LCFI2-.LCFI1
	.byte	0xd
	.uleb128 0xe
	.align 2
.LEFDE0:
	.text
.Letext0:
	.section	.debug_info
	.ualong	0xb3
	.uaword	0x2
	.ualong	.Ldebug_abbrev0
	.byte	0x4
	.uleb128 0x1
	.ualong	.Ldebug_line0
	.ualong	.Letext0
	.ualong	.Ltext0
	.string	"trap_handler.c"
	.string	"xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
	.string	"GNU C 3.2 20020529 (experimental)"
	.byte	0x1
	.uleb128 0x2
	.ualong	0xa6
	.byte	0x1
	.string	"_superh_trap_handler"
	.byte	0x1
	.byte	0x2
	.byte	0x1
	.ualong	.LFB1
	.ualong	.LFE1
	.byte	0x1
	.byte	0x5e
	.uleb128 0x3
	.string	"trap_reason"
	.byte	0x1
	.byte	0x1
	.ualong	0xa6
	.byte	0x2
	.byte	0x91
	.sleb128 0
	.byte	0x0
	.uleb128 0x4
	.string	"unsigned int"
	.byte	0x4
	.byte	0x7
	.byte	0x0
	.section	.debug_abbrev
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x10
	.uleb128 0x6
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x1b
	.uleb128 0x8
	.uleb128 0x25
	.uleb128 0x8
	.uleb128 0x13
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x1
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x5
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x4
	.uleb128 0x24
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.byte	0x0
	.byte	0x0
	.byte	0x0
	.section	.debug_pubnames,"",@progbits
	.ualong	0x27
	.uaword	0x2
	.ualong	.Ldebug_info0
	.ualong	0xb7
	.ualong	0x67
	.string	"_superh_trap_handler"
	.ualong	0x0
	.section	.debug_aranges,"",@progbits
	.ualong	0x1c
	.uaword	0x2
	.ualong	.Ldebug_info0
	.byte	0x4
	.byte	0x0
	.uaword	0x0
	.uaword	0x0
	.ualong	.Ltext0
	.ualong	.Letext0-.Ltext0
	.ualong	0x0
	.ualong	0x0
#endif /* VBR_SETUP */
