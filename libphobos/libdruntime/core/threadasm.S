/**
 * Support code for mutithreading.
 *
 * Copyright: Copyright Mikola Lysenko 2005 - 2012.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Mikola Lysenko, Martin Nowak, Kai Nacke
 */

/*
 *          Copyright Mikola Lysenko 2005 - 2012.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */

/* NOTE: This file has been patched from the original DMD distribution to
 * work with the GDC compiler.
 */
#if (__linux__ || __FreeBSD__ || __NetBSD__ || __DragonFly__) && __ELF__
/*
 * Mark the resulting object file as not requiring execution permissions on
 * stack memory. The absence of this section would mark the whole resulting
 * library as requiring an executable stack, making it impossible to
 * dynamically load druntime on several Linux platforms where this is
 * forbidden due to security policies.
 * Use %progbits instead of @progbits to support ARM and X86.
 */
.section .note.GNU-stack,"",%progbits
#endif

/* Let preprocessor tell us if C symbols have a prefix: __USER_LABEL_PREFIX__ */
#ifdef __USER_LABEL_PREFIX__
#define GLUE2(a, b) a ## b
#define GLUE(a, b) GLUE2(a, b)
#define CSYM(name) GLUE(__USER_LABEL_PREFIX__, name)
#else
#define CSYM(name) name
#endif

/************************************************************************************
 * POWER PC ASM BITS
 ************************************************************************************/
#if defined( __PPC64__ )

#if defined(_CALL_ELF) && _CALL_ELF == 2
#define USE_ABI_2
#define LINKAGE_SZ   32
#define LR_OFS       16
#define TOC_OFS      24
#define GPR_OFS      32
#define STACK_SZ     (LINKAGE_SZ + 26*8)
#define OFS_R3_R10   GPR_OFS
#define OFS_R14_R31  (GPR_OFS+8*8)
#else
#define LINKAGE_SZ   48
#define LR_OFS       16
#define TOC_OFS      40
#define GPR_OFS      112
#define STACK_SZ     (LINKAGE_SZ + 8*8 + 18*8)
#define OFS_R3_R10   (STACK_SZ+LINKAGE_SZ)
#define OFS_R14_R31  GPR_OFS
#endif

    .text
#if defined( USE_ABI_2 )
    .abiversion 2
#endif
    .globl  _D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .align  2
    .type   _D4core6thread18callWithStackShellFNbMDFNbPvZvZv,@function
#if defined( USE_ABI_2 )
    .section .text._D4core6thread18callWithStackShellFNbMDFNbPvZvZv,"a",@progbits
#else
    .section .opd,"aw",@progbits
#endif
_D4core6thread18callWithStackShellFNbMDFNbPvZvZv:
#if !defined( USE_ABI_2 )
    .align  3
    .quad .L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .quad .TOC.@tocbase
    .quad 0
#endif
    .text
/*
 * Called with:
 * r3: pointer context
 * r4: pointer to function
 */
.L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv:
    .cfi_startproc
    stdu    1, -STACK_SZ(1)
    mflr    0
    std     0, LR_OFS(1)
    .cfi_def_cfa_offset 256
    .cfi_offset lr, 16

    /* Save r14-r31 in general register save area */
    std     14, (OFS_R14_R31 + 0 * 8)(1)
    std     15, (OFS_R14_R31 + 1 * 8)(1)
    std     16, (OFS_R14_R31 + 2 * 8)(1)
    std     17, (OFS_R14_R31 + 3 * 8)(1)
    std     18, (OFS_R14_R31 + 4 * 8)(1)
    std     19, (OFS_R14_R31 + 5 * 8)(1)
    std     20, (OFS_R14_R31 + 6 * 8)(1)
    std     21, (OFS_R14_R31 + 7 * 8)(1)
    std     22, (OFS_R14_R31 + 8 * 8)(1)
    std     23, (OFS_R14_R31 + 9 * 8)(1)
    std     24, (OFS_R14_R31 + 10 * 8)(1)
    std     25, (OFS_R14_R31 + 11 * 8)(1)
    std     26, (OFS_R14_R31 + 12 * 8)(1)
    std     27, (OFS_R14_R31 + 13 * 8)(1)
    std     28, (OFS_R14_R31 + 14 * 8)(1)
    std     29, (OFS_R14_R31 + 15 * 8)(1)
    std     30, (OFS_R14_R31 + 16 * 8)(1)
    std     31, (OFS_R14_R31 + 17 * 8)(1)

    /* Save r3-r10 in parameter save area of caller */
    std     3, (OFS_R3_R10 + 0 * 8)(1)
    std     4, (OFS_R3_R10 + 1 * 8)(1)
    std     5, (OFS_R3_R10 + 2 * 8)(1)
    std     6, (OFS_R3_R10 + 3 * 8)(1)
    std     7, (OFS_R3_R10 + 4 * 8)(1)
    std     8, (OFS_R3_R10 + 5 * 8)(1)
    std     9, (OFS_R3_R10 + 6 * 8)(1)
    std     10, (OFS_R3_R10 + 7 * 8)(1)

    /* Save r2 in TOC save area */
    std     2, TOC_OFS(1)

    /* Do not save r11, r12 and r13. */

    /* Call delegate:
     * r3: pointer to context
     * r4: pointer to stack
     */
    mr      5, 4
    mr      4, 1
    ld      6, 0(5)
    ld      11, 16(5)
    ld      2, 8(5)
    mtctr   6
    bctrl
    nop

    /* Restore r2 from TOC save area */
    ld      2, TOC_OFS(1)

    /* Restore r3-r10 from local variable space */
    ld      3, (OFS_R3_R10 + 0 * 8)(1)
    ld      4, (OFS_R3_R10 + 1 * 8)(1)
    ld      5, (OFS_R3_R10 + 2 * 8)(1)
    ld      6, (OFS_R3_R10 + 3 * 8)(1)
    ld      7, (OFS_R3_R10 + 4 * 8)(1)
    ld      8, (OFS_R3_R10 + 5 * 8)(1)
    ld      9, (OFS_R3_R10 + 6 * 8)(1)
    ld      10, (OFS_R3_R10 + 7 * 8)(1)

    /* Restore r14-r31 from general register save area */
    ld      14, (OFS_R14_R31 + 0 * 8)(1)
    ld      15, (OFS_R14_R31 + 1 * 8)(1)
    ld      16, (OFS_R14_R31 + 2 * 8)(1)
    ld      17, (OFS_R14_R31 + 3 * 8)(1)
    ld      18, (OFS_R14_R31 + 4 * 8)(1)
    ld      19, (OFS_R14_R31 + 5 * 8)(1)
    ld      20, (OFS_R14_R31 + 6 * 8)(1)
    ld      21, (OFS_R14_R31 + 7 * 8)(1)
    ld      22, (OFS_R14_R31 + 8 * 8)(1)
    ld      23, (OFS_R14_R31 + 9 * 8)(1)
    ld      24, (OFS_R14_R31 + 10 * 8)(1)
    ld      25, (OFS_R14_R31 + 11 * 8)(1)
    ld      26, (OFS_R14_R31 + 12 * 8)(1)
    ld      27, (OFS_R14_R31 + 13 * 8)(1)
    ld      28, (OFS_R14_R31 + 14 * 8)(1)
    ld      29, (OFS_R14_R31 + 15 * 8)(1)
    ld      30, (OFS_R14_R31 + 16 * 8)(1)
    ld      31, (OFS_R14_R31 + 17 * 8)(1)

    ld      0, LR_OFS(1)
    mtlr    0
    addi    1, 1, STACK_SZ
    blr
    .long 0
    .quad 0
.Lend:
    .size _D4core6thread18callWithStackShellFNbMDFNbPvZvZv, .Lend-.L._D4core6thread18callWithStackShellFNbMDFNbPvZvZv
    .cfi_endproc

#elif defined( __ppc__ ) || defined( __PPC__ ) || defined( __powerpc__ )


/**
 * Performs a context switch.
 *
 * r3 - old context pointer
 * r4 - new context pointer
 *
 */
.text
.align 2
.globl _fiber_switchContext
_fiber_switchContext:

    /* Save linkage area */
    mflr        0
    mfcr        5
    stw     0, 8(1)
    stw     5, 4(1)

    /* Save GPRs */
    stw     11, (-1 * 4)(1)
    stw     13, (-2 * 4)(1)
    stw     14, (-3 * 4)(1)
    stw     15, (-4 * 4)(1)
    stw     16, (-5 * 4)(1)
    stw     17, (-6 * 4)(1)
    stw     18, (-7 * 4)(1)
    stw     19, (-8 * 4)(1)
    stw     20, (-9 * 4)(1)
    stw     21, (-10 * 4)(1)
    stw     22, (-11 * 4)(1)
    stw     23, (-12 * 4)(1)
    stw     24, (-13 * 4)(1)
    stw     25, (-14 * 4)(1)
    stw     26, (-15 * 4)(1)
    stw     27, (-16 * 4)(1)
    stw     28, (-17 * 4)(1)
    stw     29, (-18 * 4)(1)
    stw     30, (-19 * 4)(1)
    stwu    31, (-20 * 4)(1)

    /* We update the stack pointer here, since we do not want the GC to
       scan the floating point registers. */

    /* Save FPRs */
    stfd    14, (-1 * 8)(1)
    stfd    15, (-2 * 8)(1)
    stfd    16, (-3 * 8)(1)
    stfd    17, (-4 * 8)(1)
    stfd    18, (-5 * 8)(1)
    stfd    19, (-6 * 8)(1)
    stfd    20, (-7 * 8)(1)
    stfd    21, (-8 * 8)(1)
    stfd    22, (-9 * 8)(1)
    stfd    23, (-10 * 8)(1)
    stfd    24, (-11 * 8)(1)
    stfd    25, (-12 * 8)(1)
    stfd    26, (-13 * 8)(1)
    stfd    27, (-14 * 8)(1)
    stfd    28, (-15 * 8)(1)
    stfd    29, (-16 * 8)(1)
    stfd    30, (-17 * 8)(1)
    stfd    31, (-18 * 8)(1)

    /* Update the old stack pointer */
    stw     1, 0(3)

    /* Set new stack pointer */
    addi        1, 4, 20 * 4

    /* Restore linkage area */
    lwz     0, 8(1)
    lwz     5, 4(1)

    /* Restore GPRs */
    lwz     11, (-1 * 4)(1)
    lwz     13, (-2 * 4)(1)
    lwz     14, (-3 * 4)(1)
    lwz     15, (-4 * 4)(1)
    lwz     16, (-5 * 4)(1)
    lwz     17, (-6 * 4)(1)
    lwz     18, (-7 * 4)(1)
    lwz     19, (-8 * 4)(1)
    lwz     20, (-9 * 4)(1)
    lwz     21, (-10 * 4)(1)
    lwz     22, (-11 * 4)(1)
    lwz     23, (-12 * 4)(1)
    lwz     24, (-13 * 4)(1)
    lwz     25, (-14 * 4)(1)
    lwz     26, (-15 * 4)(1)
    lwz     27, (-16 * 4)(1)
    lwz     28, (-17 * 4)(1)
    lwz     29, (-18 * 4)(1)
    lwz     30, (-19 * 4)(1)
    lwz     31, (-20 * 4)(1)


    /* Restore FPRs */
    lfd     14, (-1 * 8)(4)
    lfd     15, (-2 * 8)(4)
    lfd     16, (-3 * 8)(4)
    lfd     17, (-4 * 8)(4)
    lfd     18, (-5 * 8)(4)
    lfd     19, (-6 * 8)(4)
    lfd     20, (-7 * 8)(4)
    lfd     21, (-8 * 8)(4)
    lfd     22, (-9 * 8)(4)
    lfd     23, (-10 * 8)(4)
    lfd     24, (-11 * 8)(4)
    lfd     25, (-12 * 8)(4)
    lfd     26, (-13 * 8)(4)
    lfd     27, (-14 * 8)(4)
    lfd     28, (-15 * 8)(4)
    lfd     29, (-16 * 8)(4)
    lfd     30, (-17 * 8)(4)
    lfd     31, (-18 * 8)(4)

    /* Set condition and link register */
    mtcr        5
    mtlr        0

    /* Return and switch context */
    blr

#elif defined(__mips__) && _MIPS_SIM == _ABIO32
/************************************************************************************
 * MIPS ASM BITS
 ************************************************************************************/

/**
 * Performs a context switch.
 *
 * $a0 - void** - ptr to old stack pointer
 * $a1 - void*  - new stack pointer
 *
 */
.text
.globl fiber_switchContext
fiber_switchContext:
    addiu $sp, $sp, -(10 * 4)

    // fp regs and return address are stored below the stack
    // because we don't want the GC to scan them.

#ifdef __mips_hard_float
#define ALIGN8(val) (val + (-val & 7))
#define BELOW (ALIGN8(6 * 8 + 4))
    sdc1 $f20, (0 * 8 - BELOW)($sp)
    sdc1 $f22, (1 * 8 - BELOW)($sp)
    sdc1 $f24, (2 * 8 - BELOW)($sp)
    sdc1 $f26, (3 * 8 - BELOW)($sp)
    sdc1 $f28, (4 * 8 - BELOW)($sp)
    sdc1 $f30, (5 * 8 - BELOW)($sp)
#endif
    sw $ra, -4($sp)

    sw $s0, (0 * 4)($sp)
    sw $s1, (1 * 4)($sp)
    sw $s2, (2 * 4)($sp)
    sw $s3, (3 * 4)($sp)
    sw $s4, (4 * 4)($sp)
    sw $s5, (5 * 4)($sp)
    sw $s6, (6 * 4)($sp)
    sw $s7, (7 * 4)($sp)
    sw $s8, (8 * 4)($sp)
    sw $gp, (9 * 4)($sp)

    // swap stack pointer
    sw $sp, 0($a0)
    move $sp, $a1

#ifdef __mips_hard_float
    ldc1 $f20, (0 * 8 - BELOW)($sp)
    ldc1 $f22, (1 * 8 - BELOW)($sp)
    ldc1 $f24, (2 * 8 - BELOW)($sp)
    ldc1 $f26, (3 * 8 - BELOW)($sp)
    ldc1 $f28, (4 * 8 - BELOW)($sp)
    ldc1 $f30, (5 * 8 - BELOW)($sp)
#endif
    lw $ra, -4($sp)

    lw $s0, (0 * 4)($sp)
    lw $s1, (1 * 4)($sp)
    lw $s2, (2 * 4)($sp)
    lw $s3, (3 * 4)($sp)
    lw $s4, (4 * 4)($sp)
    lw $s5, (5 * 4)($sp)
    lw $s6, (6 * 4)($sp)
    lw $s7, (7 * 4)($sp)
    lw $s8, (8 * 4)($sp)
    lw $gp, (9 * 4)($sp)

    addiu $sp, $sp, (10 * 4)

    jr $ra // return

#elif defined(__arm__) && defined(__ARM_EABI__)
/************************************************************************************
 * ARM ASM BITS
 ************************************************************************************/

/**
 * Performs a context switch.
 *
 * Parameters:
 * r0 - void** - ptr to old stack pointer
 * r1 - void*  - new stack pointer
 *
 * ARM EABI registers:
 * r0-r3   : argument/scratch registers
 * r4-r10  : callee-save registers
 * r11     : frame pointer (or a callee save register if fp isn't needed)
 * r12 =ip : inter procedure register. We can treat it like any other scratch register
 * r13 =sp : stack pointer
 * r14 =lr : link register, it contains the return address (belonging to the function which called us)
 * r15 =pc : program counter
 *
 * For floating point registers:
 * According to AAPCS (version 2.09, section 5.1.2) only the d8-d15 registers need to be preserved
 * across method calls. This applies to all ARM FPU variants, whether they have 16 or 32 double registers
 * NEON support or not, half-float support or not and so on does not matter.
 *
 * Note: If this file was compiled with -mfloat-abi=soft but the code runs on a softfp system with fpu the d8-d15
 * registers won't be saved (we do not know that the system has got a fpu in that case) but the registers might actually
 * be used by other code if it was compiled with -mfloat-abi=softfp.
 *
 * Interworking is only supported on ARMv5+, not on ARM v4T as ARM v4t requires special stubs when changing
 * from thumb to arm mode or the other way round.
 */

.text
.align  2
.global fiber_switchContext
#if defined(__ARM_PCS_VFP) || (defined(__ARM_PCS) && !defined(__SOFTFP__)) // ARM_HardFloat  || ARM_SoftFP
  .fpu vfp
#endif
.type   fiber_switchContext, %function
fiber_switchContext:
    .fnstart
    push {r4-r11}
    // update the oldp pointer. Link register and floating point registers stored later to prevent the GC from
    // scanning them.
    str sp, [r0]
    // push r0 (or any other register) as well to keep stack 8byte aligned
    push {r0, lr}

    #if defined(__ARM_PCS_VFP) || (defined(__ARM_PCS) && !defined(__SOFTFP__)) // ARM_HardFloat  || ARM_SoftFP
      vpush {d8-d15}
      // now switch over to the new stack. Need to subtract (8*8[d8-d15]+2*4[r0, lr]) to position stack pointer
      // below the last saved register. Remember we saved the SP before pushing [r0, lr, d8-d15]
      sub sp, r1, #72
      vpop {d8-d15}
    #else
      sub sp, r1, #8
    #endif

    // we don't really care about r0, we only used that for padding.
    // r1 is now what used to be in the link register when saving.
    pop {r0, r1, r4-r11}
    /**
     * The link register for the initial jump to fiber_entryPoint must be zero: The jump actually
     * looks like a normal method call as we jump to the start of the fiber_entryPoint function.
     * Although fiber_entryPoint never returns and therefore never accesses lr, it saves lr to the stack.
     * ARM unwinding will then look at the stack, find lr and think that fiber_entryPoint was called by
     * the function in lr! So if we have some address in lr the unwinder will try to continue stack unwinding,
     * although it's already at the stack base and crash.
     * In all other cases the content of lr doesn't matter.
     * Note: If we simply loaded into lr above and then moved lr into pc, the initial method call
     * to fiber_entryPoint would look as if it was called from fiber_entryPoint itself, as the fiber_entryPoint
     * address is in lr on the initial context switch.
     */
    mov lr, #0
    // return by writing lr into pc
    mov pc, r1
    .fnend

#elif defined(__aarch64__)
/************************************************************************************
 * AArch64 (arm64) ASM BITS
 ************************************************************************************/
/**
 * preserve/restore AAPCS64 registers
 *   x19-x28 5.1.1 64-bit callee saved
 *   x29 fp, or possibly callee saved reg - depends on platform choice 5.2.3)
 *   x30 lr
 *   d8-d15  5.1.2 says callee only must save bottom 64-bits (the "d" regs)
 *
 * saved regs on stack will look like:
 *   19: x19
 *   18: x20
 *   ...
 *   10: x28
 *    9: x29 (fp)  <-- oldp / *newp save stack top
 *    8: x30 (lr)
 *    7: d8
 *   ...
 *    0: d15       <-- sp
 */
        .text
        .global CSYM(fiber_switchContext)
        .type   fiber_switchContext, %function
        .p2align  2
CSYM(fiber_switchContext):
        stp     d15, d14, [sp, #-20*8]!
        stp     d13, d12, [sp, #2*8]
        stp     d11, d10, [sp, #4*8]
        stp     d9, d8,   [sp, #6*8]
        stp     x30, x29, [sp, #8*8] // lr, fp
        stp     x28, x27, [sp, #10*8]
        stp     x26, x25, [sp, #12*8]
        stp     x24, x23, [sp, #14*8]
        stp     x22, x21, [sp, #16*8]
        stp     x20, x19, [sp, #18*8]

        // oldp is set above saved lr (x30) to hide it and float regs
        // from GC
        add     x19, sp, #9*8
        str     x19, [x0]       // *oldp tstack
        sub     sp, x1, #9*8    // switch to newp sp

        ldp     x20, x19, [sp, #18*8]
        ldp     x22, x21, [sp, #16*8]
        ldp     x24, x23, [sp, #14*8]
        ldp     x26, x25, [sp, #12*8]
        ldp     x28, x27, [sp, #10*8]
        ldp     x30, x29, [sp, #8*8] // lr, fp
        ldp     d9, d8,   [sp, #6*8]
        ldp     d11, d10, [sp, #4*8]
        ldp     d13, d12, [sp, #2*8]
        ldp     d15, d14, [sp], #20*8
        ret


/**
 * When generating any kind of backtrace (gdb, exception handling) for
 * a function called in a Fiber, we need to tell the unwinder to stop
 * at our Fiber main entry point, i.e. we need to mark the bottom of
 * the call stack. This can be done by clearing the link register lr
 * prior to calling fiber_entryPoint (i.e. in fiber_switchContext) or
 * using a .cfi_undefined directive for the link register in the
 * Fiber entry point. cfi_undefined seems to yield better results in gdb.
 * Unfortunately we can't place it into fiber_entryPoint using inline
 * asm, so we use this trampoline instead.
 */
        .text
        .global CSYM(fiber_trampoline)
        .p2align  2
        .type   fiber_trampoline, %function
CSYM(fiber_trampoline):
        .cfi_startproc
        .cfi_undefined x30
        // fiber_entryPoint never returns
        bl fiber_entryPoint
        .cfi_endproc

#elif defined(__MINGW32__)
/************************************************************************************
 * GDC MinGW ASM BITS
 ************************************************************************************/
#if defined(__x86_64__)
.global fiber_switchContext
fiber_switchContext:
    pushq %RBP;
    movq %RSP, %RBP;
    pushq %RBX;
    pushq %R12;
    pushq %R13;
    pushq %R14;
    pushq %R15;
    pushq %GS:0;
    pushq %GS:8;
    pushq %GS:16;

    // store oldp
    movq %RSP, (%RCX);
    // load newp to begin context switch
    movq %RDX, %RSP;

    // load saved state from new stack
    popq %GS:16;
    popq %GS:8;
    popq %GS:0;
    popq %R15;
    popq %R14;
    popq %R13;
    popq %R12;
    popq %RBX;
    popq %RBP;

    // 'return' to complete switch
    popq %RCX;
    jmp *%RCX;
#elif defined(_X86_)
.global _fiber_switchContext
_fiber_switchContext:
    // Save current stack state.save current stack state
    // Standard CDECL prologue.
    push %EBP;
    mov  %ESP, %EBP;
    push %EDI;
    push %ESI;
    push %EBX;
    push %FS:0;
    push %FS:4;
    push %FS:8;
    push %EAX;

    // store oldp again with more accurate address
    mov 8(%EBP), %EAX;
    mov %ESP, (%EAX);
    // load newp to begin context switch
    mov 12(%EBP), %ESP;

    // load saved state from new stack
    pop %EAX;
    pop %FS:8;
    pop %FS:4;
    pop %FS:0;
    pop %EBX;
    pop %ESI;
    pop %EDI;
    pop %EBP;

    // 'return' to complete switch
    ret;
#endif

// if POSIX boils down to this (reference http://nadeausoftware.com)
#elif !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
/************************************************************************************
 * i386- and x86_64-apple-darwin POSIX ASM BITS
 ************************************************************************************/
#if defined(__i386__)
.text
.p2align 4
.globl CSYM(fiber_switchContext)
CSYM(fiber_switchContext):
    // save current stack state
    push %ebp
    mov  %esp, %ebp
    push %edi
    push %esi
    push %ebx
    push %eax

    // store oldp again with more accurate address
    mov 8(%ebp), %eax
    mov %esp, (%eax)
    // load newp to begin context switch
    mov 12(%ebp), %esp

    // load saved state from new stack
    pop %eax
    pop %ebx
    pop %esi
    pop %edi
    pop %ebp

    // 'return' to complete switch
    ret

#elif defined(__x86_64__) && !defined(__ILP32__)
.text
.p2align 4
.globl CSYM(fiber_switchContext)
CSYM(fiber_switchContext):
    // Save current stack state.save current stack state
    push %rbp
    mov  %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15

    // store oldp again with more accurate address
    mov %rsp, (%rdi)
    // load newp to begin context switch
    mov %rsi, %rsp

    // load saved state from new stack
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp

    // 'return' to complete switch
    ret
#endif	// __x86_64__ && !__ILP32__

#endif	// posix
