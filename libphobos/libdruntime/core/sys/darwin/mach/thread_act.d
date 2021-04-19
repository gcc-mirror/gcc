/**
 * D header file for Darwin.
 *
 * Copyright: Copyright Sean Kelly 2008 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 */

/*          Copyright Sean Kelly 2008 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.darwin.mach.thread_act;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Darwin):
extern (C):
nothrow:
@nogc:

public import core.sys.darwin.mach.kern_return;
public import core.sys.darwin.mach.port;

version (X86)
    version = i386;
version (X86_64)
    version = i386;
version (AArch64)
    version = AnyARM;
version (ARM)
    version = AnyARM;
version (PPC)
    version = AnyPPC;
version (PPC64)
    version = AnyPPC;

version (i386)
{
    alias mach_port_t thread_act_t;
    alias void        thread_state_t;
    alias int         thread_state_flavor_t;
    alias natural_t   mach_msg_type_number_t;

    enum
    {
        x86_THREAD_STATE32      = 1,
        x86_FLOAT_STATE32       = 2,
        x86_EXCEPTION_STATE32   = 3,
        x86_THREAD_STATE64      = 4,
        x86_FLOAT_STATE64       = 5,
        x86_EXCEPTION_STATE64   = 6,
        x86_THREAD_STATE        = 7,
        x86_FLOAT_STATE         = 8,
        x86_EXCEPTION_STATE     = 9,
        x86_DEBUG_STATE32       = 10,
        x86_DEBUG_STATE64       = 11,
        x86_DEBUG_STATE         = 12,
        THREAD_STATE_NONE       = 13,
    }

    struct x86_thread_state32_t
    {
        uint    eax;
        uint    ebx;
        uint    ecx;
        uint    edx;
        uint    edi;
        uint    esi;
        uint    ebp;
        uint    esp;
        uint    ss;
        uint    eflags;
        uint    eip;
        uint    cs;
        uint    ds;
        uint    es;
        uint    fs;
        uint    gs;
    }

    struct x86_thread_state64_t
    {
        ulong   rax;
        ulong   rbx;
        ulong   rcx;
        ulong   rdx;
        ulong   rdi;
        ulong   rsi;
        ulong   rbp;
        ulong   rsp;
        ulong   r8;
        ulong   r9;
        ulong   r10;
        ulong   r11;
        ulong   r12;
        ulong   r13;
        ulong   r14;
        ulong   r15;
        ulong   rip;
        ulong   rflags;
        ulong   cs;
        ulong   fs;
        ulong   gs;
    }

    struct x86_state_hdr_t
    {
        int     flavor;
        int     count;
    }

    struct x86_thread_state_t
    {
        x86_state_hdr_t             tsh;
        union _uts
        {
            x86_thread_state32_t    ts32;
            x86_thread_state64_t    ts64;
        }
        _uts                        uts;
    }

    enum : mach_msg_type_number_t
    {
        x86_THREAD_STATE32_COUNT = cast(mach_msg_type_number_t)( x86_thread_state32_t.sizeof / int.sizeof ),
        x86_THREAD_STATE64_COUNT = cast(mach_msg_type_number_t)( x86_thread_state64_t.sizeof / int.sizeof ),
        x86_THREAD_STATE_COUNT   = cast(mach_msg_type_number_t)( x86_thread_state_t.sizeof / int.sizeof ),
    }

    alias x86_THREAD_STATE          MACHINE_THREAD_STATE;
    alias x86_THREAD_STATE_COUNT    MACHINE_THREAD_STATE_COUNT;

    mach_port_t   mach_thread_self();
    kern_return_t thread_suspend(thread_act_t);
    kern_return_t thread_resume(thread_act_t);
    kern_return_t thread_get_state(thread_act_t, thread_state_flavor_t, thread_state_t*, mach_msg_type_number_t*);
}
// https://github.com/apple/darwin-xnu/blob/master/osfmk/mach/arm/_structs.h
// https://github.com/apple/darwin-xnu/blob/master/osfmk/mach/arm/thread_status.h
else version (AnyARM)
{
    alias thread_act_t = mach_port_t;
    alias thread_state_t = void;
    alias thread_state_flavor_t = int;
    alias mach_msg_type_number_t = natural_t;

    enum
    {
        ARM_THREAD_STATE = 1,
        ARM_UNIFIED_THREAD_STATE = ARM_THREAD_STATE,
        ARM_VFP_STATE = 2,
        ARM_EXCEPTION_STATE = 3,
        ARM_DEBUG_STATE = 4, /* pre-armv8 */
        THREAD_STATE_NONE = 5,
        ARM_THREAD_STATE64 = 6,
        ARM_EXCEPTION_STATE64 = 7,
        // ARM_THREAD_STATE_LAST = 8, /* legacy */
        ARM_THREAD_STATE32 = 9
    }

    enum
    {
        ARM_DEBUG_STATE32 = 14,
        ARM_DEBUG_STATE64 = 15,
        ARM_NEON_STATE = 16,
        ARM_NEON_STATE64 = 17,
        ARM_CPMU_STATE64 = 18
    }

    enum
    {
        ARM_AMX_STATE = 24,
        ARM_AMX_STATE_V1 = 25
    }

    struct arm_thread_state_t
    {
        uint[13] r; /// General purpose register r0-r12
        uint sp;    /// Stack pointer r13
        uint lr;    /// Link register r14
        uint pc;    /// Program counter r15
        uint cpsr;  /// Current program status register
    }

    alias arm_thread_state32_t = arm_thread_state_t;

    struct arm_thread_state64_t
    {
        ulong[29] x; /// General purpose registers x0-x28
        ulong fp; /// Frame pointer x29
        ulong lr; /// Link register x30
        ulong sp; /// Stack pointer x31
        ulong pc; /// Program counter
        ulong cpsr; /// Current program status register
        ulong pad; /// Same size for 32-bit or 64-bit clients
    }

    struct arm_state_hdr_t
    {
        uint flavor;
        uint count;
    }

    struct arm_unified_thread_state_t
    {
        arm_state_hdr_t ash;

        union _uts
        {
            arm_thread_state32_t ts_32;
            arm_thread_state64_t ts_64;
        }

        _uts uts;
    }

    enum : mach_msg_type_number_t
    {
        ARM_THREAD_STATE_COUNT = cast(mach_msg_type_number_t) (arm_thread_state_t.sizeof / uint.sizeof),
        ARM_THREAD_STATE32_COUNT = cast(mach_msg_type_number_t) (arm_thread_state32_t.sizeof / uint.sizeof),
        ARM_THREAD_STATE64_COUNT = cast(mach_msg_type_number_t) (arm_thread_state64_t.sizeof / uint.sizeof),
        ARM_UNIFIED_THREAD_STATE_COUNT = cast(mach_msg_type_number_t) (arm_unified_thread_state_t.sizeof / uint.sizeof)
    }

    alias MACHINE_THREAD_STATE = ARM_THREAD_STATE;
    alias MACHINE_THREAD_STATE_COUNT = ARM_UNIFIED_THREAD_STATE_COUNT;

    mach_port_t   mach_thread_self();
    kern_return_t thread_suspend(thread_act_t);
    kern_return_t thread_resume(thread_act_t);
    kern_return_t thread_get_state(thread_act_t, thread_state_flavor_t, thread_state_t*, mach_msg_type_number_t*);
}
else version (AnyPPC)
{
    alias thread_act_t = mach_port_t;
    alias thread_state_t = void;
    alias thread_state_flavor_t = int;
    alias mach_msg_type_number_t = natural_t;

    enum
    {
        PPC_THREAD_STATE = 1,
        PPC_FLOAT_STATE = 2,
        PPC_EXCEPTION_STATE = 3,
        PPC_VECTOR_STATE = 4,
        PPC_THREAD_STATE64 = 5,
        PPC_EXCEPTION_STATE64 = 6,
        THREAD_STATE_NONE = 7
    }

    struct ppc_thread_state_t
    {
        uint srr0;   /// Instruction address register (PC)
        uint srr1;   /// Machine state register (supervisor)
        uint[32] r;  /// General purpose register r0-r31
        uint cr;     /// Condition register
        uint xer;    /// User's integer exception register
        uint lr;     /// Link register
        uint ctr;    /// Count register
        uint mq;     /// MQ register (601 only)
        uint vrsave; /// Vector save register
    }

    alias ppc_thread_state32_t = ppc_thread_state_t;

    struct ppc_thread_state64_t
    {
        ulong srr0;   /// Instruction address register (PC)
        ulong srr1;   /// Machine state register (supervisor)
        ulong[32] r;  /// General purpose register r0-r31
        uint cr;      /// Condition register
        uint pad0;
        ulong xer;    /// User's integer exception register
        ulong lr;     /// Link register
        ulong ctr;    /// Count register
        uint vrsave;  /// Vector save register
        uint pad1;
    }

    enum : mach_msg_type_number_t
    {
        PPC_THREAD_STATE_COUNT = cast(mach_msg_type_number_t) (ppc_thread_state_t.sizeof / uint.sizeof),
        PPC_THREAD_STATE32_COUNT = cast(mach_msg_type_number_t) (ppc_thread_state32_t.sizeof / uint.sizeof),
        PPC_THREAD_STATE64_COUNT = cast(mach_msg_type_number_t) (ppc_thread_state64_t.sizeof / uint.sizeof),
    }

    alias MACHINE_THREAD_STATE = PPC_THREAD_STATE;
    alias MACHINE_THREAD_STATE_COUNT = PPC_THREAD_STATE_COUNT;

    mach_port_t   mach_thread_self();
    kern_return_t thread_suspend(thread_act_t);
    kern_return_t thread_resume(thread_act_t);
    kern_return_t thread_get_state(thread_act_t, thread_state_flavor_t, thread_state_t*, mach_msg_type_number_t*);
}
