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
