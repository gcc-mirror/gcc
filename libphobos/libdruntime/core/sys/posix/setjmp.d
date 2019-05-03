/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.setjmp;

private import core.sys.posix.config;
private import core.sys.posix.signal; // for sigset_t

version (Posix):
extern (C) nothrow @nogc:

version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;

//
// Required
//
/*
jmp_buf

int  setjmp(ref jmp_buf);
void longjmp(ref jmp_buf, int);
*/

version (CRuntime_Glibc)
{
    version (X86_64)
    {
        //enum JB_BX      = 0;
        //enum JB_BP      = 1;
        //enum JB_12      = 2;
        //enum JB_13      = 3;
        //enum JB_14      = 4;
        //enum JB_15      = 5;
        //enum JB_SP      = 6;
        //enum JB_PC      = 7;
        //enum JB_SIZE    = 64;

        alias long[8] __jmp_buf;
    }
    else version (X86)
    {
        //enum JB_BX      = 0;
        //enum JB_SI      = 1;
        //enum JB_DI      = 2;
        //enum JB_BP      = 3;
        //enum JB_SP      = 4;
        //enum JB_PC      = 5;
        //enum JB_SIZE    = 24;

        alias int[6] __jmp_buf;
    }
    else version (SPARC)
    {
        alias int[3] __jmp_buf;
    }
    else version (SPARC64)
    {
        alias __jmp_buf = ulong[22];
    }
    else version (AArch64)
    {
        alias long[22] __jmp_buf;
    }
    else version (ARM)
    {
        alias int[64] __jmp_buf;
    }
    else version (HPPA)
    {
        struct __jmp_buf
        {
            int __r3;
            int[15] __r4_r18;
            int __r19;
            int __r27;
            int __sp;
            int __rp;
            int __pad1;
            double[10] __fr12_fr21;
        }
    }
    else version (PPC)
    {
        alias int[64 + (12*4)] __jmp_buf;
    }
    else version (PPC64)
    {
        alias long[64] __jmp_buf;
    }
    else version (MIPS32)
    {
        struct __jmp_buf
        {
            version (MIPS_O32)
            {
                void * __pc;
                void * __sp;
                int[8] __regs;
                void * __fp;
                void * __gp;
            }
            else
            {
                long __pc;
                long __sp;
                long[8] __regs;
                long __fp;
                long __gp;
            }
            int __fpc_csr;
            version (MIPS_N64)
                double[8] __fpregs;
            else
                double[6] __fpregs;
        }
    }
    else version (MIPS64)
    {
        struct __jmp_buf
        {
            long __pc;
            long __sp;
            long[8] __regs;
            long __fp;
            long __gp;
            int __fpc_csr;
            version (MIPS_N64)
                double[8] __fpregs;
            else
                double[6] __fpregs;
        }
    }
    else version (RISCV_Any)
    {
        struct __riscv_jmp_buf
        {
            c_long __pc;
            c_long[12] __regs;
            c_long __sp;
            double[12] __fpregs;
        }
        alias __jmp_buf = __riscv_jmp_buf[1];
    }
    else version (S390)
    {
        struct __s390_jmp_buf
        {
            c_long[10] __gregs;
            c_long[4] __fpregs;
        }
        alias __jmp_buf = __s390_jmp_buf[1];
    }
    else version (SystemZ)
    {
        struct __s390_jmp_buf
        {
            c_long[10] __gregs;
            c_long[8] __fpregs;
        }
        alias __jmp_buf = __s390_jmp_buf[1];
    }
    else
        static assert(0, "unimplemented");

    struct __jmp_buf_tag
    {
        __jmp_buf   __jmpbuf;
        int         __mask_was_saved;
        sigset_t    __saved_mask;
    }

    alias __jmp_buf_tag[1] jmp_buf;

    alias _setjmp setjmp; // see XOpen block
    void longjmp(ref jmp_buf, int);
}
else version (FreeBSD)
{
    // <machine/setjmp.h>
    version (X86)
    {
        enum _JBLEN = 11;
        struct _jmp_buf { int[_JBLEN + 1] _jb; }
    }
    else version (X86_64)
    {
        enum _JBLEN = 12;
        struct _jmp_buf { c_long[_JBLEN] _jb; }
    }
    else version (SPARC)
    {
        enum _JBLEN = 5;
        struct _jmp_buf { c_long[_JBLEN + 1] _jb; }
    }
    else version (AArch64)
    {
        enum _JBLEN = 31;
        // __int128_t
        struct _jmp_buf { long[2][_JBLEN + 1] _jb; };
    }
    else
        static assert(0);
    alias _jmp_buf[1] jmp_buf;

    int  setjmp(ref jmp_buf);
    void longjmp(ref jmp_buf, int);
}
else version (NetBSD)
{
    // <machine/setjmp.h>
    version (X86)
    {
        enum _JBLEN = 13;
        struct _jmp_buf { int[_JBLEN + 1] _jb; }
    }
    else version (X86_64)
    {
        enum _JBLEN = 11;
        struct _jmp_buf { c_long[_JBLEN] _jb; }
    }
    else
        static assert(0);
    alias _jmp_buf[_JBLEN] jmp_buf;

    int  setjmp(ref jmp_buf);
    void longjmp(ref jmp_buf, int);
}
else version (OpenBSD)
{
    // <machine/setjmp.h>
    version (X86)
    {
        enum _JBLEN = 10;
    }
    else version (X86_64)
    {
        enum _JBLEN = 11;
    }
    else version (ARM)
    {
        enum _JBLEN = 64;
    }
    else version (PPC)
    {
        enum _JBLEN = 100;
    }
    else version (MIPS64)
    {
        enum _JBLEN = 83;
    }
    else version (SPARC)
    {
        enum _JBLEN = 10;
    }
    else version (SPARC64)
    {
        enum _JBLEN = 14;
    }
    else
        static assert(0);

    alias jmp_buf = c_long[_JBLEN];

    int  setjmp(ref jmp_buf);
    void longjmp(ref jmp_buf, int);
}
else version (DragonFlyBSD)
{
    // <machine/setjmp.h>
    version (X86_64)
    {
        enum _JBLEN = 12;
        struct _jmp_buf { c_long[_JBLEN] _jb; }
    }
    else
        static assert(0);
    alias _jmp_buf[1] jmp_buf;

    int  setjmp(ref jmp_buf);
    void longjmp(ref jmp_buf, int);
}
else version (CRuntime_Bionic)
{
    // <machine/setjmp.h>
    version (X86)
    {
        enum _JBLEN = 10;
    }
    else version (ARM)
    {
        enum _JBLEN = 64;
    }
    else version (AArch64)
    {
        enum _JBLEN = 32;
    }
    else version (X86_64)
    {
        enum _JBLEN = 11;
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }

    alias c_long[_JBLEN] jmp_buf;

    int  setjmp(ref jmp_buf);
    void longjmp(ref jmp_buf, int);
}
else version (CRuntime_UClibc)
{
    version (X86_64)
    {
        alias long[8] __jmp_buf;
    }
    else version (ARM)
    {
        align(8) alias int[64] __jmp_buf;
    }
    else version (MIPS32)
    {
        struct __jmp_buf
        {
            version (MIPS_O32)
            {
                void * __pc;
                void * __sp;
                int[8] __regs;
                void * __fp;
                void * __gp;
            }
            else
            {
                long __pc;
                long __sp;
                long[8] __regs;
                long __fp;
                long __gp;
            }
            int __fpc_csr;
            version (MIPS_N64)
                double[8] __fpregs;
            else
                double[6] __fpregs;
        };
    }
    else
        static assert(0, "unimplemented");

    struct __jmp_buf_tag
    {
        __jmp_buf   __jmpbuf;
        int         __mask_was_saved;
        sigset_t    __saved_mask;
    }

    alias __jmp_buf_tag[1] jmp_buf;

    alias _setjmp setjmp;
    void longjmp(ref jmp_buf, int);
}

//
// C Extension (CX)
//
/*
sigjmp_buf

int  sigsetjmp(sigjmp_buf, int);
void siglongjmp(sigjmp_buf, int);
*/

version (CRuntime_Glibc)
{
    alias jmp_buf sigjmp_buf;

    int __sigsetjmp(sigjmp_buf, int);
    alias __sigsetjmp sigsetjmp;
    void siglongjmp(sigjmp_buf, int);
}
else version (FreeBSD)
{
    // <machine/setjmp.h>
    version (X86)
    {
        struct _sigjmp_buf { int[_JBLEN + 1] _ssjb; }
    }
    else version (X86_64)
    {
        struct _sigjmp_buf { c_long[_JBLEN] _sjb; }
    }
    else version (SPARC)
    {
        enum _JBLEN         = 5;
        enum _JB_FP         = 0;
        enum _JB_PC         = 1;
        enum _JB_SP         = 2;
        enum _JB_SIGMASK    = 3;
        enum _JB_SIGFLAG    = 5;
        struct _sigjmp_buf { c_long[_JBLEN + 1] _sjb; }
    }
    else version (AArch64)
    {
        // __int128_t
        struct _sigjmp_buf { long[2][_JBLEN + 1] _jb; };
    }
    else
        static assert(0);
    alias _sigjmp_buf[1] sigjmp_buf;

    int  sigsetjmp(ref sigjmp_buf);
    void siglongjmp(ref sigjmp_buf, int);
}
else version (NetBSD)
{
    // <machine/setjmp.h>
    version (X86)
    {
        struct _sigjmp_buf { int[_JBLEN + 1] _ssjb; }
    }
    else version (X86_64)
    {
        struct _sigjmp_buf { c_long[_JBLEN] _sjb; }
    }
    else
        static assert(0);
    alias _sigjmp_buf[_JBLEN + 1] sigjmp_buf;

    int  sigsetjmp(ref sigjmp_buf);
    void siglongjmp(ref sigjmp_buf, int);
}
else version (OpenBSD)
{
    alias sigjmp_buf = c_long[_JBLEN + 1];

    int  sigsetjmp(ref sigjmp_buf);
    void siglongjmp(ref sigjmp_buf, int);
}
else version (DragonFlyBSD)
{
    // <machine/setjmp.h>
    version (X86_64)
    {
        struct _sigjmp_buf { c_long[_JBLEN] _sjb; }
    }
    else
        static assert(0);
    alias _sigjmp_buf[1] sigjmp_buf;

    int  sigsetjmp(ref sigjmp_buf);
    void siglongjmp(ref sigjmp_buf, int);
}
else version (CRuntime_Bionic)
{
    alias c_long[_JBLEN + 1] sigjmp_buf;

    int  sigsetjmp(ref sigjmp_buf, int);
    void siglongjmp(ref sigjmp_buf, int);
}
else version (CRuntime_UClibc)
{
    alias jmp_buf sigjmp_buf;

    int __sigsetjmp(ref sigjmp_buf, int);
    alias __sigsetjmp sigsetjmp;
    void siglongjmp(ref sigjmp_buf, int);
}

//
// XOpen (XSI)
//
/*
int  _setjmp(jmp_buf);
void _longjmp(jmp_buf, int);
*/

version (CRuntime_Glibc)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (FreeBSD)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (NetBSD)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (OpenBSD)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (DragonFlyBSD)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (CRuntime_Bionic)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
else version (CRuntime_UClibc)
{
    int  _setjmp(ref jmp_buf);
    void _longjmp(ref jmp_buf, int);
}
