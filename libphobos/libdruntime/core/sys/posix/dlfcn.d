/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.dlfcn;

import core.sys.posix.config;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

version (Posix):
extern (C):
nothrow:
@nogc:

//
// XOpen (XSI)
//
/*
RTLD_LAZY
RTLD_NOW
RTLD_GLOBAL
RTLD_LOCAL

int   dlclose(void*);
char* dlerror();
void* dlopen(const scope char*, int);
void* dlsym(void*, const scope char*);
*/

version (CRuntime_Glibc)
{
    version (X86_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0x00000;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (HPPA_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=ports/sysdeps/hppa/bits/dlfcn.h
        enum RTLD_LAZY      = 0x0001;
        enum RTLD_NOW       = 0x0002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x0100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (MIPS_Any)
    {
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=ports/sysdeps/mips/bits/dlfcn.h
        enum RTLD_LAZY      = 0x0001;
        enum RTLD_NOW       = 0x0002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00008;
        enum RTLD_DEEPBIND  = 0x00010;
        enum RTLD_GLOBAL    = 0x0004;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (PPC_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (ARM_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (RISCV_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (SPARC_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;

    }
    else version (IBMZ_Any)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else version (LoongArch64)
    {
        // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/dlfcn.h
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_BINDING_MASK = 0x3;
        enum RTLD_NOLOAD    = 0x00004;
        enum RTLD_DEEPBIND  = 0x00008;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
        enum RTLD_NODELETE  = 0x01000;
    }
    else
        static assert(0, "unimplemented");

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int dladdr(const scope void*, Dl_info*);

    struct Dl_info
    {
        const(char)* dli_fname;
        void* dli_fbase;
        const(char)* dli_sname;
        void* dli_saddr;
    }
}
else
version (CRuntime_Musl)
{
    enum RTLD_LAZY   = 1;
    enum RTLD_NOW    = 2;
    enum RTLD_NOLOAD = 4;
    enum RTLD_NODELETE = 4096;
    enum RTLD_GLOBAL = 256;
    enum RTLD_LOCAL  = 0;

    enum RTLD_NEXT    = cast(void *)-1;
    enum RTLD_DEFAULT = cast(void *)0;

    enum RTLD_DI_LINKMAP = 2;

    int    dlclose(void *);
    char  *dlerror();
    void  *dlopen(const(char) *, int);

    pragma(mangle, muslRedirTime64Mangle!("dlsym", "__dlsym_time64"))
    void  *dlsym(void *__restrict, const(char) *__restrict);

    struct Dl_info
    {
        const(char)* dli_fname;
        void* dli_fbase;
        const(char)* dli_sname;
        void* dli_saddr;
    }
    int dladdr(const(void) *, Dl_info *);
    int dlinfo(void *, int, void *);
}
else version (Darwin)
{
    enum RTLD_LAZY      = 0x00001;
    enum RTLD_NOW       = 0x00002;
    enum RTLD_NOLOAD    = 0x10;
    enum RTLD_NODELETE  = 0x80;
    enum RTLD_GLOBAL    = 0x00100;
    enum RTLD_LOCAL     = 0x00000;
    enum RTLD_FIRST     = 0x100;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(scope const void* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (FreeBSD)
{
    enum RTLD_LAZY      = 1;
    enum RTLD_NOW       = 2;
    enum RTLD_MODEMASK  =  0x3;
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;
    enum RTLD_TRACE     =  0x200;
    enum RTLD_NODELETE  =  0x01000;
    enum RTLD_NOLOAD    =  0x02000;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(const(void)* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (NetBSD)
{
    enum RTLD_LAZY      = 1;
    enum RTLD_NOW       = 2;
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0x200;
    enum RTLD_NODELETE  = 0x01000;         /* Do not remove members. */
    enum RTLD_NOLOAD    = 0x02000;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(const(void)* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (OpenBSD)
{
    enum RTLD_LAZY      = 1;
    enum RTLD_NOW       = 2;
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;
    enum RTLD_TRACE     = 0x200;
    enum RTLD_NODELETE  = 0x400;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(const(void)* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (DragonFlyBSD)
{
    enum RTLD_LAZY      = 1;
    enum RTLD_NOW       = 2;
    enum RTLD_MODEMASK  =  0x3;
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;
    enum RTLD_TRACE     =  0x200;
    enum RTLD_NODELETE  =  0x01000;
    enum RTLD_NOLOAD    =  0x02000;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(const(void)* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (Solaris)
{
    enum RTLD_LAZY      = 1;
    enum RTLD_NOW       = 2;
    enum RTLD_NOLOAD    = 0x00004;
    enum RTLD_DEEPBIND  = 0x00008;
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;
    enum RTLD_PARENT    = 0x00200;
    enum RTLD_GROUP     = 0x00400;
    enum RTLD_WORLD     = 0x00800;
    enum RTLD_NODELETE  = 0x01000;
    enum RTLD_FIRST     = 0x02000;
    enum RTLD_CONFGEN   = 0x10000;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int   dladdr(const(void)* addr, Dl_info* info);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (CRuntime_Bionic)
{
    enum RTLD_LOCAL    = 0;
    enum RTLD_LAZY     = 0x00001;
    enum RTLD_NOLOAD   = 0x00004;
    enum RTLD_NODELETE = 0x01000;

    version (D_LP64)
    {
        enum RTLD_NOW      = 0x00002;
        enum RTLD_GLOBAL   = 0x00100;
    }
    else // NDK: 'LP32 is broken for historical reasons'
    {
        enum RTLD_NOW      = 0;
        enum RTLD_GLOBAL   = 0x00002;
    }

    int          dladdr(const scope void*, Dl_info*);
    int          dlclose(void*);
    const(char)* dlerror();
    void*        dlopen(const scope char*, int);
    void*        dlsym(void*, const scope char*);

    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (CRuntime_Musl)
{
    enum {
        RTLD_LAZY     = 1,
        RTLD_NOW      = 2,
        RTLD_NOLOAD   = 4,
        RTLD_NODELETE = 4096,
        RTLD_GLOBAL   = 256,
        RTLD_LOCAL    = 0,
    }
    int          dlclose(void*);
    const(char)* dlerror();
    void*        dlopen(const scope char*, int);
    void*        dlsym(void*, const scope char*);

    int dladdr(scope const void *addr, Dl_info *info);
    struct Dl_info
    {
        const(char)* dli_fname;
        void*        dli_fbase;
        const(char)* dli_sname;
        void*        dli_saddr;
    }
}
else version (CRuntime_UClibc)
{
    version (X86_Any)
    {
        enum RTLD_LAZY              = 0x0001;
        enum RTLD_NOW               = 0x0002;
        enum RTLD_BINDING_MASK      = 0x3;
        enum RTLD_NOLOAD            = 0x00004;
        enum RTLD_GLOBAL            = 0x00100;
        enum RTLD_LOCAL             = 0;
        enum RTLD_NODELETE          = 0x01000;
    }
    else version (MIPS_Any)
    {
        enum RTLD_LAZY              = 0x0001;
        enum RTLD_NOW               = 0x0002;
        enum RTLD_BINDING_MASK      = 0x3;
        enum RTLD_NOLOAD            = 0x00008;
        enum RTLD_GLOBAL            = 0x0004;
        enum RTLD_LOCAL             = 0;
        enum RTLD_NODELETE          = 0x01000;
    }
    else version (ARM)
    {
        enum RTLD_LAZY              = 0x0001;
        enum RTLD_NOW               = 0x0002;
        enum RTLD_BINDING_MASK      = 0x3;
        enum RTLD_NOLOAD            = 0x00004;
        enum RTLD_GLOBAL            = 0x00100;
        enum RTLD_LOCAL             = 0;
        enum RTLD_NODELETE          = 0x01000;
    }
    else
        static assert(0, "unimplemented");

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(const scope char*, int);
    void* dlsym(void*, const scope char*);
    int dladdr(const scope void*, Dl_info*);

    struct Dl_info
    {
        const(char)* dli_fname;
        void* dli_fbase;
        const(char)* dli_sname;
        void* dli_saddr;
    }
}
