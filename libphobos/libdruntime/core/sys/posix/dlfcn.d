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

private import core.sys.posix.config;

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
void* dlopen(in char*, int);
void* dlsym(void*, in char*);
*/

version (CRuntime_Glibc)
{
    version (X86_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0x00000;
    }
    else version (HPPA_Any)
    {
        enum RTLD_LAZY      = 0x0001;
        enum RTLD_NOW       = 0x0002;
        enum RTLD_GLOBAL    = 0x0100;
        enum RTLD_LOCAL     = 0;
    }
    else version (MIPS_Any)
    {
        enum RTLD_LAZY      = 0x0001;
        enum RTLD_NOW       = 0x0002;
        enum RTLD_GLOBAL    = 0x0004;
        enum RTLD_LOCAL     = 0;
    }
    else version (PPC_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
    }
    else version (ARM_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
    }
    else version (RISCV_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
    }
    else version (SPARC_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
    }
    else version (IBMZ_Any)
    {
        enum RTLD_LAZY      = 0x00001;
        enum RTLD_NOW       = 0x00002;
        enum RTLD_GLOBAL    = 0x00100;
        enum RTLD_LOCAL     = 0;
    }
    else
        static assert(0, "unimplemented");

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
}
else version (Darwin)
{
    enum RTLD_LAZY      = 0x00001;
    enum RTLD_NOW       = 0x00002;
    enum RTLD_GLOBAL    = 0x00100;
    enum RTLD_LOCAL     = 0x00000;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
    int   dladdr(void* addr, Dl_info* info);

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
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
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
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
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

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
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
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
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
    enum RTLD_GLOBAL    = 0x100;
    enum RTLD_LOCAL     = 0;

    int   dlclose(void*);
    char* dlerror();
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
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
    enum
    {
        RTLD_NOW    = 0,
        RTLD_LAZY   = 1,
        RTLD_LOCAL  = 0,
        RTLD_GLOBAL = 2
    }

    int          dladdr(in void*, Dl_info*);
    int          dlclose(void*);
    const(char)* dlerror();
    void*        dlopen(in char*, int);
    void*        dlsym(void*, in char*);

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
    void*        dlopen(in char*, int);
    void*        dlsym(void*, in char*);
}
else version (CRuntime_UClibc)
{
    version (X86_64)
    {
        enum RTLD_LAZY              = 0x0001;
        enum RTLD_NOW               = 0x0002;
        enum RTLD_BINDING_MASK      = 0x3;
        enum RTLD_NOLOAD            = 0x00004;
        enum RTLD_GLOBAL            = 0x00100;
        enum RTLD_LOCAL             = 0;
        enum RTLD_NODELETE          = 0x01000;
    }
    else version (MIPS32)
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
    void* dlopen(in char*, int);
    void* dlsym(void*, in char*);
}
