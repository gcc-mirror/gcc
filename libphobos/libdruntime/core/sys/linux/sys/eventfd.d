/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Nemanja Boric
 */
module core.sys.linux.sys.eventfd;

version (linux):
extern (C):
@nogc:
@system:
nothrow:

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

import core.stdc.stdint: uint64_t;

/// Type for the event counter
alias uint64_t eventfd_t;

/* Return file descriptor for generic event channel.  Set initial
   value to count.  */
int eventfd (uint count, int flags);

/* Read event counter and possibly wait for events.  */
int eventfd_read (int fd, eventfd_t* value);

/* Increment event counter.  */
int eventfd_write (int fd, eventfd_t value);

version (CRuntime_UClibc)
{
    version (MIPS_Any)
    {
        enum EFD_SEMAPHORE = 1;
        enum EFD_CLOEXEC = 0x80000; // octal!02000000
        enum EFD_NONBLOCK = 0x80; // octal!00000200
    }
    else version (SPARC_Any)
    {
        enum EFD_SEMAPHORE = 1;
        enum EFD_CLOEXEC = 0x400000;
        enum EFD_NONBLOCK = 0x004000;
    }
    else
    {
        enum EFD_SEMAPHORE = 1;
        enum EFD_CLOEXEC = 0x80000; // octal!02000000
        enum EFD_NONBLOCK = 0x800; // octal!00004000
    }
}
else version (X86_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (HPPA_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x200000; // octal!10000000
    enum EFD_NONBLOCK = 0x10004; // octal!00200004
}
else version (MIPS_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x80; // octal!200
}
else version (PPC_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (ARM_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (RISCV_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (SPARC_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (IBMZ_Any)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else
    static assert(0, "unimplemented");
