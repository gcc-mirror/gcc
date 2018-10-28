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

version (X86)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (X86_64)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (MIPS32)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x80; // octal!200
}
else version (MIPS64)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x80; // octal!200
}
else version (PPC)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (PPC64)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (ARM)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (AArch64)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (SPARC64)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else version (SystemZ)
{
    enum EFD_SEMAPHORE = 1;
    enum EFD_CLOEXEC = 0x80000; // octal!2000000
    enum EFD_NONBLOCK = 0x800; // octal!4000
}
else
    static assert(0, "unimplemented");
