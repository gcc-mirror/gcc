/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Gary Willoughby
 */
module core.sys.linux.sys.inotify;

// The BSDs (including macOS) have a kqueue-backed API-compatible inotify
// library in ports. However, inotify is a Linux interface so it lives here.
// All BSD people need this library to use inotify:
//   https://github.com/libinotify-kqueue/libinotify-kqueue
// It is the responsibility of all BSD people to configure the library before
// using this interface.

version (linux)        version = LinuxOrCompatible;
version (Darwin)       version = LinuxOrCompatible;
version (FreeBSD)      version = LinuxOrCompatible;
version (OpenBSD)      version = LinuxOrCompatible;
version (NetBSD)       version = LinuxOrCompatible;
version (DragonFlyBSD) version = LinuxOrCompatible;

version (LinuxOrCompatible):
extern (C):
nothrow:
@nogc:

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

struct inotify_event
{
    int wd;
    uint mask;
    uint cookie;
    uint len;
    char[0] name;

    @disable this(this);
}

enum: uint
{
    IN_ACCESS        = 0x00000000,
    IN_MODIFY        = 0x00000002,
    IN_ATTRIB        = 0x00000004,
    IN_CLOSE_WRITE   = 0x00000008,
    IN_CLOSE_NOWRITE = 0x00000010,
    IN_OPEN          = 0x00000020,
    IN_MOVED_FROM    = 0x00000040,
    IN_MOVED_TO      = 0x00000080,
    IN_CREATE        = 0x00000100,
    IN_DELETE        = 0x00000200,
    IN_DELETE_SELF   = 0x00000400,
    IN_MOVE_SELF     = 0x00000800,
    IN_UNMOUNT       = 0x00002000,
    IN_Q_OVERFLOW    = 0x00004000,
    IN_IGNORED       = 0x00008000,
    IN_CLOSE         = 0x00000018,
    IN_MOVE          = 0x000000C0,
    IN_ONLYDIR       = 0x01000000,
    IN_DONT_FOLLOW   = 0x02000000,
    IN_EXCL_UNLINK   = 0x04000000,
    IN_MASK_ADD      = 0x20000000,
    IN_ISDIR         = 0x40000000,
    IN_ONESHOT       = 0x80000000,
    IN_ALL_EVENTS    = 0x80000FFF,
}

// Old typo, preserved for compatibility
enum IN_UMOUNT = IN_UNMOUNT;

version (X86_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (HPPA_Any)
{
    enum IN_CLOEXEC = 0x200000; // octal!10000000
    enum IN_NONBLOCK = 0x10004; // octal!200004
}
else version (MIPS_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x80; // octal!200
}
else version (PPC_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (ARM_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (RISCV_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (SPARC_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (IBMZ_Any)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else
    static assert(0, "unimplemented");

int inotify_init();
int inotify_init1(int flags);
int inotify_add_watch(int fd, const(char)* name, uint mask);
int inotify_rm_watch(int fd, uint wd);
