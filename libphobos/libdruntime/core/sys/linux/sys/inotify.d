/**
 * D header file for GNU/Linux.
 *
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Gary Willoughby
 */
module core.sys.linux.sys.inotify;

version (linux):
extern (C):
@system:
nothrow:

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

version (X86)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (X86_64)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (MIPS32)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x80; // octal!200
}
else version (MIPS64)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x80; // octal!200
}
else version (PPC)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (PPC64)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (ARM)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (AArch64)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (SPARC64)
{
    enum IN_CLOEXEC = 0x80000; // octal!2000000
    enum IN_NONBLOCK = 0x800; // octal!4000
}
else version (SystemZ)
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
