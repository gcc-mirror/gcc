
// Interface to <sys/utsname.h>

module core.sys.posix.sys.utsname;

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern(C):
nothrow:
@nogc:
@system:

version (CRuntime_Glibc)
{
    private enum utsNameLength = 65;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        // TODO Deprecate after version_ has been in an official release.
        alias update = version_;
        char[utsNameLength] machine = 0;

        char[utsNameLength] __domainname = 0;
    }

    int uname(utsname* __name);
}
else version (Darwin)
{
    private enum utsNameLength = 256;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        // TODO Deprecate after version_ has been in an official release.
        alias update = version_;
        char[utsNameLength] machine = 0;
    }

    int uname(utsname* __name);
}
else version (FreeBSD)
{
    //private enum SYS_NMLN = 32;       // old FreeBSD 1.1 ABI
    private enum SYS_NMLN = 256;

    struct utsname
    {
        char[SYS_NMLN] sysname = 0;
        char[SYS_NMLN] nodename = 0;
        char[SYS_NMLN] release = 0;
        char[SYS_NMLN] version_ = 0;
        // TODO Deprecate after version_ has been in an official release.
        alias update = version_;
        char[SYS_NMLN] machine = 0;
    }

    int __xuname(int, void*);
    int uname()(utsname* __name) { return __xuname(SYS_NMLN, cast(void*) __name); }
}
else version (NetBSD)
{
    private enum utsNameLength = 256;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        // TODO Deprecate after version_ has been in an official release.
        alias update = version_;
        char[utsNameLength] machine = 0;
    }

    int uname(utsname* __name);
}
else version (OpenBSD)
{
    private enum utsNameLength = 256;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        char[utsNameLength] machine = 0;
    }

    int uname(utsname* __name);
}
else version (DragonFlyBSD)
{
    private enum utsNameLength = 32;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        // TODO Deprecate after version_ has been in an official release.
        alias update = version_;
        char[utsNameLength] machine = 0;
    }

    int uname(utsname* __name);
}
else version (Solaris)
{
    private enum SYS_NMLN = 257;

    struct utsname
    {
        char[SYS_NMLN] sysname = 0;
        char[SYS_NMLN] nodename = 0;
        char[SYS_NMLN] release = 0;
        // The field name is version but version is a keyword in D.
        char[SYS_NMLN] _version = 0;
        char[SYS_NMLN] machine = 0;
    }

    int uname(utsname* __name);
}
else version (CRuntime_Bionic)
{
    private enum SYS_NMLN = 65;

    struct utsname
    {
        char[SYS_NMLN] sysname = 0;
        char[SYS_NMLN] nodename = 0;
        char[SYS_NMLN] release = 0;
        // The field name is version but version is a keyword in D.
        char[SYS_NMLN] _version = 0;
        char[SYS_NMLN] machine = 0;
        char[SYS_NMLN] domainname = 0;
    }

    int uname(utsname*);
}
else version (CRuntime_Musl)
{
    private enum SYS_NMLN = 65;

    struct utsname
    {
        char[SYS_NMLN] sysname = 0;
        char[SYS_NMLN] nodename = 0;
        char[SYS_NMLN] release = 0;
        char[SYS_NMLN] _version = 0;
        char[SYS_NMLN] machine = 0;
        char[SYS_NMLN] domainname = 0;
    }

    int uname(utsname*);
}
else version (CRuntime_UClibc)
{
    private enum utsNameLength = 65;

    struct utsname
    {
        char[utsNameLength] sysname = 0;
        char[utsNameLength] nodename = 0;
        char[utsNameLength] release = 0;
        char[utsNameLength] version_ = 0;
        char[utsNameLength] machine = 0;
        char[utsNameLength] domainname = 0;
    }

    int uname(utsname*);
}
else
{
    static assert(false, "unsupported system");
}
