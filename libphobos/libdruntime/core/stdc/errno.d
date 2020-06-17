/**
 * D header file for C99.
 *
 * $(C_HEADER_DESCRIPTION pubs.opengroup.org/onlinepubs/009695399/basedefs/_errno.h.html, _errno.h)
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Sean Kelly, Alex Rønne Petersen
 * Source:    https://github.com/dlang/druntime/blob/master/src/core/stdc/errno.d
 * Standards: ISO/IEC 9899:1999 (E)
 */

module core.stdc.errno;

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

@trusted: // Only manipulates errno.
nothrow:
@nogc:

version (CRuntime_DigitalMars)
{
    extern (C)
    {
        ref int _errno();
        alias errno = _errno;
    }
}
else version (CRuntime_Microsoft)
{
    extern (C)
    {
        ref int _errno();
        alias errno = _errno;
    }
}
else version (CRuntime_Glibc)
{
    extern (C)
    {
        ref int __errno_location();
        alias errno = __errno_location;
    }
}
else version (CRuntime_Musl)
{
    extern (C)
    {
        ref int __errno_location();
        alias errno = __errno_location;
    }
}
else version (OpenBSD)
{
    // https://github.com/openbsd/src/blob/master/include/errno.h
    extern (C)
    {
        ref int __errno();
        alias errno = __errno;
    }
}
else version (NetBSD)
{
    // https://github.com/NetBSD/src/blob/trunk/include/errno.h
    extern (C)
    {
        ref int __errno();
        alias errno = __errno;
    }
}
else version (FreeBSD)
{
    extern (C)
    {
        ref int __error();
        alias errno = __error;
    }
}
else version (DragonFlyBSD)
{
    extern (C)
    {
        pragma(mangle, "errno") int __errno;
        ref int __error() {
            return __errno;
        }
        alias errno = __error;
    }
}
else version (CRuntime_Bionic)
{
    extern (C)
    {
        ref int __errno();
        alias errno = __errno;
    }
}
else version (CRuntime_UClibc)
{
    extern (C)
    {
        ref int __errno_location();
        alias errno = __errno_location;
    }
}
else version (Darwin)
{
    extern (C)
    {
        ref int __error();
        alias errno = __error;
    }
}
else version (Solaris)
{
    extern (C)
    {
        ref int ___errno();
        alias errno = ___errno;
    }
}
else version (Haiku)
{
    // https://github.com/haiku/haiku/blob/master/headers/posix/errno.h
    extern (C)
    {
        ref int _errnop();
        alias errno = _errnop;
    }
}
else
{
    ///
    @property int errno() { return getErrno(); }
    ///
    @property int errno(int n) { return setErrno(n); }

    extern (C)
    {
        private int getErrno();      // for internal use
        private int setErrno(int);   // for internal use
    }
}

extern (C):


version (Windows)
{
    enum EPERM              = 1;        /// Operation not permitted
    enum ENOENT             = 2;        /// No such file or directory
    enum ESRCH              = 3;        /// No such process
    enum EINTR              = 4;        /// Interrupted system call
    enum EIO                = 5;        /// I/O error
    enum ENXIO              = 6;        /// No such device or address
    enum E2BIG              = 7;        /// Argument list too long
    enum ENOEXEC            = 8;        /// Exec format error
    enum EBADF              = 9;        /// Bad file number
    enum ECHILD             = 10;       /// No child processes
    enum EAGAIN             = 11;       /// Try again
    enum ENOMEM             = 12;       /// Out of memory
    enum EACCES             = 13;       /// Permission denied
    enum EFAULT             = 14;       /// Bad address
    enum EBUSY              = 16;       /// Device or resource busy
    enum EEXIST             = 17;       /// File exists
    enum EXDEV              = 18;       /// Cross-device link
    enum ENODEV             = 19;       /// No such device
    enum ENOTDIR            = 20;       /// Not a directory
    enum EISDIR             = 21;       /// Is a directory
    enum EINVAL             = 22;       /// Invalid argument
    enum ENFILE             = 23;       /// File table overflow
    enum EMFILE             = 24;       /// Too many open files
    enum ENOTTY             = 25;       /// Not a typewriter
    enum EFBIG              = 27;       /// File too large
    enum ENOSPC             = 28;       /// No space left on device
    enum ESPIPE             = 29;       /// Illegal seek
    enum EROFS              = 30;       /// Read-only file system
    enum EMLINK             = 31;       /// Too many links
    enum EPIPE              = 32;       /// Broken pipe
    enum EDOM               = 33;       /// Math argument out of domain of func
    enum ERANGE             = 34;       /// Math result not representable
    enum EDEADLK            = 36;       /// Resource deadlock would occur
    enum ENAMETOOLONG       = 38;       /// File name too long
    enum ENOLCK             = 39;       /// No record locks available
    enum ENOSYS             = 40;       /// Function not implemented
    enum ENOTEMPTY          = 41;       /// Directory not empty
    enum EILSEQ             = 42;       /// Illegal byte sequence
    enum EDEADLOCK          = EDEADLK;  /// Resource deadlock would occur

    // POSIX compatibility
    // See_Also: https://docs.microsoft.com/en-us/cpp/c-runtime-library/errno-constants
    enum EADDRINUSE         = 100;
    enum EADDRNOTAVAIL      = 101;
    enum EAFNOSUPPORT       = 102;
    enum EALREADY           = 103;
    enum EBADMSG            = 104;
    enum ECANCELED          = 105;
    enum ECONNABORTED       = 106;
    enum ECONNREFUSED       = 107;
    enum ECONNRESET         = 108;
    enum EDESTADDRREQ       = 109;
    enum EHOSTUNREACH       = 110;
    enum EIDRM              = 111;
    enum EINPROGRESS        = 112;
    enum EISCONN            = 113;
    enum ELOOP              = 114;
    enum EMSGSIZE           = 115;
    enum ENETDOWN           = 116;
    enum ENETRESET          = 117;
    enum ENETUNREACH        = 118;
    enum ENOBUFS            = 119;
    enum ENODATA            = 120;
    enum ENOLINK            = 121;
    enum ENOMSG             = 122;
    enum ENOPROTOOPT        = 123;
    enum ENOSR              = 124;
    enum ENOSTR             = 125;
    enum ENOTCONN           = 126;
    enum ENOTRECOVERABLE    = 127;
    enum ENOTSOCK           = 128;
    enum ENOTSUP            = 129;
    enum EOPNOTSUPP         = 130;
    enum EOTHER             = 131;
    enum EOVERFLOW          = 132;
    enum EOWNERDEAD         = 133;
    enum EPROTO             = 134;
    enum EPROTONOSUPPORT    = 135;
    enum EPROTOTYPE         = 136;
    enum ETIME              = 137;
    enum ETIMEDOUT          = 138;
    enum ETXTBSY            = 139;
    enum EWOULDBLOCK        = 140;
}
else version (linux)
{
    enum EPERM              = 1;  ///
    enum ENOENT             = 2;  ///
    enum ESRCH              = 3;  ///
    enum EINTR              = 4;  ///
    enum EIO                = 5;  ///
    enum ENXIO              = 6;  ///
    enum E2BIG              = 7;  ///
    enum ENOEXEC            = 8;  ///
    enum EBADF              = 9;  ///
    enum ECHILD             = 10; ///
    enum EAGAIN             = 11; ///
    enum ENOMEM             = 12; ///
    enum EACCES             = 13; ///
    enum EFAULT             = 14; ///
    enum ENOTBLK            = 15; ///
    enum EBUSY              = 16; ///
    enum EEXIST             = 17; ///
    enum EXDEV              = 18; ///
    enum ENODEV             = 19; ///
    enum ENOTDIR            = 20; ///
    enum EISDIR             = 21; ///
    enum EINVAL             = 22; ///
    enum ENFILE             = 23; ///
    enum EMFILE             = 24; ///
    enum ENOTTY             = 25; ///
    enum ETXTBSY            = 26; ///
    enum EFBIG              = 27; ///
    enum ENOSPC             = 28; ///
    enum ESPIPE             = 29; ///
    enum EROFS              = 30; ///
    enum EMLINK             = 31; ///
    enum EPIPE              = 32; ///
    enum EDOM               = 33; ///
    enum ERANGE             = 34; ///

    version (X86_Any)
    {
        enum EDEADLK            = 35;         ///
        enum ENAMETOOLONG       = 36;         ///
        enum ENOLCK             = 37;         ///
        enum ENOSYS             = 38;         ///
        enum ENOTEMPTY          = 39;         ///
        enum ELOOP              = 40;         ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOMSG             = 42;         ///
        enum EIDRM              = 43;         ///
        enum ECHRNG             = 44;         ///
        enum EL2NSYNC           = 45;         ///
        enum EL3HLT             = 46;         ///
        enum EL3RST             = 47;         ///
        enum ELNRNG             = 48;         ///
        enum EUNATCH            = 49;         ///
        enum ENOCSI             = 50;         ///
        enum EL2HLT             = 51;         ///
        enum EBADE              = 52;         ///
        enum EBADR              = 53;         ///
        enum EXFULL             = 54;         ///
        enum ENOANO             = 55;         ///
        enum EBADRQC            = 56;         ///
        enum EBADSLT            = 57;         ///
        enum EDEADLOCK          = EDEADLK;    ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EMULTIHOP          = 72;         ///
        enum EDOTDOT            = 73;         ///
        enum EBADMSG            = 74;         ///
        enum EOVERFLOW          = 75;         ///
        enum ENOTUNIQ           = 76;         ///
        enum EBADFD             = 77;         ///
        enum EREMCHG            = 78;         ///
        enum ELIBACC            = 79;         ///
        enum ELIBBAD            = 80;         ///
        enum ELIBSCN            = 81;         ///
        enum ELIBMAX            = 82;         ///
        enum ELIBEXEC           = 83;         ///
        enum EILSEQ             = 84;         ///
        enum ERESTART           = 85;         ///
        enum ESTRPIPE           = 86;         ///
        enum EUSERS             = 87;         ///
        enum ENOTSOCK           = 88;         ///
        enum EDESTADDRREQ       = 89;         ///
        enum EMSGSIZE           = 90;         ///
        enum EPROTOTYPE         = 91;         ///
        enum ENOPROTOOPT        = 92;         ///
        enum EPROTONOSUPPORT    = 93;         ///
        enum ESOCKTNOSUPPORT    = 94;         ///
        enum EOPNOTSUPP         = 95;         ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 96;         ///
        enum EAFNOSUPPORT       = 97;         ///
        enum EADDRINUSE         = 98;         ///
        enum EADDRNOTAVAIL      = 99;         ///
        enum ENETDOWN           = 100;        ///
        enum ENETUNREACH        = 101;        ///
        enum ENETRESET          = 102;        ///
        enum ECONNABORTED       = 103;        ///
        enum ECONNRESET         = 104;        ///
        enum ENOBUFS            = 105;        ///
        enum EISCONN            = 106;        ///
        enum ENOTCONN           = 107;        ///
        enum ESHUTDOWN          = 108;        ///
        enum ETOOMANYREFS       = 109;        ///
        enum ETIMEDOUT          = 110;        ///
        enum ECONNREFUSED       = 111;        ///
        enum EHOSTDOWN          = 112;        ///
        enum EHOSTUNREACH       = 113;        ///
        enum EALREADY           = 114;        ///
        enum EINPROGRESS        = 115;        ///
        enum ESTALE             = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EDQUOT             = 122;        ///
        enum ENOMEDIUM          = 123;        ///
        enum EMEDIUMTYPE        = 124;        ///
        enum ECANCELED          = 125;        ///
        enum ENOKEY             = 126;        ///
        enum EKEYEXPIRED        = 127;        ///
        enum EKEYREVOKED        = 128;        ///
        enum EKEYREJECTED       = 129;        ///
        enum EOWNERDEAD         = 130;        ///
        enum ENOTRECOVERABLE    = 131;        ///
        enum ERFKILL            = 132;        ///
        enum EHWPOISON          = 133;        ///
    }
    else version (ARM_Any)
    {
        enum EDEADLK            = 35;         ///
        enum ENAMETOOLONG       = 36;         ///
        enum ENOLCK             = 37;         ///
        enum ENOSYS             = 38;         ///
        enum ENOTEMPTY          = 39;         ///
        enum ELOOP              = 40;         ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOMSG             = 42;         ///
        enum EIDRM              = 43;         ///
        enum ECHRNG             = 44;         ///
        enum EL2NSYNC           = 45;         ///
        enum EL3HLT             = 46;         ///
        enum EL3RST             = 47;         ///
        enum ELNRNG             = 48;         ///
        enum EUNATCH            = 49;         ///
        enum ENOCSI             = 50;         ///
        enum EL2HLT             = 51;         ///
        enum EBADE              = 52;         ///
        enum EBADR              = 53;         ///
        enum EXFULL             = 54;         ///
        enum ENOANO             = 55;         ///
        enum EBADRQC            = 56;         ///
        enum EBADSLT            = 57;         ///
        enum EDEADLOCK          = EDEADLK;    ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EMULTIHOP          = 72;         ///
        enum EDOTDOT            = 73;         ///
        enum EBADMSG            = 74;         ///
        enum EOVERFLOW          = 75;         ///
        enum ENOTUNIQ           = 76;         ///
        enum EBADFD             = 77;         ///
        enum EREMCHG            = 78;         ///
        enum ELIBACC            = 79;         ///
        enum ELIBBAD            = 80;         ///
        enum ELIBSCN            = 81;         ///
        enum ELIBMAX            = 82;         ///
        enum ELIBEXEC           = 83;         ///
        enum EILSEQ             = 84;         ///
        enum ERESTART           = 85;         ///
        enum ESTRPIPE           = 86;         ///
        enum EUSERS             = 87;         ///
        enum ENOTSOCK           = 88;         ///
        enum EDESTADDRREQ       = 89;         ///
        enum EMSGSIZE           = 90;         ///
        enum EPROTOTYPE         = 91;         ///
        enum ENOPROTOOPT        = 92;         ///
        enum EPROTONOSUPPORT    = 93;         ///
        enum ESOCKTNOSUPPORT    = 94;         ///
        enum EOPNOTSUPP         = 95;         ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 96;         ///
        enum EAFNOSUPPORT       = 97;         ///
        enum EADDRINUSE         = 98;         ///
        enum EADDRNOTAVAIL      = 99;         ///
        enum ENETDOWN           = 100;        ///
        enum ENETUNREACH        = 101;        ///
        enum ENETRESET          = 102;        ///
        enum ECONNABORTED       = 103;        ///
        enum ECONNRESET         = 104;        ///
        enum ENOBUFS            = 105;        ///
        enum EISCONN            = 106;        ///
        enum ENOTCONN           = 107;        ///
        enum ESHUTDOWN          = 108;        ///
        enum ETOOMANYREFS       = 109;        ///
        enum ETIMEDOUT          = 110;        ///
        enum ECONNREFUSED       = 111;        ///
        enum EHOSTDOWN          = 112;        ///
        enum EHOSTUNREACH       = 113;        ///
        enum EALREADY           = 114;        ///
        enum EINPROGRESS        = 115;        ///
        enum ESTALE             = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EDQUOT             = 122;        ///
        enum ENOMEDIUM          = 123;        ///
        enum EMEDIUMTYPE        = 124;        ///
        enum ECANCELED          = 125;        ///
        enum ENOKEY             = 126;        ///
        enum EKEYEXPIRED        = 127;        ///
        enum EKEYREVOKED        = 128;        ///
        enum EKEYREJECTED       = 129;        ///
        enum EOWNERDEAD         = 130;        ///
        enum ENOTRECOVERABLE    = 131;        ///
        enum ERFKILL            = 132;        ///
        enum EHWPOISON          = 133;        ///
    }
    else version (HPPA_Any)
    {
        enum ENOMSG             = 35;         ///
        enum EIDRM              = 36;         ///
        enum ECHRNG             = 37;         ///
        enum EL2NSYNC           = 38;         ///
        enum EL3HLT             = 39;         ///
        enum EL3RST             = 40;         ///
        enum ELNRNG             = 41;         ///
        enum EUNATCH            = 42;         ///
        enum ENOCSI             = 43;         ///
        enum EL2HLT             = 44;         ///
        enum EDEADLK            = 45;         ///
        enum EDEADLOCK          = EDEADLK;    ///
        enum ENOLCK             = 46;         ///
        enum EILSEQ             = 47;         ///
        enum ENONET             = 50;         ///
        enum ENODATA            = 51;         ///
        enum ETIME              = 52;         ///
        enum ENOSR              = 53;         ///
        enum ENOSTR             = 54;         ///
        enum ENOPKG             = 55;         ///
        enum ENOLINK            = 57;         ///
        enum EADV               = 58;         ///
        enum ESRMNT             = 59;         ///
        enum ECOMM              = 60;         ///
        enum EPROTO             = 61;         ///
        enum EMULTIHOP          = 64;         ///
        enum EDOTDOT            = 66;         ///
        enum EBADMSG            = 67;         ///
        enum EUSERS             = 68;         ///
        enum EDQUOT             = 69;         ///
        enum ESTALE             = 70;         ///
        enum EREMOTE            = 71;         ///
        enum EOVERFLOW          = 72;         ///
        enum EBADE              = 160;        ///
        enum EBADR              = 161;        ///
        enum EXFULL             = 162;        ///
        enum ENOANO             = 163;        ///
        enum EBADRQC            = 164;        ///
        enum EBADSLT            = 165;        ///
        enum EBFONT             = 166;        ///
        enum ENOTUNIQ           = 167;        ///
        enum EBADFD             = 168;        ///
        enum EREMCHG            = 169;        ///
        enum ELIBACC            = 170;        ///
        enum ELIBBAD            = 171;        ///
        enum ELIBSCN            = 172;        ///
        enum ELIBMAX            = 173;        ///
        enum ELIBEXEC           = 174;        ///
        enum ERESTART           = 175;        ///
        enum ESTRPIPE           = 176;        ///
        enum EUCLEAN            = 177;        ///
        enum ENOTNAM            = 178;        ///
        enum ENAVAIL            = 179;        ///
        enum EISNAM             = 180;        ///
        enum EREMOTEIO          = 181;        ///
        enum ENOMEDIUM          = 182;        ///
        enum EMEDIUMTYPE        = 183;        ///
        enum ENOKEY             = 184;        ///
        enum EKEYEXPIRED        = 185;        ///
        enum EKEYREVOKED        = 186;        ///
        enum EKEYREJECTED       = 187;        ///
        enum ENOSYM             = 215;        ///
        enum ENOTSOCK           = 216;        ///
        enum EDESTADDRREQ       = 217;        ///
        enum EMSGSIZE           = 218;        ///
        enum EPROTOTYPE         = 219;        ///
        enum ENOPROTOOPT        = 220;        ///
        enum EPROTONOSUPPORT    = 221;        ///
        enum ESOCKTNOSUPPORT    = 221;        ///
        enum EOPNOTSUPP         = 223;        ///
        enum EPFNOSUPPORT       = 224;        ///
        enum EAFNOSUPPORT       = 225;        ///
        enum EADDRINUSE         = 226;        ///
        enum EADDRNOTAVAIL      = 227;        ///
        enum ENETDOWN           = 228;        ///
        enum ENETUNREACH        = 229;        ///
        enum ENETRESET          = 230;        ///
        enum ECONNABORTED       = 231;        ///
        enum ECONNRESET         = 232;        ///
        enum ENOBUFS            = 233;        ///
        enum EISCONN            = 234;        ///
        enum ENOTCONN           = 235;        ///
        enum ESHUTDOWN          = 236;        ///
        enum ETOOMANYREFS       = 237;        ///
        enum ETIMEDOUT          = 238;        ///
        enum ECONNREFUSED       = 239;        ///
        enum EREFUSED           = ECONNREFUSED; ///
        enum EREMOTERELEASE     = 240;        ///
        enum EHOSTDOWN          = 241;        ///
        enum EHOSTUNREACH       = 242;        ///
        enum EALREADY           = 244;        ///
        enum EINPROGRESS        = 245;        ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOTEMPTY          = 247;        ///
        enum ENAMETOOLONG       = 248;        ///
        enum ELOOP              = 249;        ///
        enum ENOSYS             = 251;        ///
        enum ECANCELLED         = 253;        ///
        enum ECANCELED          = ECANCELLED;  ///
        enum EOWNERDEAD         = 254;        ///
        enum ENOTRECOVERABLE    = 255;        ///
        enum ERFKILL            = 256;        ///
        enum EHWPOISON          = 257;        ///
    }
    else version (MIPS_Any)
    {
        enum ENOMSG             = 35;         ///
        enum EIDRM              = 36;         ///
        enum ECHRNG             = 37;         ///
        enum EL2NSYNC           = 38;         ///
        enum EL3HLT             = 39;         ///
        enum EL3RST             = 40;         ///
        enum ELNRNG             = 41;         ///
        enum EUNATCH            = 42;         ///
        enum ENOCSI             = 43;         ///
        enum EL2HLT             = 44;         ///
        enum EDEADLK            = 45;         ///
        enum ENOLCK             = 46;         ///
        enum EBADE              = 50;         ///
        enum EBADR              = 51;         ///
        enum EXFULL             = 52;         ///
        enum ENOANO             = 53;         ///
        enum EBADRQC            = 54;         ///
        enum EBADSLT            = 55;         ///
        enum EDEADLOCK          = 56;         ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EDOTDOT            = 73;         ///
        enum EMULTIHOP          = 74;         ///
        enum EBADMSG            = 77;         ///
        enum ENAMETOOLONG       = 78;         ///
        enum EOVERFLOW          = 79;         ///
        enum ENOTUNIQ           = 80;         ///
        enum EBADFD             = 81;         ///
        enum EREMCHG            = 82;         ///
        enum ELIBACC            = 83;         ///
        enum ELIBBAD            = 84;         ///
        enum ELIBSCN            = 85;         ///
        enum ELIBMAX            = 86;         ///
        enum ELIBEXEC           = 87;         ///
        enum EILSEQ             = 88;         ///
        enum ENOSYS             = 89;         ///
        enum ELOOP              = 90;         ///
        enum ERESTART           = 91;         ///
        enum ESTRPIPE           = 92;         ///
        enum ENOTEMPTY          = 93;         ///
        enum EUSERS             = 94;         ///
        enum ENOTSOCK           = 95;         ///
        enum EDESTADDRREQ       = 96;         ///
        enum EMSGSIZE           = 97;         ///
        enum EPROTOTYPE         = 98;         ///
        enum ENOPROTOOPT        = 99;         ///
        enum EPROTONOSUPPORT    = 120;        ///
        enum ESOCKTNOSUPPORT    = 121;        ///
        enum EOPNOTSUPP         = 122;        ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 123;        ///
        enum EAFNOSUPPORT       = 124;        ///
        enum EADDRINUSE         = 125;        ///
        enum EADDRNOTAVAIL      = 126;        ///
        enum ENETDOWN           = 127;        ///
        enum ENETUNREACH        = 128;        ///
        enum ENETRESET          = 129;        ///
        enum ECONNABORTED       = 130;        ///
        enum ECONNRESET         = 131;        ///
        enum ENOBUFS            = 132;        ///
        enum EISCONN            = 133;        ///
        enum ENOTCONN           = 134;        ///
        enum EUCLEAN            = 135;        ///
        enum ENOTNAM            = 137;        ///
        enum ENAVAIL            = 138;        ///
        enum EISNAM             = 139;        ///
        enum EREMOTEIO          = 140;        ///
        enum EINIT              = 141;        ///
        enum EREMDEV            = 142;        ///
        enum ESHUTDOWN          = 143;        ///
        enum ETOOMANYREFS       = 144;        ///
        enum ETIMEDOUT          = 145;        ///
        enum ECONNREFUSED       = 146;        ///
        enum EHOSTDOWN          = 147;        ///
        enum EHOSTUNREACH       = 148;        ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum EALREADY           = 149;        ///
        enum EINPROGRESS        = 150;        ///
        enum ESTALE             = 151;        ///
        enum ECANCELED          = 158;        ///
        enum ENOMEDIUM          = 159;        ///
        enum EMEDIUMTYPE        = 160;        ///
        enum ENOKEY             = 161;        ///
        enum EKEYEXPIRED        = 162;        ///
        enum EKEYREVOKED        = 163;        ///
        enum EKEYREJECTED       = 164;        ///
        enum EOWNERDEAD         = 165;        ///
        enum ENOTRECOVERABLE    = 166;        ///
        enum ERFKILL            = 167;        ///
        enum EHWPOISON          = 168;        ///
        enum EDQUOT             = 1133;       ///
    }
    else version (PPC_Any)
    {
        enum EDEADLK            = 35;         ///
        enum ENAMETOOLONG       = 36;         ///
        enum ENOLCK             = 37;         ///
        enum ENOSYS             = 38;         ///
        enum ENOTEMPTY          = 39;         ///
        enum ELOOP              = 40;         ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOMSG             = 42;         ///
        enum EIDRM              = 43;         ///
        enum ECHRNG             = 44;         ///
        enum EL2NSYNC           = 45;         ///
        enum EL3HLT             = 46;         ///
        enum EL3RST             = 47;         ///
        enum ELNRNG             = 48;         ///
        enum EUNATCH            = 49;         ///
        enum ENOCSI             = 50;         ///
        enum EL2HLT             = 51;         ///
        enum EBADE              = 52;         ///
        enum EBADR              = 53;         ///
        enum EXFULL             = 54;         ///
        enum ENOANO             = 55;         ///
        enum EBADRQC            = 56;         ///
        enum EBADSLT            = 57;         ///
        enum EDEADLOCK          = 58;         ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EMULTIHOP          = 72;         ///
        enum EDOTDOT            = 73;         ///
        enum EBADMSG            = 74;         ///
        enum EOVERFLOW          = 75;         ///
        enum ENOTUNIQ           = 76;         ///
        enum EBADFD             = 77;         ///
        enum EREMCHG            = 78;         ///
        enum ELIBACC            = 79;         ///
        enum ELIBBAD            = 80;         ///
        enum ELIBSCN            = 81;         ///
        enum ELIBMAX            = 82;         ///
        enum ELIBEXEC           = 83;         ///
        enum EILSEQ             = 84;         ///
        enum ERESTART           = 85;         ///
        enum ESTRPIPE           = 86;         ///
        enum EUSERS             = 87;         ///
        enum ENOTSOCK           = 88;         ///
        enum EDESTADDRREQ       = 89;         ///
        enum EMSGSIZE           = 90;         ///
        enum EPROTOTYPE         = 91;         ///
        enum ENOPROTOOPT        = 92;         ///
        enum EPROTONOSUPPORT    = 93;         ///
        enum ESOCKTNOSUPPORT    = 94;         ///
        enum EOPNOTSUPP         = 95;         ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 96;         ///
        enum EAFNOSUPPORT       = 97;         ///
        enum EADDRINUSE         = 98;         ///
        enum EADDRNOTAVAIL      = 99;         ///
        enum ENETDOWN           = 100;        ///
        enum ENETUNREACH        = 101;        ///
        enum ENETRESET          = 102;        ///
        enum ECONNABORTED       = 103;        ///
        enum ECONNRESET         = 104;        ///
        enum ENOBUFS            = 105;        ///
        enum EISCONN            = 106;        ///
        enum ENOTCONN           = 107;        ///
        enum ESHUTDOWN          = 108;        ///
        enum ETOOMANYREFS       = 109;        ///
        enum ETIMEDOUT          = 110;        ///
        enum ECONNREFUSED       = 111;        ///
        enum EHOSTDOWN          = 112;        ///
        enum EHOSTUNREACH       = 113;        ///
        enum EALREADY           = 114;        ///
        enum EINPROGRESS        = 115;        ///
        enum ESTALE             = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EDQUOT             = 122;        ///
        enum ENOMEDIUM          = 123;        ///
        enum EMEDIUMTYPE        = 124;        ///
        enum ECANCELED          = 125;        ///
        enum ENOKEY             = 126;        ///
        enum EKEYEXPIRED        = 127;        ///
        enum EKEYREVOKED        = 128;        ///
        enum EKEYREJECTED       = 129;        ///
        enum EOWNERDEAD         = 130;        ///
        enum ENOTRECOVERABLE    = 131;        ///
        enum ERFKILL            = 132;        ///
        enum EHWPOISON          = 133;        ///
    }
    else version (RISCV_Any)
    {
        enum EDEADLK            = 35;         ///
        enum ENAMETOOLONG       = 36;         ///
        enum ENOLCK             = 37;         ///
        enum ENOSYS             = 38;         ///
        enum ENOTEMPTY          = 39;         ///
        enum ELOOP              = 40;         ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOMSG             = 42;         ///
        enum EIDRM              = 43;         ///
        enum ECHRNG             = 44;         ///
        enum EL2NSYNC           = 45;         ///
        enum EL3HLT             = 46;         ///
        enum EL3RST             = 47;         ///
        enum ELNRNG             = 48;         ///
        enum EUNATCH            = 49;         ///
        enum ENOCSI             = 50;         ///
        enum EL2HLT             = 51;         ///
        enum EBADE              = 52;         ///
        enum EBADR              = 53;         ///
        enum EXFULL             = 54;         ///
        enum ENOANO             = 55;         ///
        enum EBADRQC            = 56;         ///
        enum EBADSLT            = 57;         ///
        enum EDEADLOCK          = EDEADLK;    ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EMULTIHOP          = 72;         ///
        enum EDOTDOT            = 73;         ///
        enum EBADMSG            = 74;         ///
        enum EOVERFLOW          = 75;         ///
        enum ENOTUNIQ           = 76;         ///
        enum EBADFD             = 77;         ///
        enum EREMCHG            = 78;         ///
        enum ELIBACC            = 79;         ///
        enum ELIBBAD            = 80;         ///
        enum ELIBSCN            = 81;         ///
        enum ELIBMAX            = 82;         ///
        enum ELIBEXEC           = 83;         ///
        enum EILSEQ             = 84;         ///
        enum ERESTART           = 85;         ///
        enum ESTRPIPE           = 86;         ///
        enum EUSERS             = 87;         ///
        enum ENOTSOCK           = 88;         ///
        enum EDESTADDRREQ       = 89;         ///
        enum EMSGSIZE           = 90;         ///
        enum EPROTOTYPE         = 91;         ///
        enum ENOPROTOOPT        = 92;         ///
        enum EPROTONOSUPPORT    = 93;         ///
        enum ESOCKTNOSUPPORT    = 94;         ///
        enum EOPNOTSUPP         = 95;         ///
        enum EPFNOSUPPORT       = 96;         ///
        enum EAFNOSUPPORT       = 97;         ///
        enum EADDRINUSE         = 98;         ///
        enum EADDRNOTAVAIL      = 99;         ///
        enum ENETDOWN           = 100;        ///
        enum ENETUNREACH        = 101;        ///
        enum ENETRESET          = 102;        ///
        enum ECONNABORTED       = 103;        ///
        enum ECONNRESET         = 104;        ///
        enum ENOBUFS            = 105;        ///
        enum EISCONN            = 106;        ///
        enum ENOTCONN           = 107;        ///
        enum ESHUTDOWN          = 108;        ///
        enum ETOOMANYREFS       = 109;        ///
        enum ETIMEDOUT          = 110;        ///
        enum ECONNREFUSED       = 111;        ///
        enum EHOSTDOWN          = 112;        ///
        enum EHOSTUNREACH       = 113;        ///
        enum EALREADY           = 114;        ///
        enum EINPROGRESS        = 115;        ///
        enum ESTALE             = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EDQUOT             = 122;        ///
        enum ENOMEDIUM          = 123;        ///
        enum EMEDIUMTYPE        = 124;        ///
        enum ECANCELED          = 125;        ///
        enum ENOKEY             = 126;        ///
        enum EKEYEXPIRED        = 127;        ///
        enum EKEYREVOKED        = 128;        ///
        enum EKEYREJECTED       = 129;        ///
        enum EOWNERDEAD         = 130;        ///
        enum ENOTRECOVERABLE    = 131;        ///
        enum ERFKILL            = 132;        ///
        enum EHWPOISON          = 133;        ///
    }
    else version (SPARC_Any)
    {
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum EINPROGRESS        = 36;         ///
        enum EALREADY           = 37;         ///
        enum ENOTSOCK           = 38;         ///
        enum EDESTADDRREQ       = 39;         ///
        enum EMSGSIZE           = 40;         ///
        enum EPROTOTYPE         = 41;         ///
        enum ENOPROTOOPT        = 42;         ///
        enum EPROTONOSUPPORT    = 43;         ///
        enum ESOCKTNOSUPPORT    = 44;         ///
        enum EOPNOTSUPP         = 45;         ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 46;         ///
        enum EAFNOSUPPORT       = 47;         ///
        enum EADDRINUSE         = 48;         ///
        enum EADDRNOTAVAIL      = 49;         ///
        enum ENETDOWN           = 50;         ///
        enum ENETUNREACH        = 51;         ///
        enum ENETRESET          = 52;         ///
        enum ECONNABORTED       = 53;         ///
        enum ECONNRESET         = 54;         ///
        enum ENOBUFS            = 55;         ///
        enum EISCONN            = 56;         ///
        enum ENOTCONN           = 57;         ///
        enum ESHUTDOWN          = 58;         ///
        enum ETOOMANYREFS       = 59;         ///
        enum ETIMEDOUT          = 60;         ///
        enum ECONNREFUSED       = 61;         ///
        enum ELOOP              = 62;         ///
        enum ENAMETOOLONG       = 63;         ///
        enum EHOSTDOWN          = 64;         ///
        enum EHOSTUNREACH       = 65;         ///
        enum ENOTEMPTY          = 66;         ///
        enum EPROCLIM           = 67;         ///
        enum EUSERS             = 68;         ///
        enum EDQUOT             = 69;         ///
        enum ESTALE             = 70;         ///
        enum EREMOTE            = 71;         ///
        enum ENOSTR             = 72;         ///
        enum ETIME              = 73;         ///
        enum ENOSR              = 74;         ///
        enum ENOMSG             = 75;         ///
        enum EBADMSG            = 76;         ///
        enum EIDRM              = 77;         ///
        enum EDEADLK            = 78;         ///
        enum ENOLCK             = 79;         ///
        enum ENONET             = 80;         ///
        enum ERREMOTE           = 81;         ///
        enum ENOLINK            = 82;         ///
        enum EADV               = 83;         ///
        enum ESRMNT             = 84;         ///
        enum ECOMM              = 85;         ///
        enum EPROTO             = 86;         ///
        enum EMULTIHOP          = 87;         ///
        enum EDOTDOT            = 88;         ///
        enum EREMCHG            = 89;         ///
        enum ENOSYS             = 90;         ///
        enum ESTRPIPE           = 91;         ///
        enum EOVERFLOW          = 92;         ///
        enum EBADFD             = 93;         ///
        enum ECHRNG             = 94;         ///
        enum EL2NSYNC           = 95;         ///
        enum EL3HLT             = 96;         ///
        enum EL3RST             = 97;         ///
        enum ELNRNG             = 98;         ///
        enum EUNATCH            = 99;         ///
        enum ENOCSI             = 100;        ///
        enum EL2HLT             = 101;        ///
        enum EBADE              = 102;        ///
        enum EBADR              = 103;        ///
        enum EXFULL             = 104;        ///
        enum ENOANO             = 105;        ///
        enum EBADRQC            = 106;        ///
        enum EBADSLT            = 107;        ///
        enum EDEADLOCK          = 108;        ///
        enum EBFONT             = 109;        ///
        enum ELIBEXEC           = 110;        ///
        enum ENODATA            = 111;        ///
        enum ELIBBAD            = 112;        ///
        enum ENOPKG             = 113;        ///
        enum ELIBACC            = 114;        ///
        enum ENOTUNIQ           = 115;        ///
        enum ERESTART           = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EILSEQ             = 122;        ///
        enum ELIBMAX            = 123;        ///
        enum ELIBSCN            = 124;        ///
        enum ENOMEDIUM          = 125;        ///
        enum EMEDIUMTYPE        = 126;        ///
        enum ECANCELED          = 127;        ///
        enum ENOKEY             = 128;        ///
        enum EKEYEXPIRED        = 129;        ///
        enum EKEYREVOKED        = 130;        ///
        enum EKEYREJECTED       = 131;        ///
        enum EOWNERDEAD         = 132;        ///
        enum ENOTRECOVERABLE    = 133;        ///
        enum ERFKILL            = 134;        ///
        enum EHWPOISON          = 135;        ///
    }
    else version (IBMZ_Any)
    {
        enum EDEADLK            = 35;         ///
        enum ENAMETOOLONG       = 36;         ///
        enum ENOLCK             = 37;         ///
        enum ENOSYS             = 38;         ///
        enum ENOTEMPTY          = 39;         ///
        enum ELOOP              = 40;         ///
        enum EWOULDBLOCK        = EAGAIN;     ///
        enum ENOMSG             = 42;         ///
        enum EIDRM              = 43;         ///
        enum ECHRNG             = 44;         ///
        enum EL2NSYNC           = 45;         ///
        enum EL3HLT             = 46;         ///
        enum EL3RST             = 47;         ///
        enum ELNRNG             = 48;         ///
        enum EUNATCH            = 49;         ///
        enum ENOCSI             = 50;         ///
        enum EL2HLT             = 51;         ///
        enum EBADE              = 52;         ///
        enum EBADR              = 53;         ///
        enum EXFULL             = 54;         ///
        enum ENOANO             = 55;         ///
        enum EBADRQC            = 56;         ///
        enum EBADSLT            = 57;         ///
        enum EDEADLOCK          = EDEADLK;    ///
        enum EBFONT             = 59;         ///
        enum ENOSTR             = 60;         ///
        enum ENODATA            = 61;         ///
        enum ETIME              = 62;         ///
        enum ENOSR              = 63;         ///
        enum ENONET             = 64;         ///
        enum ENOPKG             = 65;         ///
        enum EREMOTE            = 66;         ///
        enum ENOLINK            = 67;         ///
        enum EADV               = 68;         ///
        enum ESRMNT             = 69;         ///
        enum ECOMM              = 70;         ///
        enum EPROTO             = 71;         ///
        enum EMULTIHOP          = 72;         ///
        enum EDOTDOT            = 73;         ///
        enum EBADMSG            = 74;         ///
        enum EOVERFLOW          = 75;         ///
        enum ENOTUNIQ           = 76;         ///
        enum EBADFD             = 77;         ///
        enum EREMCHG            = 78;         ///
        enum ELIBACC            = 79;         ///
        enum ELIBBAD            = 80;         ///
        enum ELIBSCN            = 81;         ///
        enum ELIBMAX            = 82;         ///
        enum ELIBEXEC           = 83;         ///
        enum EILSEQ             = 84;         ///
        enum ERESTART           = 85;         ///
        enum ESTRPIPE           = 86;         ///
        enum EUSERS             = 87;         ///
        enum ENOTSOCK           = 88;         ///
        enum EDESTADDRREQ       = 89;         ///
        enum EMSGSIZE           = 90;         ///
        enum EPROTOTYPE         = 91;         ///
        enum ENOPROTOOPT        = 92;         ///
        enum EPROTONOSUPPORT    = 93;         ///
        enum ESOCKTNOSUPPORT    = 94;         ///
        enum EOPNOTSUPP         = 95;         ///
        enum ENOTSUP            = EOPNOTSUPP; ///
        enum EPFNOSUPPORT       = 96;         ///
        enum EAFNOSUPPORT       = 97;         ///
        enum EADDRINUSE         = 98;         ///
        enum EADDRNOTAVAIL      = 99;         ///
        enum ENETDOWN           = 100;        ///
        enum ENETUNREACH        = 101;        ///
        enum ENETRESET          = 102;        ///
        enum ECONNABORTED       = 103;        ///
        enum ECONNRESET         = 104;        ///
        enum ENOBUFS            = 105;        ///
        enum EISCONN            = 106;        ///
        enum ENOTCONN           = 107;        ///
        enum ESHUTDOWN          = 108;        ///
        enum ETOOMANYREFS       = 109;        ///
        enum ETIMEDOUT          = 110;        ///
        enum ECONNREFUSED       = 111;        ///
        enum EHOSTDOWN          = 112;        ///
        enum EHOSTUNREACH       = 113;        ///
        enum EALREADY           = 114;        ///
        enum EINPROGRESS        = 115;        ///
        enum ESTALE             = 116;        ///
        enum EUCLEAN            = 117;        ///
        enum ENOTNAM            = 118;        ///
        enum ENAVAIL            = 119;        ///
        enum EISNAM             = 120;        ///
        enum EREMOTEIO          = 121;        ///
        enum EDQUOT             = 122;        ///
        enum ENOMEDIUM          = 123;        ///
        enum EMEDIUMTYPE        = 124;        ///
        enum ECANCELED          = 125;        ///
        enum ENOKEY             = 126;        ///
        enum EKEYEXPIRED        = 127;        ///
        enum EKEYREVOKED        = 128;        ///
        enum EKEYREJECTED       = 129;        ///
        enum EOWNERDEAD         = 130;        ///
        enum ENOTRECOVERABLE    = 131;        ///
        enum ERFKILL            = 132;        ///
        enum EHWPOISON          = 133;        ///
    }
    else
    {
        static assert(false, "Architecture not supported.");
    }
}
else version (Darwin)
{
    enum EPERM              = 1;        /// Operation not permitted
    enum ENOENT             = 2;        /// No such file or directory
    enum ESRCH              = 3;        /// No such process
    enum EINTR              = 4;        /// Interrupted system call
    enum EIO                = 5;        /// Input/output error
    enum ENXIO              = 6;        /// Device not configured
    enum E2BIG              = 7;        /// Argument list too long
    enum ENOEXEC            = 8;        /// Exec format error
    enum EBADF              = 9;        /// Bad file descriptor
    enum ECHILD             = 10;       /// No child processes
    enum EDEADLK            = 11;       /// Resource deadlock avoided
    enum ENOMEM             = 12;       /// Cannot allocate memory
    enum EACCES             = 13;       /// Permission denied
    enum EFAULT             = 14;       /// Bad address
    enum EBUSY              = 16;       /// Device busy
    enum EEXIST             = 17;       /// File exists
    enum EXDEV              = 18;       /// Cross-device link
    enum ENODEV             = 19;       /// Operation not supported by device
    enum ENOTDIR            = 20;       /// Not a directory
    enum EISDIR             = 21;       /// Is a directory
    enum EINVAL             = 22;       /// Invalid argument
    enum ENFILE             = 23;       /// Too many open files in system
    enum EMFILE             = 24;       /// Too many open files
    enum ENOTTY             = 25;       /// Inappropriate ioctl for device
    enum ETXTBSY            = 26;       /// Text file busy
    enum EFBIG              = 27;       /// File too large
    enum ENOSPC             = 28;       /// No space left on device
    enum ESPIPE             = 29;       /// Illegal seek
    enum EROFS              = 30;       /// Read-only file system
    enum EMLINK             = 31;       /// Too many links
    enum EPIPE              = 32;       /// Broken pipe
    enum EDOM               = 33;       /// Numerical argument out of domain
    enum ERANGE             = 34;       /// Result too large
    enum EAGAIN             = 35;       /// Resource temporarily unavailable
    enum EWOULDBLOCK        = EAGAIN;   /// Operation would block
    enum EINPROGRESS        = 36;       /// Operation now in progress
    enum EALREADY           = 37;       /// Operation already in progress
    enum ENOTSOCK           = 38;       /// Socket operation on non-socket
    enum EDESTADDRREQ       = 39;       /// Destination address required
    enum EMSGSIZE           = 40;       /// Message too long
    enum EPROTOTYPE         = 41;       /// Protocol wrong type for socket
    enum ENOPROTOOPT        = 42;       /// Protocol not available
    enum EPROTONOSUPPORT    = 43;       /// Protocol not supported
    enum ENOTSUP            = 45;       /// Operation not supported
    enum EOPNOTSUPP         = ENOTSUP;  /// Operation not supported on socket
    enum EAFNOSUPPORT       = 47;       /// Address family not supported by protocol family
    enum EADDRINUSE         = 48;       /// Address already in use
    enum EADDRNOTAVAIL      = 49;       /// Can't assign requested address
    enum ENETDOWN           = 50;       /// Network is down
    enum ENETUNREACH        = 51;       /// Network is unreachable
    enum ENETRESET          = 52;       /// Network dropped connection on reset
    enum ECONNABORTED       = 53;       /// Software caused connection abort
    enum ECONNRESET         = 54;       /// Connection reset by peer
    enum ENOBUFS            = 55;       /// No buffer space available
    enum EISCONN            = 56;       /// Socket is already connected
    enum ENOTCONN           = 57;       /// Socket is not connected
    enum ETIMEDOUT          = 60;       /// Operation timed out
    enum ECONNREFUSED       = 61;       /// Connection refused
    enum ELOOP              = 62;       /// Too many levels of symbolic links
    enum ENAMETOOLONG       = 63;       /// File name too long
    enum EHOSTUNREACH       = 65;       /// No route to host
    enum ENOTEMPTY          = 66;       /// Directory not empty
    enum EDQUOT             = 69;       /// Disc quota exceeded
    enum ESTALE             = 70;       /// Stale NFS file handle
    enum ENOLCK             = 77;       /// No locks available
    enum ENOSYS             = 78;       /// Function not implemented
    enum EOVERFLOW          = 84;       /// Value too large to be stored in data type
    enum ECANCELED          = 89;       /// Operation canceled
    enum EIDRM              = 90;       /// Identifier removed
    enum ENOMSG             = 91;       /// No message of desired type
    enum EILSEQ             = 92;       /// Illegal byte sequence
    enum EBADMSG            = 94;       /// Bad message
    enum EMULTIHOP          = 95;       /// Reserved
    enum ENODATA            = 96;       /// No message available on STREAM
    enum ENOLINK            = 97;       /// Reserved
    enum ENOSR              = 98;       /// No STREAM resources
    enum ENOSTR             = 99;       /// Not a STREAM
    enum EPROTO             = 100;      /// Protocol error
    enum ETIME              = 101;      /// STREAM ioctl timeout
    enum ELAST              = 101;      /// Must be equal largest errno
}
else version (FreeBSD)
{
    enum EPERM              = 1;        /// Operation not permitted
    enum ENOENT             = 2;        /// No such file or directory
    enum ESRCH              = 3;        /// No such process
    enum EINTR              = 4;        /// Interrupted system call
    enum EIO                = 5;        /// Input/output error
    enum ENXIO              = 6;        /// Device not configured
    enum E2BIG              = 7;        /// Argument list too long
    enum ENOEXEC            = 8;        /// Exec format error
    enum EBADF              = 9;        /// Bad file descriptor
    enum ECHILD             = 10;       /// No child processes
    enum EDEADLK            = 11;       /// Resource deadlock avoided
    enum ENOMEM             = 12;       /// Cannot allocate memory
    enum EACCES             = 13;       /// Permission denied
    enum EFAULT             = 14;       /// Bad address
    enum ENOTBLK            = 15;       /// Block device required
    enum EBUSY              = 16;       /// Device busy
    enum EEXIST             = 17;       /// File exists
    enum EXDEV              = 18;       /// Cross-device link
    enum ENODEV             = 19;       /// Operation not supported by device
    enum ENOTDIR            = 20;       /// Not a directory
    enum EISDIR             = 21;       /// Is a directory
    enum EINVAL             = 22;       /// Invalid argument
    enum ENFILE             = 23;       /// Too many open files in system
    enum EMFILE             = 24;       /// Too many open files
    enum ENOTTY             = 25;       /// Inappropriate ioctl for device
    enum ETXTBSY            = 26;       /// Text file busy
    enum EFBIG              = 27;       /// File too large
    enum ENOSPC             = 28;       /// No space left on device
    enum ESPIPE             = 29;       /// Illegal seek
    enum EROFS              = 30;       /// Read-only file system
    enum EMLINK             = 31;       /// Too many links
    enum EPIPE              = 32;       /// Broken pipe
    enum EDOM               = 33;       /// Numerical argument out of domain
    enum ERANGE             = 34;       /// Result too large
    enum EAGAIN             = 35;       /// Resource temporarily unavailable
    enum EWOULDBLOCK        = EAGAIN;   /// Operation would block
    enum EINPROGRESS        = 36;       /// Operation now in progress
    enum EALREADY           = 37;       /// Operation already in progress
    enum ENOTSOCK           = 38;       /// Socket operation on non-socket
    enum EDESTADDRREQ       = 39;       /// Destination address required
    enum EMSGSIZE           = 40;       /// Message too long
    enum EPROTOTYPE         = 41;       /// Protocol wrong type for socket
    enum ENOPROTOOPT        = 42;       /// Protocol not available
    enum EPROTONOSUPPORT    = 43;       /// Protocol not supported
    enum ENOTSUP            = 45;       /// Operation not supported
    enum EOPNOTSUPP         = ENOTSUP;  /// Operation not supported on socket
    enum EAFNOSUPPORT       = 47;       /// Address family not supported by protocol family
    enum EADDRINUSE         = 48;       /// Address already in use
    enum EADDRNOTAVAIL      = 49;       /// Can't assign requested address
    enum ENETDOWN           = 50;       /// Network is down
    enum ENETUNREACH        = 51;       /// Network is unreachable
    enum ENETRESET          = 52;       /// Network dropped connection on reset
    enum ECONNABORTED       = 53;       /// Software caused connection abort
    enum ECONNRESET         = 54;       /// Connection reset by peer
    enum ENOBUFS            = 55;       /// No buffer space available
    enum EISCONN            = 56;       /// Socket is already connected
    enum ENOTCONN           = 57;       /// Socket is not connected
    enum ESHUTDOWN          = 58;       /// Can't send after socket shutdown
    enum ETOOMANYREFS       = 59;       /// Too many refrences; can't splice
    enum ETIMEDOUT          = 60;       /// Operation timed out
    enum ECONNREFUSED       = 61;       /// Connection refused
    enum ELOOP              = 62;       /// Too many levels of symbolic links
    enum ENAMETOOLONG       = 63;       /// File name too long
    enum EHOSTUNREACH       = 65;       /// No route to host
    enum ENOTEMPTY          = 66;       /// Directory not empty
    enum EPROCLIM           = 67;       /// Too many processes
    enum EUSERS             = 68;       /// Too many users
    enum EDQUOT             = 69;       /// Disc quota exceeded
    enum ESTALE             = 70;       /// Stale NFS file handle
    enum EREMOTE            = 71;       /// Too many levels of remote in path
    enum EBADRPC            = 72;       /// RPC struct is bad
    enum ERPCMISMATCH       = 73;       /// RPC version wrong
    enum EPROGUNAVAIL       = 74;       /// RPC prog. not avail
    enum EPROGMISMATCH      = 75;       /// Program version wrong
    enum EPROCUNAVAIL       = 76;       /// Bad procedure for program
    enum ENOLCK             = 77;       /// No locks available
    enum ENOSYS             = 78;       /// Function not implemented
    enum EFTYPE             = 79;       /// Inappropriate file type or format
    enum EAUTH              = 80;       /// Authentication error
    enum ENEEDAUTH          = 81;       /// Need authenticator
    enum EIDRM              = 82;       /// Itendifier removed
    enum ENOMSG             = 83;       /// No message of desired type
    enum EOVERFLOW          = 84;       /// Value too large to be stored in data type
    enum ECANCELED          = 85;       /// Operation canceled
    enum EILSEQ             = 86;       /// Illegal byte sequence
    enum ENOATTR            = 87;       /// Attribute not found
    enum EDOOFUS            = 88;       /// Programming error
    enum EBADMSG            = 89;       /// Bad message
    enum EMULTIHOP          = 90;       /// Multihop attempted
    enum ENOLINK            = 91;       /// Link has been severed
    enum EPROTO             = 92;       /// Protocol error
    enum ELAST              = 92;       /// Must be equal largest errno
}
else version (NetBSD)
{
    // http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/sys/sys/errno.h
    enum EPERM           = 1;
    enum ENOENT          = 2;
    enum ESRCH           = 3;
    enum EINTR           = 4;
    enum EIO             = 5;
    enum ENXIO           = 6;
    enum E2BIG           = 7;
    enum ENOEXEC         = 8;
    enum EBADF           = 9;
    enum ECHILD          = 10;
    enum EDEADLK         = 11;
    ///
    enum ENOMEM          = 12;
    enum EACCES          = 13;
    enum EFAULT          = 14;
    enum ENOTBLK         = 15;
    enum EBUSY           = 16;
    enum EEXIST          = 17;
    enum EXDEV           = 18;
    enum ENODEV          = 19;
    enum ENOTDIR         = 20;
    enum EISDIR          = 21;
    enum EINVAL          = 22;
    enum ENFILE          = 23;
    enum EMFILE          = 24;
    enum ENOTTY          = 25;
    enum ETXTBSY         = 26;
    enum EFBIG           = 27;
    enum ENOSPC          = 28;
    enum ESPIPE          = 29;
    enum EROFS           = 30;
    enum EMLINK          = 31;
    enum EPIPE           = 32;
    ///
    enum EDOM            = 33;
    enum ERANGE          = 34;

    ///
    enum EAGAIN          = 35;
    enum EWOULDBLOCK     = EAGAIN;
    enum EINPROGRESS     = 36;
    enum EALREADY        = 37;

    ///
    enum ENOTSOCK        = 38;
    enum EDESTADDRREQ    = 39;
    enum EMSGSIZE        = 40;
    enum EPROTOTYPE      = 41;
    enum ENOPROTOOPT     = 42;
    enum EPROTONOSUPPORT = 43;
    enum ESOCKTNOSUPPORT = 44;
    enum EOPNOTSUPP      = 45;
    enum EPFNOSUPPORT    = 46;
    enum EAFNOSUPPORT    = 47;
    enum EADDRINUSE      = 48;
    enum EADDRNOTAVAIL   = 49;

    ///
    enum ENETDOWN        = 50;
    enum ENETUNREACH     = 51;
    enum ENETRESET       = 52;
    enum ECONNABORTED    = 53;
    enum ECONNRESET      = 54;
    enum ENOBUFS         = 55;
    enum EISCONN         = 56;
    enum ENOTCONN        = 57;
    enum ESHUTDOWN       = 58;
    enum ETOOMANYREFS    = 59;
    enum ETIMEDOUT       = 60;
    enum ECONNREFUSED    = 61;
    enum ELOOP           = 62;
    enum ENAMETOOLONG    = 63;

    ///
    enum EHOSTDOWN       = 64;
    enum EHOSTUNREACH    = 65;
    enum ENOTEMPTY       = 66;

    ///
    enum EPROCLIM        = 67;
    enum EUSERS          = 68;
    enum EDQUOT          = 69;

    ///
    enum ESTALE          = 70;
    enum EREMOTE         = 71;
    enum EBADRPC         = 72;
    enum ERPCMISMATCH    = 73;
    enum EPROGUNAVAIL    = 74;
    enum EPROGMISMATCH   = 75;
    enum EPROCUNAVAIL    = 76;

    enum ENOLCK          = 77;
    enum ENOSYS          = 78;

    enum EFTYPE          = 79;
    enum EAUTH           = 80;
    enum ENEEDAUTH       = 81;

    ///
    enum EIDRM           = 82;
    enum ENOMSG          = 83;
    enum EOVERFLOW       = 84;
    ///
    enum EILSEQ          = 85;

    ///
    enum ENOTSUP         = 86;

    ///
    enum ECANCELED       = 87;

    ///
    enum EBADMSG         = 88;

    ///
    enum ENODATA         = 89;
    enum ENOSR           = 90;
    enum ENOSTR          = 91;
    enum ETIME           = 92;

    ///
    enum ENOATTR         = 93;

    ///
    enum EMULTIHOP       = 94;
    enum ENOLINK         = 95;
    enum EPROTO          = 96;
}
else version (OpenBSD)
{
    enum EPERM              = 1;        /// Operation not permitted
    enum ENOENT             = 2;        /// No such file or directory
    enum ESRCH              = 3;        /// No such process
    enum EINTR              = 4;        /// Interrupted system call
    enum EIO                = 5;        /// Input/output error
    enum ENXIO              = 6;        /// Device not configured
    enum E2BIG              = 7;        /// Argument list too long
    enum ENOEXEC            = 8;        /// Exec format error
    enum EBADF              = 9;        /// Bad file descriptor
    enum ECHILD             = 10;       /// No child processes
    enum EDEADLK            = 11;       /// Resource deadlock avoided
    enum ENOMEM             = 12;       /// Cannot allocate memory
    enum EACCES             = 13;       /// Permission denied
    enum EFAULT             = 14;       /// Bad address
    enum ENOTBLK            = 15;       /// Block device required
    enum EBUSY              = 16;       /// Device busy
    enum EEXIST             = 17;       /// File exists
    enum EXDEV              = 18;       /// Cross-device link
    enum ENODEV             = 19;       /// Operation not supported by device
    enum ENOTDIR            = 20;       /// Not a directory
    enum EISDIR             = 21;       /// Is a directory
    enum EINVAL             = 22;       /// Invalid argument
    enum ENFILE             = 23;       /// Too many open files in system
    enum EMFILE             = 24;       /// Too many open files
    enum ENOTTY             = 25;       /// Inappropriate ioctl for device
    enum ETXTBSY            = 26;       /// Text file busy
    enum EFBIG              = 27;       /// File too large
    enum ENOSPC             = 28;       /// No space left on device
    enum ESPIPE             = 29;       /// Illegal seek
    enum EROFS              = 30;       /// Read-only file system
    enum EMLINK             = 31;       /// Too many links
    enum EPIPE              = 32;       /// Broken pipe
    enum EDOM               = 33;       /// Numerical argument out of domain
    enum ERANGE             = 34;       /// Result too large
    enum EAGAIN             = 35;       /// Resource temporarily unavailable
    enum EWOULDBLOCK        = EAGAIN;   /// Operation would block
    enum EINPROGRESS        = 36;       /// Operation now in progress
    enum EALREADY           = 37;       /// Operation already in progress
    enum ENOTSOCK           = 38;       /// Socket operation on non-socket
    enum EDESTADDRREQ       = 39;       /// Destination address required
    enum EMSGSIZE           = 40;       /// Message too long
    enum EPROTOTYPE         = 41;       /// Protocol wrong type for socket
    enum ENOPROTOOPT        = 42;       /// Protocol not available
    enum EPROTONOSUPPORT    = 43;       /// Protocol not supported
    enum ESOCKTNOSUPPORT    = 44;       /// Socket type not supported
    enum EOPNOTSUPP         = 45;       /// Operation not supported
    enum EPFNOSUPPORT       = 46;       /// Protocol family not supported
    enum EAFNOSUPPORT       = 47;       /// Address family not supported by protocol family
    enum EADDRINUSE         = 48;       /// Address already in use
    enum EADDRNOTAVAIL      = 49;       /// Can't assign requested address
    enum ENETDOWN           = 50;       /// Network is down
    enum ENETUNREACH        = 51;       /// Network is unreachable
    enum ENETRESET          = 52;       /// Network dropped connection on reset
    enum ECONNABORTED       = 53;       /// Software caused connection abort
    enum ECONNRESET         = 54;       /// Connection reset by peer
    enum ENOBUFS            = 55;       /// No buffer space available
    enum EISCONN            = 56;       /// Socket is already connected
    enum ENOTCONN           = 57;       /// Socket is not connected
    enum ESHUTDOWN          = 58;       /// Can't send after socket shutdown
    enum ETOOMANYREFS       = 59;       /// Too many references: can't splice
    enum ETIMEDOUT          = 60;       /// Operation timed out
    enum ECONNREFUSED       = 61;       /// Connection refused
    enum ELOOP              = 62;       /// Too many levels of symbolic links
    enum ENAMETOOLONG       = 63;       /// File name too long
    enum EHOSTDOWN          = 64;       /// Host is down
    enum EHOSTUNREACH       = 65;       /// No route to host
    enum ENOTEMPTY          = 66;       /// Directory not empty
    enum EPROCLIM           = 67;       /// Too many processes
    enum EUSERS             = 68;       /// Too many users
    enum EDQUOT             = 69;       /// Disk quota exceeded
    enum ESTALE             = 70;       /// Stale NFS file handle
    enum EREMOTE            = 71;       /// Too many levels of remote in path
    enum EBADRPC            = 72;       /// RPC struct is bad
    enum ERPCMISMATCH       = 73;       /// RPC version wrong
    enum EPROGUNAVAIL       = 74;       /// RPC program not available
    enum EPROGMISMATCH      = 75;       /// Program version wrong
    enum EPROCUNAVAIL       = 76;       /// Bad procedure for program
    enum ENOLCK             = 77;       /// No locks available
    enum ENOSYS             = 78;       /// Function not implemented
    enum EFTYPE             = 79;       /// Inappropriate file type or format
    enum EAUTH              = 80;       /// Authentication error
    enum ENEEDAUTH          = 81;       /// Need authenticator
    enum EIPSEC             = 82;       /// IPsec processing failure
    enum ENOATTR            = 83;       /// Attribute not found
    enum EILSEQ             = 84;       /// Illegal byte sequence
    enum ENOMEDIUM          = 85;       /// No medium found
    enum EMEDIUMTYPE        = 86;       /// Wrong medium type
    enum EOVERFLOW          = 87;       /// Value too large to be stored in data type
    enum ECANCELED          = 88;       /// Operation canceled
    enum EIDRM              = 89;       /// Identifier removed
    enum ENOMSG             = 90;       /// No message of desired type
    enum ENOTSUP            = 91;       /// Not supported
    enum ELAST              = 91;       /// Must be equal largest errno
}
else version (DragonFlyBSD)
{
    enum EPERM              = 1;
    enum ENOENT             = 2;
    enum ESRCH              = 3;
    enum EINTR              = 4;
    enum EIO                = 5;
    enum ENXIO              = 6;
    enum E2BIG              = 7;
    enum ENOEXEC            = 8;
    enum EBADF              = 9;
    enum ECHILD             = 10;
    enum EDEADLK            = 11;
    enum ENOMEM             = 12;
    enum EACCES             = 13;
    enum EFAULT             = 14;
    enum ENOTBLK            = 15;
    enum EBUSY              = 16;
    enum EEXIST             = 17;
    enum EXDEV              = 18;
    enum ENODEV             = 19;
    enum ENOTDIR            = 20;
    enum EISDIR             = 21;
    enum EINVAL             = 22;
    enum ENFILE             = 23;
    enum EMFILE             = 24;
    enum ENOTTY             = 25;
    enum ETXTBSY            = 26;
    enum EFBIG              = 27;
    enum ENOSPC             = 28;
    enum ESPIPE             = 29;
    enum EROFS              = 30;
    enum EMLINK             = 31;
    enum EPIPE              = 32;
    enum EDOM               = 33;
    enum ERANGE             = 34;
    enum EAGAIN             = 35;
    enum EWOULDBLOCK        = EAGAIN;
    enum EINPROGRESS        = 36;
    enum EALREADY           = 37;
    enum ENOTSOCK           = 38;
    enum EDESTADDRREQ       = 39;
    enum EMSGSIZE           = 40;
    enum EPROTOTYPE         = 41;
    enum ENOPROTOOPT        = 42;
    enum EPROTONOSUPPORT    = 43;
    enum ENOTSUP            = 45;
    enum EOPNOTSUPP         = ENOTSUP;
    enum EPFNOSUPPORT       = 46;
    enum EAFNOSUPPORT       = 47;
    enum EADDRINUSE         = 48;
    enum EADDRNOTAVAIL      = 49;
    enum ENETDOWN           = 50;
    enum ENETUNREACH        = 51;
    enum ENETRESET          = 52;
    enum ECONNABORTED       = 53;
    enum ECONNRESET         = 54;
    enum ENOBUFS            = 55;
    enum EISCONN            = 56;
    enum ENOTCONN           = 57;
    enum ESHUTDOWN          = 58;
    enum ETOOMANYREFS       = 59;
    enum ETIMEDOUT          = 60;
    enum ECONNREFUSED       = 61;
    enum ELOOP              = 62;
    enum ENAMETOOLONG       = 63;
    enum EHOSTUNREACH       = 65;
    enum ENOTEMPTY          = 66;
    enum EPROCLIM           = 67;
    enum EUSERS             = 68;
    enum EDQUOT             = 69;
    enum ESTALE             = 70;
    enum EREMOTE            = 71;
    enum EBADRPC            = 72;
    enum ERPCMISMATCH       = 73;
    enum EPROGUNAVAIL       = 74;
    enum EPROGMISMATCH      = 75;
    enum EPROCUNAVAIL       = 76;
    enum ENOLCK             = 77;
    enum ENOSYS             = 78;
    enum EFTYPE             = 79;
    enum EAUTH              = 80;
    enum ENEEDAUTH          = 81;
    enum EIDRM              = 82;
    enum ENOMSG             = 83;
    enum EOVERFLOW          = 84;
    enum ECANCELED          = 85;
    enum EILSEQ             = 86;
    enum ENOATTR            = 87;
    enum EDOOFUS            = 88;
    enum EBADMSG            = 89;
    enum EMULTIHOP          = 90;
    enum ENOLINK            = 91;
    enum EPROTO             = 92;
    enum ENOMEDIUM          = 93;
    enum EUNUSED94          = 94;
    enum EUNUSED95          = 95;
    enum EUNUSED96          = 96;
    enum EUNUSED97          = 97;
    enum EUNUSED98          = 98;
    enum EASYNC             = 99;
    enum ELAST              = 99;
}
else version (Solaris)
{
    enum EPERM =  1       /** Not super-user                       */;
    enum ENOENT = 2       /** No such file or directory            */;
    enum ESRCH =  3       /** No such process                      */;
    enum EINTR =  4       /** interrupted system call              */;
    enum EIO =    5       /** I/O error                            */;
    enum ENXIO =  6       /** No such device or address            */;
    enum E2BIG =  7       /** Arg list too long                    */;
    enum ENOEXEC = 8       /** Exec format error                    */;
    enum EBADF =  9       /** Bad file number                      */;
    enum ECHILD = 10      /** No children                          */;
    enum EAGAIN = 11      /** Resource temporarily unavailable     */;
    enum ENOMEM = 12      /** Not enough core                      */;
    enum EACCES = 13      /** Permission denied                    */;
    enum EFAULT = 14      /** Bad address                          */;
    enum ENOTBLK = 15      /** Block device required                */;
    enum EBUSY =  16      /** Mount device busy                    */;
    enum EEXIST = 17      /** File exists                          */;
    enum EXDEV =  18      /** Cross-device link                    */;
    enum ENODEV = 19      /** No such device                       */;
    enum ENOTDIR = 20      /** Not a directory                      */;
    enum EISDIR = 21      /** Is a directory                       */;
    enum EINVAL = 22      /** Invalid argument                     */;
    enum ENFILE = 23      /** File table overflow                  */;
    enum EMFILE = 24      /** Too many open files                  */;
    enum ENOTTY = 25      /** Inappropriate ioctl for device       */;
    enum ETXTBSY = 26      /** Text file busy                       */;
    enum EFBIG =  27      /** File too large                       */;
    enum ENOSPC = 28      /** No space left on device              */;
    enum ESPIPE = 29      /** Illegal seek                         */;
    enum EROFS =  30      /** Read only file system                */;
    enum EMLINK = 31      /** Too many links                       */;
    enum EPIPE =  32      /** Broken pipe                          */;
    enum EDOM =   33      /** Math arg out of domain of func       */;
    enum ERANGE = 34      /** Math result not representable        */;
    enum ENOMSG = 35      /** No message of desired type           */;
    enum EIDRM =  36      /** Identifier removed                   */;
    enum ECHRNG = 37      /** Channel number out of range          */;
    enum EL2NSYNC = 38     /** Level 2 not synchronized             */;
    enum EL3HLT = 39      /** Level 3 halted                       */;
    enum EL3RST = 40      /** Level 3 reset                        */;
    enum ELNRNG = 41      /** Link number out of range             */;
    enum EUNATCH = 42      /** Protocol driver not attached         */;
    enum ENOCSI = 43      /** No CSI structure available           */;
    enum EL2HLT = 44      /** Level 2 halted                       */;
    enum EDEADLK = 45      /** Deadlock condition.                  */;
    enum ENOLCK = 46      /** No record locks available.           */;
    enum ECANCELED = 47    /** Operation canceled                   */;
    enum ENOTSUP = 48      /** Operation not supported              */;
    enum EDQUOT = 49      /** Disc quota exceeded                  */;
    enum EBADE =  50      /** invalid exchange                     */;
    enum EBADR =  51      /** invalid request descriptor           */;
    enum EXFULL = 52      /** exchange full                        */;
    enum ENOANO = 53      /** no anode                             */;
    enum EBADRQC = 54      /** invalid request code                 */;
    enum EBADSLT = 55      /** invalid slot                         */;
    enum EDEADLOCK = 56    /** file locking deadlock error          */;
    enum EBFONT = 57      /** bad font file fmt                    */;
    enum EOWNERDEAD =     58      /** process died with the lock */;
    enum ENOTRECOVERABLE = 59      /** lock is not recoverable */;
    enum ENOSTR = 60      /** Device not a stream                  */;
    enum ENODATA = 61      /** no data (for no delay io)            */;
    enum ETIME =  62      /** timer expired                        */;
    enum ENOSR =  63      /** out of streams resources             */;
    enum ENONET = 64      /** Machine is not on the network        */;
    enum ENOPKG = 65      /** Package not installed                */;
    enum EREMOTE = 66      /** The object is remote                 */;
    enum ENOLINK = 67      /** the link has been severed            */;
    enum EADV =   68      /** advertise error                      */;
    enum ESRMNT = 69      /** srmount error                        */;
    enum ECOMM =  70      /** Communication error on send          */;
    enum EPROTO = 71      /** Protocol error                       */;
    enum ELOCKUNMAPPED =  72      /** locked lock was unmapped */;
    enum ENOTACTIVE = 73   /** Facility is not active               */;
    enum EMULTIHOP = 74    /** multihop attempted                   */;
    enum EBADMSG = 77      /** trying to read unreadable message    */;
    enum ENAMETOOLONG = 78 /** path name is too long                */;
    enum EOVERFLOW = 79    /** value too large to be stored in data type */;
    enum ENOTUNIQ = 80     /** given log. name not unique           */;
    enum EBADFD =  81      /** f.d. invalid for this operation      */;
    enum EREMCHG = 82      /** Remote address changed               */;
    enum ELIBACC = 83      /** Can't access a needed shared lib.    */;
    enum ELIBBAD = 84      /** Accessing a corrupted shared lib.    */;
    enum ELIBSCN = 85      /** .lib section in a.out corrupted.     */;
    enum ELIBMAX = 86      /** Attempting to link in too many libs. */;
    enum ELIBEXEC = 87     /** Attempting to exec a shared library. */;
    enum EILSEQ = 88      /** Illegal byte sequence.               */;
    enum ENOSYS = 89      /** Unsupported file system operation    */;
    enum ELOOP =  90      /** Symbolic link loop                   */;
    enum ERESTART = 91     /** Restartable system call              */;
    enum ESTRPIPE = 92     /** if pipe/FIFO, don't sleep in stream head */;
    enum ENOTEMPTY = 93    /** directory not empty                  */;
    enum EUSERS = 94      /** Too many users (for UFS)             */;
    enum ENOTSOCK =       95      /** Socket operation on non-socket */;
    enum EDESTADDRREQ =   96      /** Destination address required */;
    enum EMSGSIZE =       97      /** Message too long */;
    enum EPROTOTYPE =     98      /** Protocol wrong type for socket */;
    enum ENOPROTOOPT =    99      /** Protocol not available */;
    enum EPROTONOSUPPORT = 120     /** Protocol not supported */;
    enum ESOCKTNOSUPPORT = 121     /** Socket type not supported */;
    enum EOPNOTSUPP =     122     /** Operation not supported on socket */;
    enum EPFNOSUPPORT =   123     /** Protocol family not supported */;
    enum EAFNOSUPPORT =   124     /** Address family not supported by the protocol family */;
    enum EADDRINUSE =     125     /** Address already in use */;
    enum EADDRNOTAVAIL =   126     /** Can't assign requested address */;
    enum ENETDOWN =       127     /** Network is down */;
    enum ENETUNREACH =    128     /** Network is unreachable */;
    enum ENETRESET =      129     /** Network dropped connection because of reset */;
    enum ECONNABORTED =   130     /** Software caused connection abort */;
    enum ECONNRESET =     131     /** Connection reset by peer */;
    enum ENOBUFS =        132     /** No buffer space available */;
    enum EISCONN =        133     /** Socket is already connected */;
    enum ENOTCONN =       134     /** Socket is not connected */;
    enum ESHUTDOWN =      143     /** Can't send after socket shutdown */;
    enum ETOOMANYREFS =   144     /** Too many references: can't splice */;
    enum ETIMEDOUT =      145     /** Connection timed out */;
    enum ECONNREFUSED =   146     /** Connection refused */;
    enum EHOSTDOWN =      147     /** Host is down */;
    enum EHOSTUNREACH =   148     /** No route to host */;
    enum EWOULDBLOCK =    EAGAIN;      /** Resource temporarily unavailable     */;
    enum EALREADY =       149     /** operation already in progress */;
    enum EINPROGRESS =    150     /** operation now in progress */;
    enum ESTALE =         151     /** Stale NFS file handle */;
}
else version (Haiku)
{
    // https://github.com/haiku/haiku/blob/master/headers/os/support/Errors.h
    // https://github.com/haiku/haiku/blob/master/headers/build/os/support/Errors.h
    import core.stdc.limits : INT_MIN;
    enum B_GENERAL_ERROR_BASE        = INT_MIN;
    enum B_OS_ERROR_BASE             = (B_GENERAL_ERROR_BASE + 0x1000);
    enum B_APP_ERROR_BASE            = (B_GENERAL_ERROR_BASE + 0x2000);
    enum B_INTERFACE_ERROR_BASE      = (B_GENERAL_ERROR_BASE + 0x3000);
    enum B_MEDIA_ERROR_BASE          = (B_GENERAL_ERROR_BASE + 0x4000);
                                            /* - 0x41ff */
    enum B_TRANSLATION_ERROR_BASE    = (B_GENERAL_ERROR_BASE + 0x4800);
                                            /* - 0x48ff */
    enum B_MIDI_ERROR_BASE           = (B_GENERAL_ERROR_BASE + 0x5000);
    enum B_STORAGE_ERROR_BASE        = (B_GENERAL_ERROR_BASE + 0x6000);
    enum B_POSIX_ERROR_BASE          = (B_GENERAL_ERROR_BASE + 0x7000);
    enum B_MAIL_ERROR_BASE           = (B_GENERAL_ERROR_BASE + 0x8000);
    enum B_PRINT_ERROR_BASE          = (B_GENERAL_ERROR_BASE + 0x9000);
    enum B_DEVICE_ERROR_BASE         = (B_GENERAL_ERROR_BASE + 0xa000);

    /* General Errors */
    enum B_NO_MEMORY                 = (B_GENERAL_ERROR_BASE + 0);
    enum B_IO_ERROR                  = (B_GENERAL_ERROR_BASE + 1);
    enum B_PERMISSION_DENIED         = (B_GENERAL_ERROR_BASE + 2);
    enum B_BAD_INDEX                 = (B_GENERAL_ERROR_BASE + 3);
    enum B_BAD_TYPE                  = (B_GENERAL_ERROR_BASE + 4);
    enum B_BAD_VALUE                 = (B_GENERAL_ERROR_BASE + 5);
    enum B_MISMATCHED_VALUES         = (B_GENERAL_ERROR_BASE + 6);
    enum B_NAME_NOT_FOUND            = (B_GENERAL_ERROR_BASE + 7);
    enum B_NAME_IN_USE               = (B_GENERAL_ERROR_BASE + 8);
    enum B_TIMED_OUT                 = (B_GENERAL_ERROR_BASE + 9);
    enum B_INTERRUPTED               = (B_GENERAL_ERROR_BASE + 10);
    enum B_WOULD_BLOCK               = (B_GENERAL_ERROR_BASE + 11);
    enum B_CANCELED                  = (B_GENERAL_ERROR_BASE + 12);
    enum B_NO_INIT                   = (B_GENERAL_ERROR_BASE + 13);
    enum B_NOT_INITIALIZED           = (B_GENERAL_ERROR_BASE + 13);
    enum B_BUSY                      = (B_GENERAL_ERROR_BASE + 14);
    enum B_NOT_ALLOWED               = (B_GENERAL_ERROR_BASE + 15);
    enum B_BAD_DATA                  = (B_GENERAL_ERROR_BASE + 16);
    enum B_DONT_DO_THAT              = (B_GENERAL_ERROR_BASE + 17);

    enum B_ERROR                     = (-1);
    enum B_OK                        = (int(0));
    enum B_NO_ERROR                  = (int(0));

    /* Kernel Kit Errors */
    enum B_BAD_SEM_ID                = (B_OS_ERROR_BASE + 0);
    enum B_NO_MORE_SEMS              = (B_OS_ERROR_BASE + 1);

    enum B_BAD_THREAD_ID             = (B_OS_ERROR_BASE + 0x100);
    enum B_NO_MORE_THREADS           = (B_OS_ERROR_BASE + 0x101);
    enum B_BAD_THREAD_STATE          = (B_OS_ERROR_BASE + 0x102);
    enum B_BAD_TEAM_ID               = (B_OS_ERROR_BASE + 0x103);
    enum B_NO_MORE_TEAMS             = (B_OS_ERROR_BASE + 0x104);

    enum B_BAD_PORT_ID               = (B_OS_ERROR_BASE + 0x200);
    enum B_NO_MORE_PORTS             = (B_OS_ERROR_BASE + 0x201);

    enum B_BAD_IMAGE_ID              = (B_OS_ERROR_BASE + 0x300);
    enum B_BAD_ADDRESS               = (B_OS_ERROR_BASE + 0x301);
    enum B_NOT_AN_EXECUTABLE         = (B_OS_ERROR_BASE + 0x302);
    enum B_MISSING_LIBRARY           = (B_OS_ERROR_BASE + 0x303);
    enum B_MISSING_SYMBOL            = (B_OS_ERROR_BASE + 0x304);
    enum B_UNKNOWN_EXECUTABLE        = (B_OS_ERROR_BASE + 0x305);
    enum B_LEGACY_EXECUTABLE         = (B_OS_ERROR_BASE + 0x306);

    enum B_DEBUGGER_ALREADY_INSTALLED    = (B_OS_ERROR_BASE + 0x400);

    /* Application Kit Errors */
    enum B_BAD_REPLY                         = (B_APP_ERROR_BASE + 0);
    enum B_DUPLICATE_REPLY                   = (B_APP_ERROR_BASE + 1);
    enum B_MESSAGE_TO_SELF                   = (B_APP_ERROR_BASE + 2);
    enum B_BAD_HANDLER                       = (B_APP_ERROR_BASE + 3);
    enum B_ALREADY_RUNNING                   = (B_APP_ERROR_BASE + 4);
    enum B_LAUNCH_FAILED                     = (B_APP_ERROR_BASE + 5);
    enum B_AMBIGUOUS_APP_LAUNCH              = (B_APP_ERROR_BASE + 6);
    enum B_UNKNOWN_MIME_TYPE                 = (B_APP_ERROR_BASE + 7);
    enum B_BAD_SCRIPT_SYNTAX                 = (B_APP_ERROR_BASE + 8);
    enum B_LAUNCH_FAILED_NO_RESOLVE_LINK     = (B_APP_ERROR_BASE + 9);
    enum B_LAUNCH_FAILED_EXECUTABLE          = (B_APP_ERROR_BASE + 10);
    enum B_LAUNCH_FAILED_APP_NOT_FOUND       = (B_APP_ERROR_BASE + 11);
    enum B_LAUNCH_FAILED_APP_IN_TRASH        = (B_APP_ERROR_BASE + 12);
    enum B_LAUNCH_FAILED_NO_PREFERRED_APP    = (B_APP_ERROR_BASE + 13);
    enum B_LAUNCH_FAILED_FILES_APP_NOT_FOUND = (B_APP_ERROR_BASE + 14);
    enum B_BAD_MIME_SNIFFER_RULE             = (B_APP_ERROR_BASE + 15);
    enum B_NOT_A_MESSAGE                     = (B_APP_ERROR_BASE + 16);
    enum B_SHUTDOWN_CANCELLED                = (B_APP_ERROR_BASE + 17);
    enum B_SHUTTING_DOWN                     = (B_APP_ERROR_BASE + 18);

    /* Storage Kit/File System Errors */
    enum B_FILE_ERROR                        = (B_STORAGE_ERROR_BASE + 0);
    enum B_FILE_NOT_FOUND                    = (B_STORAGE_ERROR_BASE + 1);
                /* deprecated: use B_ENTRY_NOT_FOUND instead */
    enum B_FILE_EXISTS                       = (B_STORAGE_ERROR_BASE + 2);
    enum B_ENTRY_NOT_FOUND                   = (B_STORAGE_ERROR_BASE + 3);
    enum B_NAME_TOO_LONG                     = (B_STORAGE_ERROR_BASE + 4);
    enum B_NOT_A_DIRECTORY                   = (B_STORAGE_ERROR_BASE + 5);
    enum B_DIRECTORY_NOT_EMPTY               = (B_STORAGE_ERROR_BASE + 6);
    enum B_DEVICE_FULL                       = (B_STORAGE_ERROR_BASE + 7);
    enum B_READ_ONLY_DEVICE                  = (B_STORAGE_ERROR_BASE + 8);
    enum B_IS_A_DIRECTORY                    = (B_STORAGE_ERROR_BASE + 9);
    enum B_NO_MORE_FDS                       = (B_STORAGE_ERROR_BASE + 10);
    enum B_CROSS_DEVICE_LINK                 = (B_STORAGE_ERROR_BASE + 11);
    enum B_LINK_LIMIT                        = (B_STORAGE_ERROR_BASE + 12);
    enum B_BUSTED_PIPE                       = (B_STORAGE_ERROR_BASE + 13);
    enum B_UNSUPPORTED                       = (B_STORAGE_ERROR_BASE + 14);
    enum B_PARTITION_TOO_SMALL               = (B_STORAGE_ERROR_BASE + 15);
    enum B_PARTIAL_READ                      = (B_STORAGE_ERROR_BASE + 16);
    enum B_PARTIAL_WRITE                     = (B_STORAGE_ERROR_BASE + 17);

    /* POSIX Errors */
    enum B_USE_POSITIVE_POSIX_ERRORS = false;

    static if (B_USE_POSITIVE_POSIX_ERRORS)
    {
        enum B_TO_POSIX_ERROR(int code) = -code;
    }
    else
    {
        enum B_TO_POSIX_ERROR(int code) = code;
    }
    alias B_FROM_POSIX_ERROR = B_TO_POSIX_ERROR;

    enum B_POSIX_ENOMEM  = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 0);
    enum E2BIG           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 1);
    enum ECHILD          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 2);
    enum EDEADLK         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 3);
    enum EFBIG           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 4);
    enum EMLINK          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 5);
    enum ENFILE          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 6);
    enum ENODEV          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 7);
    enum ENOLCK          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 8);
    enum ENOSYS          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 9);
    enum ENOTTY          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 10);
    enum ENXIO           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 11);
    enum ESPIPE          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 12);
    enum ESRCH           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 13);
    enum EFPOS           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 14);
    enum ESIGPARM        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 15);
    enum EDOM            = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 16);
    enum ERANGE          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 17);
    enum EPROTOTYPE      = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 18);
    enum EPROTONOSUPPORT = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 19);
    enum EPFNOSUPPORT    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 20);
    enum EAFNOSUPPORT    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 21);
    enum EADDRINUSE      = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 22);
    enum EADDRNOTAVAIL   = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 23);
    enum ENETDOWN        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 24);
    enum ENETUNREACH     = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 25);
    enum ENETRESET       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 26);
    enum ECONNABORTED    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 27);
    enum ECONNRESET      = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 28);
    enum EISCONN         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 29);
    enum ENOTCONN        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 30);
    enum ESHUTDOWN       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 31);
    enum ECONNREFUSED    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 32);
    enum EHOSTUNREACH    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 33);
    enum ENOPROTOOPT     = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 34);
    enum ENOBUFS         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 35);
    enum EINPROGRESS     = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 36);
    enum EALREADY        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 37);
    enum EILSEQ          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 38);
    enum ENOMSG          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 39);
    enum ESTALE          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 40);
    enum EOVERFLOW       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 41);
    enum EMSGSIZE        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 42);
    enum EOPNOTSUPP      = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 43);
    enum ENOTSOCK        = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 44);
    enum EHOSTDOWN       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 45);
    enum EBADMSG         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 46);
    enum ECANCELED       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 47);
    enum EDESTADDRREQ    = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 48);
    enum EDQUOT          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 49);
    enum EIDRM           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 50);
    enum EMULTIHOP       = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 51);
    enum ENODATA         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 52);
    enum ENOLINK         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 53);
    enum ENOSR           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 54);
    enum ENOSTR          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 55);
    enum ENOTSUP         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 56);
    enum EPROTO          = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 57);
    enum ETIME           = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 58);
    enum ETXTBSY         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 59);
    enum ENOATTR         = B_TO_POSIX_ERROR!(B_POSIX_ERROR_BASE + 60);

    /* B_NO_MEMORY (0x80000000) can't be negated, so it needs special handling */
    static if (B_USE_POSITIVE_POSIX_ERRORS)
        enum ENOMEM = B_POSIX_ENOMEM;
    else
        enum ENOMEM = B_NO_MEMORY;

    /* POSIX errors that can be mapped to BeOS error codes */
    enum EACCES          = B_TO_POSIX_ERROR!(B_PERMISSION_DENIED);
    enum EINTR           = B_TO_POSIX_ERROR!(B_INTERRUPTED);
    enum EIO             = B_TO_POSIX_ERROR!(B_IO_ERROR);
    enum EBUSY           = B_TO_POSIX_ERROR!(B_BUSY);
    enum EFAULT          = B_TO_POSIX_ERROR!(B_BAD_ADDRESS);
    enum ETIMEDOUT       = B_TO_POSIX_ERROR!(B_TIMED_OUT);
    enum EAGAIN          = B_TO_POSIX_ERROR!(B_WOULD_BLOCK) /* SysV compatibility */;
    enum EWOULDBLOCK     = B_TO_POSIX_ERROR!(B_WOULD_BLOCK) /* BSD compatibility */;
    enum EBADF           = B_TO_POSIX_ERROR!(B_FILE_ERROR);
    enum EEXIST          = B_TO_POSIX_ERROR!(B_FILE_EXISTS);
    enum EINVAL          = B_TO_POSIX_ERROR!(B_BAD_VALUE);
    enum ENAMETOOLONG    = B_TO_POSIX_ERROR!(B_NAME_TOO_LONG);
    enum ENOENT          = B_TO_POSIX_ERROR!(B_ENTRY_NOT_FOUND);
    enum EPERM           = B_TO_POSIX_ERROR!(B_NOT_ALLOWED);
    enum ENOTDIR         = B_TO_POSIX_ERROR!(B_NOT_A_DIRECTORY);
    enum EISDIR          = B_TO_POSIX_ERROR!(B_IS_A_DIRECTORY);
    enum ENOTEMPTY       = B_TO_POSIX_ERROR!(B_DIRECTORY_NOT_EMPTY);
    enum ENOSPC          = B_TO_POSIX_ERROR!(B_DEVICE_FULL);
    enum EROFS           = B_TO_POSIX_ERROR!(B_READ_ONLY_DEVICE);
    enum EMFILE          = B_TO_POSIX_ERROR!(B_NO_MORE_FDS);
    enum EXDEV           = B_TO_POSIX_ERROR!(B_CROSS_DEVICE_LINK);
    enum ELOOP           = B_TO_POSIX_ERROR!(B_LINK_LIMIT);
    enum ENOEXEC         = B_TO_POSIX_ERROR!(B_NOT_AN_EXECUTABLE);
    enum EPIPE           = B_TO_POSIX_ERROR!(B_BUSTED_PIPE);

    /* new error codes that can be mapped to POSIX errors */
    enum B_BUFFER_OVERFLOW          =  B_FROM_POSIX_ERROR!(EOVERFLOW);
    enum B_TOO_MANY_ARGS            =  B_FROM_POSIX_ERROR!(E2BIG);
    enum B_FILE_TOO_LARGE           =  B_FROM_POSIX_ERROR!(EFBIG);
    enum B_RESULT_NOT_REPRESENTABLE =  B_FROM_POSIX_ERROR!(ERANGE);
    enum B_DEVICE_NOT_FOUND         =  B_FROM_POSIX_ERROR!(ENODEV);
    enum B_NOT_SUPPORTED            =  B_FROM_POSIX_ERROR!(EOPNOTSUPP);

    /* Media Kit Errors */
    enum B_STREAM_NOT_FOUND              = (B_MEDIA_ERROR_BASE + 0);
    enum B_SERVER_NOT_FOUND              = (B_MEDIA_ERROR_BASE + 1);
    enum B_RESOURCE_NOT_FOUND            = (B_MEDIA_ERROR_BASE + 2);
    enum B_RESOURCE_UNAVAILABLE          = (B_MEDIA_ERROR_BASE + 3);
    enum B_BAD_SUBSCRIBER                = (B_MEDIA_ERROR_BASE + 4);
    enum B_SUBSCRIBER_NOT_ENTERED        = (B_MEDIA_ERROR_BASE + 5);
    enum B_BUFFER_NOT_AVAILABLE          = (B_MEDIA_ERROR_BASE + 6);
    enum B_LAST_BUFFER_ERROR             = (B_MEDIA_ERROR_BASE + 7);

    enum B_MEDIA_SYSTEM_FAILURE          = (B_MEDIA_ERROR_BASE + 100);
    enum B_MEDIA_BAD_NODE                = (B_MEDIA_ERROR_BASE + 101);
    enum B_MEDIA_NODE_BUSY               = (B_MEDIA_ERROR_BASE + 102);
    enum B_MEDIA_BAD_FORMAT              = (B_MEDIA_ERROR_BASE + 103);
    enum B_MEDIA_BAD_BUFFER              = (B_MEDIA_ERROR_BASE + 104);
    enum B_MEDIA_TOO_MANY_NODES          = (B_MEDIA_ERROR_BASE + 105);
    enum B_MEDIA_TOO_MANY_BUFFERS        = (B_MEDIA_ERROR_BASE + 106);
    enum B_MEDIA_NODE_ALREADY_EXISTS     = (B_MEDIA_ERROR_BASE + 107);
    enum B_MEDIA_BUFFER_ALREADY_EXISTS   = (B_MEDIA_ERROR_BASE + 108);
    enum B_MEDIA_CANNOT_SEEK             = (B_MEDIA_ERROR_BASE + 109);
    enum B_MEDIA_CANNOT_CHANGE_RUN_MODE  = (B_MEDIA_ERROR_BASE + 110);
    enum B_MEDIA_APP_ALREADY_REGISTERED  = (B_MEDIA_ERROR_BASE + 111);
    enum B_MEDIA_APP_NOT_REGISTERED      = (B_MEDIA_ERROR_BASE + 112);
    enum B_MEDIA_CANNOT_RECLAIM_BUFFERS  = (B_MEDIA_ERROR_BASE + 113);
    enum B_MEDIA_BUFFERS_NOT_RECLAIMED   = (B_MEDIA_ERROR_BASE + 114);
    enum B_MEDIA_TIME_SOURCE_STOPPED     = (B_MEDIA_ERROR_BASE + 115);
    enum B_MEDIA_TIME_SOURCE_BUSY        = (B_MEDIA_ERROR_BASE + 116);
    enum B_MEDIA_BAD_SOURCE              = (B_MEDIA_ERROR_BASE + 117);
    enum B_MEDIA_BAD_DESTINATION         = (B_MEDIA_ERROR_BASE + 118);
    enum B_MEDIA_ALREADY_CONNECTED       = (B_MEDIA_ERROR_BASE + 119);
    enum B_MEDIA_NOT_CONNECTED           = (B_MEDIA_ERROR_BASE + 120);
    enum B_MEDIA_BAD_CLIP_FORMAT         = (B_MEDIA_ERROR_BASE + 121);
    enum B_MEDIA_ADDON_FAILED            = (B_MEDIA_ERROR_BASE + 122);
    enum B_MEDIA_ADDON_DISABLED          = (B_MEDIA_ERROR_BASE + 123);
    enum B_MEDIA_CHANGE_IN_PROGRESS      = (B_MEDIA_ERROR_BASE + 124);
    enum B_MEDIA_STALE_CHANGE_COUNT      = (B_MEDIA_ERROR_BASE + 125);
    enum B_MEDIA_ADDON_RESTRICTED        = (B_MEDIA_ERROR_BASE + 126);
    enum B_MEDIA_NO_HANDLER              = (B_MEDIA_ERROR_BASE + 127);
    enum B_MEDIA_DUPLICATE_FORMAT        = (B_MEDIA_ERROR_BASE + 128);
    enum B_MEDIA_REALTIME_DISABLED       = (B_MEDIA_ERROR_BASE + 129);
    enum B_MEDIA_REALTIME_UNAVAILABLE    = (B_MEDIA_ERROR_BASE + 130);

    /* Mail Kit Errors */
    enum B_MAIL_NO_DAEMON                = (B_MAIL_ERROR_BASE + 0);
    enum B_MAIL_UNKNOWN_USER             = (B_MAIL_ERROR_BASE + 1);
    enum B_MAIL_WRONG_PASSWORD           = (B_MAIL_ERROR_BASE + 2);
    enum B_MAIL_UNKNOWN_HOST             = (B_MAIL_ERROR_BASE + 3);
    enum B_MAIL_ACCESS_ERROR             = (B_MAIL_ERROR_BASE + 4);
    enum B_MAIL_UNKNOWN_FIELD            = (B_MAIL_ERROR_BASE + 5);
    enum B_MAIL_NO_RECIPIENT             = (B_MAIL_ERROR_BASE + 6);
    enum B_MAIL_INVALID_MAIL             = (B_MAIL_ERROR_BASE + 7);

    /* Printing Errors */
    enum B_NO_PRINT_SERVER               = (B_PRINT_ERROR_BASE + 0);

    /* Device Kit Errors */
    enum B_DEV_INVALID_IOCTL             = (B_DEVICE_ERROR_BASE + 0);
    enum B_DEV_NO_MEMORY                 = (B_DEVICE_ERROR_BASE + 1);
    enum B_DEV_BAD_DRIVE_NUM             = (B_DEVICE_ERROR_BASE + 2);
    enum B_DEV_NO_MEDIA                  = (B_DEVICE_ERROR_BASE + 3);
    enum B_DEV_UNREADABLE                = (B_DEVICE_ERROR_BASE + 4);
    enum B_DEV_FORMAT_ERROR              = (B_DEVICE_ERROR_BASE + 5);
    enum B_DEV_TIMEOUT                   = (B_DEVICE_ERROR_BASE + 6);
    enum B_DEV_RECALIBRATE_ERROR         = (B_DEVICE_ERROR_BASE + 7);
    enum B_DEV_SEEK_ERROR                = (B_DEVICE_ERROR_BASE + 8);
    enum B_DEV_ID_ERROR                  = (B_DEVICE_ERROR_BASE + 9);
    enum B_DEV_READ_ERROR                = (B_DEVICE_ERROR_BASE + 10);
    enum B_DEV_WRITE_ERROR               = (B_DEVICE_ERROR_BASE + 11);
    enum B_DEV_NOT_READY                 = (B_DEVICE_ERROR_BASE + 12);
    enum B_DEV_MEDIA_CHANGED             = (B_DEVICE_ERROR_BASE + 13);
    enum B_DEV_MEDIA_CHANGE_REQUESTED    = (B_DEVICE_ERROR_BASE + 14);
    enum B_DEV_RESOURCE_CONFLICT         = (B_DEVICE_ERROR_BASE + 15);
    enum B_DEV_CONFIGURATION_ERROR       = (B_DEVICE_ERROR_BASE + 16);
    enum B_DEV_DISABLED_BY_USER          = (B_DEVICE_ERROR_BASE + 17);
    enum B_DEV_DOOR_OPEN                 = (B_DEVICE_ERROR_BASE + 18);

    enum B_DEV_INVALID_PIPE              = (B_DEVICE_ERROR_BASE + 19);
    enum B_DEV_CRC_ERROR                 = (B_DEVICE_ERROR_BASE + 20);
    enum B_DEV_STALLED                   = (B_DEVICE_ERROR_BASE + 21);
    enum B_DEV_BAD_PID                   = (B_DEVICE_ERROR_BASE + 22);
    enum B_DEV_UNEXPECTED_PID            = (B_DEVICE_ERROR_BASE + 23);
    enum B_DEV_DATA_OVERRUN              = (B_DEVICE_ERROR_BASE + 24);
    enum B_DEV_DATA_UNDERRUN             = (B_DEVICE_ERROR_BASE + 25);
    enum B_DEV_FIFO_OVERRUN              = (B_DEVICE_ERROR_BASE + 26);
    enum B_DEV_FIFO_UNDERRUN             = (B_DEVICE_ERROR_BASE + 27);
    enum B_DEV_PENDING                   = (B_DEVICE_ERROR_BASE + 28);
    enum B_DEV_MULTIPLE_ERRORS           = (B_DEVICE_ERROR_BASE + 29);
    enum B_DEV_TOO_LATE                  = (B_DEVICE_ERROR_BASE + 30);

    /* Translation Kit Errors */
    enum B_TRANSLATION_BASE_ERROR        = (B_TRANSLATION_ERROR_BASE + 0);
    enum B_NO_TRANSLATOR                 = (B_TRANSLATION_ERROR_BASE + 1);
    enum B_ILLEGAL_DATA                  = (B_TRANSLATION_ERROR_BASE + 2);
}
else
{
    static assert(false, "Unsupported platform");
}
