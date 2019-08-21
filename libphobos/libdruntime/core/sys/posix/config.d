/**
 * D header file for POSIX.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly,
              Alex Rønne Petersen
 * Standards: The Open Group Base Specifications Issue 6, IEEE Std 1003.1, 2004 Edition
 */

/*          Copyright Sean Kelly 2005 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module core.sys.posix.config;

public import core.stdc.config;

version (Posix):
extern (C) nothrow @nogc:

enum _XOPEN_SOURCE     = 600;
enum _POSIX_SOURCE     = true;
enum _POSIX_C_SOURCE   = 200112L;

version (CRuntime_Glibc)
{
    // man 7 feature_test_macros
    // http://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
    enum _GNU_SOURCE         = false;
    enum _DEFAULT_SOURCE     = false;
    enum _ATFILE_SOURCE      = false;

    // _BSD_SOURCE and _SVID_SOURCE are deprecated aliases for _DEFAULT_SOURCE.
    deprecated("use _DEFAULT_SOURCE")
    {
        enum _BSD_SOURCE = false;
        enum _SVID_SOURCE = false;
    }

    enum _FILE_OFFSET_BITS   = 64;
    // <sys/cdefs.h>
    enum __REDIRECT          = false;

    enum _REENTRANT          = true; // set by compiler when linking -pthread

    // deduced <features.h>
    enum __USE_FILE_OFFSET64 = _FILE_OFFSET_BITS == 64;
    enum __USE_LARGEFILE     = __USE_FILE_OFFSET64 && !__REDIRECT;
    enum __USE_LARGEFILE64   = __USE_FILE_OFFSET64 && !__REDIRECT;

    enum __USE_XOPEN2K       = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2KXSI    = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2K8      = _XOPEN_SOURCE >= 700;
    enum __USE_XOPEN2K8XSI   = _XOPEN_SOURCE >= 700;

    enum __USE_MISC          = _DEFAULT_SOURCE;
    enum __USE_ATFILE        = _ATFILE_SOURCE;
    enum __USE_GNU           = _GNU_SOURCE;
    enum __USE_REENTRANT     = _REENTRANT;

    version (D_LP64)
        enum __WORDSIZE=64;
    else
        enum __WORDSIZE=32;
}
else version (CRuntime_Musl)
{
    enum _FILE_OFFSET_BITS   = 64;

    enum __REDIRECT          = false;

    enum __USE_FILE_OFFSET64 = _FILE_OFFSET_BITS == 64;
    enum __USE_LARGEFILE     = __USE_FILE_OFFSET64 && !__REDIRECT;
    enum __USE_LARGEFILE64   = __USE_FILE_OFFSET64 && !__REDIRECT;

    enum __WORDSIZE=64;
}
else version (CRuntime_UClibc)
{
    enum _GNU_SOURCE         = false;
    enum _DEFAULT_SOURCE     = false;
    enum _ATFILE_SOURCE      = false;

    enum _FILE_OFFSET_BITS   = 64;
    enum __REDIRECT          = false;

    enum _REENTRANT          = true;

    enum __USE_FILE_OFFSET64 = _FILE_OFFSET_BITS == 64;
    enum __USE_LARGEFILE     = __USE_FILE_OFFSET64 && !__REDIRECT;
    enum __USE_LARGEFILE64   = __USE_FILE_OFFSET64 && !__REDIRECT;

    enum __USE_XOPEN2K       = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2KXSI    = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2K8      = _XOPEN_SOURCE >= 700;
    enum __USE_XOPEN2K8XSI   = _XOPEN_SOURCE >= 700;

    enum __USE_MISC          = _DEFAULT_SOURCE;
    enum __USE_ATFILE        = _ATFILE_SOURCE;
    enum __USE_GNU           = _GNU_SOURCE;
    enum __USE_REENTRANT     = _REENTRANT;

    version (D_LP64)
        enum __WORDSIZE=64;
    else
        enum __WORDSIZE=32;
}
else version (CRuntime_Bionic)
{
    enum __USE_GNU           = false;
}
else version (OpenBSD)
{
    version (Alpha)
    {
        enum _ALIGNBYTES = 7;
        enum _STACKALIGNBYTES = 7;
        enum _MAX_PAGE_SHIFT = 13;
    }
    else version (X86_64)
    {
        enum _ALIGNBYTES = c_long.sizeof - 1;
        enum _STACKALIGNBYTES = 15;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (AArch64)
    {
        enum _ALIGNBYTES = c_long.sizeof - 1;
        enum _STACKALIGNBYTES = 15;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (ARM)
    {
        enum _ALIGNBYTES = 7;
        enum _STACKALIGNBYTES = 7;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (HPPA)
    {
        enum _ALIGNBYTES = 7;
        enum _STACKALIGNBYTES = 7;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (X86)
    {
        enum _ALIGNBYTES = 3;
        enum _STACKALIGNBYTES = 15;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (PPC)
    {
        enum _ALIGNBYTES = 7;
        enum _STACKALIGNBYTES = 15;
        enum _MAX_PAGE_SHIFT = 12;
    }
    else version (SPARC64)
    {
        enum _ALIGNBYTES = 15;
        enum _STACKALIGNBYTES = 15;
        enum _MAX_PAGE_SHIFT = 13;
    }
    else
        static assert(false, "Architecture not supported.");
}
else version (Solaris)
{
    enum _FILE_OFFSET_BITS = 64;
    enum __REDIRECT = false;

    enum __USE_FILE_OFFSET64 = _FILE_OFFSET_BITS == 64;
    enum __USE_LARGEFILE = __USE_FILE_OFFSET64 && !__REDIRECT;
    enum __USE_LARGEFILE64 = __USE_FILE_OFFSET64 && !__REDIRECT;

    enum __USE_XOPEN2K = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2KXSI = _XOPEN_SOURCE >= 600;
    enum __USE_XOPEN2K8 = _XOPEN_SOURCE >= 700;
    enum __USE_XOPEN2K8XSI = _XOPEN_SOURCE >= 700;

    version (D_LP64)
        enum __WORDSIZE = 64;
    else
        enum __WORDSIZE = 32;
}
