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
module core.sys.posix.stdio;

import core.sys.posix.config;
public import core.stdc.stdio;
public import core.sys.posix.sys.types; // for off_t

version (OSX)
    version = Darwin;
else version (iOS)
    version = Darwin;
else version (TVOS)
    version = Darwin;
else version (WatchOS)
    version = Darwin;

version (Posix):
extern (C):

nothrow:
@nogc:

//
// Required (defined in core.stdc.stdio)
//
/*
BUFSIZ
_IOFBF
_IOLBF
_IONBF
L_tmpnam
SEEK_CUR
SEEK_END
SEEK_SET
FILENAME_MAX
FOPEN_MAX
TMP_MAX
EOF
NULL
stderr
stdin
stdout
FILE
fpos_t
size_t

void   clearerr(FILE*);
int    fclose(FILE*);
int    feof(FILE*);
int    ferror(FILE*);
int    fflush(FILE*);
int    fgetc(FILE*);
int    fgetpos(FILE*, fpos_t *);
char*  fgets(char*, int, FILE*);
FILE*  fopen(const scope char*, const scope char*);
int    fprintf(FILE*, const scope char*, ...);
int    fputc(int, FILE*);
int    fputs(const scope char*, FILE*);
size_t fread(void *, size_t, size_t, FILE*);
FILE*  freopen(const scope char*, const scope char*, FILE*);
int    fscanf(FILE*, const scope char*, ...);
int    fseek(FILE*, c_long, int);
int    fsetpos(FILE*, const scope fpos_t*);
c_long ftell(FILE*);
size_t fwrite(in void *, size_t, size_t, FILE*);
int    getc(FILE*);
int    getchar();
char*  gets(char*);
void   perror(const scope char*);
int    printf(const scope char*, ...);
int    putc(int, FILE*);
int    putchar(int);
int    puts(const scope char*);
int    remove(const scope char*);
int    rename(const scope char*, const scope char*);
void   rewind(FILE*);
int    scanf(const scope char*, ...);
void   setbuf(FILE*, char*);
int    setvbuf(FILE*, char*, int, size_t);
int    snprintf(char*, size_t, const scope char*, ...);
int    sprintf(char*, const scope char*, ...);
int    sscanf(const scope char*, const scope char*, int ...);
FILE*  tmpfile();
char*  tmpnam(char*);
int    ungetc(int, FILE*);
int    vfprintf(FILE*, const scope char*, va_list);
int    vfscanf(FILE*, const scope char*, va_list);
int    vprintf(const scope char*, va_list);
int    vscanf(const scope char*, va_list);
int    vsnprintf(char*, size_t, const scope char*, va_list);
int    vsprintf(char*, const scope char*, va_list);
int    vsscanf(const scope char*, const scope char*, va_list arg);
*/

version (CRuntime_Glibc)
{
    /*
     * actually, if __USE_FILE_OFFSET64 && !_LARGEFILE64_SOURCE
     * the *64 functions shouldn't be visible, but the aliases should
     * still be supported
     */
    static if ( __USE_FILE_OFFSET64 )
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos64 fgetpos;

        FILE* fopen64(const scope char*, const scope char*);
        alias fopen64 fopen;

        FILE* freopen64(const scope char*, const scope char*, FILE*);
        alias freopen64 freopen;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, const scope fpos_t*);
        alias fsetpos64 fsetpos;

        FILE* tmpfile64();
        alias tmpfile64 tmpfile;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(const scope char*, const scope char*);
        FILE* freopen(const scope char*, const scope char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, const scope fpos_t*);
        FILE* tmpfile();
    }
}
else version (CRuntime_Bionic)
{
    int   fgetpos(FILE*, fpos_t *);
    FILE* fopen(const scope char*, const scope char*);
    FILE* freopen(const scope char*, const scope char*, FILE*);
    int   fseek(FILE*, c_long, int);
    int   fsetpos(FILE*, const scope fpos_t*);
}
else version (CRuntime_UClibc)
{
    static if ( __USE_FILE_OFFSET64 )
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos64 fgetpos;

        FILE* fopen64(const scope char*, const scope char*);
        alias fopen64 fopen;

        FILE* freopen64(const scope char*, const scope char*, FILE*);
        alias freopen64 freopen;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, const scope fpos_t*);
        alias fsetpos64 fsetpos;

        FILE* tmpfile64();
        alias tmpfile64 tmpfile;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(const scope char*, const scope char*);
        FILE* freopen(const scope char*, const scope char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, const scope fpos_t*);
        FILE* tmpfile();
    }
}
else version (CRuntime_Musl)
{
    static if ( __USE_FILE_OFFSET64 )
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos64 fgetpos;

        FILE* fopen64(const scope char*, const scope char*);
        alias fopen64 fopen;

        FILE* freopen64(const scope char*, const scope char*, FILE*);
        alias freopen64 freopen;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, const scope fpos_t*);
        alias fsetpos64 fsetpos;

        FILE* tmpfile64();
        alias tmpfile64 tmpfile;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(const scope char*, const scope char*);
        FILE* freopen(const scope char*, const scope char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, const scope fpos_t*);
        FILE* tmpfile();
    }
}
else version (Solaris)
{
    static if (__USE_FILE_OFFSET64 && __WORDSIZE != 64)
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos = fgetpos64;

        FILE* fopen64(const scope char*, const scope char*);
        alias fopen = fopen64;

        FILE* freopen64(const scope char*, const scope char*, FILE*);
        alias freopen = freopen64;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, const scope fpos_t*);
        alias fsetpos = fsetpos64;

        FILE* tmpfile64();
        alias tmpfile = tmpfile64;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(const scope char*, const scope char*);
        FILE* freopen(const scope char*, const scope char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, const scope fpos_t*);
        FILE* tmpfile();
    }
}

//
// C Extension (CX)
//
/*
L_ctermid

char*   ctermid(char*);
FILE*   fdopen(int, const scope char*);
int     fileno(FILE*);
int     fseeko(FILE*, off_t, int);
off_t   ftello(FILE*);
ssize_t getdelim(char**, size_t*, int, FILE*);
ssize_t getline(char**, size_t*, FILE*);
char*   gets(char*);
int     pclose(FILE*);
FILE*   popen(const scope char*, const scope char*);
*/

version (CRuntime_Glibc)
{
    enum L_ctermid = 9;

    static if ( __USE_FILE_OFFSET64 )
    {
        int   fseeko64(FILE*, off_t, int);
        alias fseeko64 fseeko;
    }
    else
    {
        int   fseeko(FILE*, off_t, int);
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        off_t ftello64(FILE*);
        alias ftello64 ftello;
    }
    else
    {
        off_t ftello(FILE*);
    }

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (CRuntime_UClibc)
{
    enum L_ctermid = 9;
    enum L_cuserid = 9;

    static if ( __USE_FILE_OFFSET64 )
    {
        int   fseeko64(FILE*, off_t, int);
        alias fseeko64 fseeko;
    }
    else
    {
        int   fseeko(FILE*, off_t, int);
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        off_t ftello64(FILE*);
        alias ftello64 ftello;
    }
    else
    {
        off_t ftello(FILE*);
    }

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (CRuntime_Musl)
{
    enum L_ctermid = 20;

    static if ( __USE_FILE_OFFSET64 )
    {
        int   fseeko64(FILE*, off_t, int);
        alias fseeko64 fseeko;
    }
    else
    {
        int   fseeko(FILE*, off_t, int);
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        off_t ftello64(FILE*);
        alias ftello64 ftello;
    }
    else
    {
        off_t ftello(FILE*);
    }

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (CRuntime_Bionic)
{
    enum L_ctermid = 1024;

    static if ( __USE_FILE_OFFSET64 )
    {
        int   fseeko64(FILE*, off_t, int);
        alias fseeko64 fseeko;
    }
    else
    {
        int   fseeko(FILE*, off_t, int);
    }

    static if ( __USE_FILE_OFFSET64 )
    {
        off_t ftello64(FILE*);
        alias ftello64 ftello;
    }
    else
    {
        off_t ftello(FILE*);
    }

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (Darwin)
{
    enum L_ctermid = 1024;

    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (FreeBSD)
{
    import core.sys.freebsd.config;

    enum L_ctermid = 1024;

    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);

    static if (__FreeBSD_version >= 800000)
    {
        ssize_t getdelim(char**, size_t*, int, FILE*);
        ssize_t getline(char**, size_t*, FILE*);
    }
}
else version (NetBSD)
{
    enum L_ctermid = 1024;

    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (OpenBSD)
{
    enum L_ctermid = 1024;

    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (DragonFlyBSD)
{
    enum L_ctermid = 1024;

    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (Solaris)
{
    enum L_ctermid = 9;
    enum L_cuserid = 9;

    static if (__USE_FILE_OFFSET64 && __WORDSIZE != 64)
    {
        int   fseeko64(FILE*, off_t, int);
        alias fseeko = fseeko64;
    }
    else
    {
        int   fseeko(FILE*, off_t, int);
    }

    static if (__USE_FILE_OFFSET64 && __WORDSIZE != 64)
    {
        off_t ftello64(FILE*);
        alias ftello = ftello64;
    }
    else
    {
        off_t ftello(FILE*);
    }

    ssize_t getdelim(char**, size_t*, int, FILE*);
    ssize_t getline(char**, size_t*, FILE*);
}
else version (Posix)
{
    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);
}

char*  ctermid(char*);
FILE*  fdopen(int, const scope char*);
int    fileno(FILE*);
char*  gets(char*);
int    pclose(FILE*);
FILE*  popen(const scope char*, const scope char*);


// memstream functions are conforming to POSIX.1-2008.  These functions are
// not specified in POSIX.1-2001 and are not widely available on other
// systems.
version (CRuntime_Glibc)                     // as of glibc 1.0x
    version = HaveMemstream;
else version (FreeBSD)                      // as of FreeBSD 9.2
    version = HaveMemstream;
else version (DragonFlyBSD)                 // for DragonFlyBSD
    version = HaveMemstream;
else version (OpenBSD)                      // as of OpenBSD 5.4
    version = HaveMemstream;
else version (CRuntime_UClibc)
    version = HaveMemstream;
// http://git.musl-libc.org/cgit/musl/commit/src/stdio/open_memstream.c?id=b158b32a44d56ef20407d4285b58180447ffff1f
else version (CRuntime_Musl)
    version = HaveMemstream;

version (HaveMemstream)
{
    FILE*  fmemopen(const scope void* buf, size_t size, const scope char* mode);
    FILE*  open_memstream(char** ptr, size_t* sizeloc);
    version (CRuntime_UClibc) {} else
    FILE*  open_wmemstream(wchar_t** ptr, size_t* sizeloc);
}

//
// Thread-Safe Functions (TSF)
//
/*
void   flockfile(FILE*);
int    ftrylockfile(FILE*);
void   funlockfile(FILE*);
int    getc_unlocked(FILE*);
int    getchar_unlocked();
int    putc_unlocked(int, FILE*);
int    putchar_unlocked(int);
*/

version (CRuntime_Glibc)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (CRuntime_Musl)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (CRuntime_Bionic)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (Darwin)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (FreeBSD)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (NetBSD)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (OpenBSD)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (DragonFlyBSD)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (Solaris)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}
else version (CRuntime_UClibc)
{
    void   flockfile(FILE*);
    int    ftrylockfile(FILE*);
    void   funlockfile(FILE*);
    int    getc_unlocked(FILE*);
    int    getchar_unlocked();
    int    putc_unlocked(int, FILE*);
    int    putchar_unlocked(int);
}

//
// XOpen (XSI)
//
/*
P_tmpdir
va_list (defined in core.stdc.stdarg)

char*  tempnam(const scope char*, const scope char*);
*/

char*  tempnam(const scope char*, const scope char*);

version (CRuntime_Glibc)
{
    enum P_tmpdir  = "/tmp";
}
else version (CRuntime_Musl)
{
    enum P_tmpdir  = "/tmp";
}
else version (Darwin)
{
    enum P_tmpdir  = "/var/tmp";
}
else version (FreeBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
else version (NetBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
else version (OpenBSD)
{
    enum P_tmpdir  = "/tmp/";
}
else version (DragonFlyBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
else version (Solaris)
{
    enum P_tmpdir  = "/var/tmp/";
}
else version (CRuntime_UClibc)
{
    enum P_tmpdir  = "/tmp";
}

version (HaveMemstream)
unittest
{ /* fmemopen */
    import core.stdc.string : memcmp;
    byte[10] buf;
    auto f = fmemopen(buf.ptr, 10, "w");
    assert(f !is null);
    assert(fprintf(f, "hello") == "hello".length);
    assert(fflush(f) == 0);
    assert(memcmp(buf.ptr, "hello".ptr, "hello".length) == 0);
    //assert(buf
    assert(fclose(f) == 0);
}

version (HaveMemstream)
unittest
{ /* Note: open_memstream is only useful for writing */
    import core.stdc.string : memcmp;
    char* ptr = null;
    char[6] testdata = ['h', 'e', 'l', 'l', 'o', 0];
    size_t sz = 0;
    auto f = open_memstream(&ptr, &sz);
    assert(f !is null);
    assert(fprintf(f, "%s", testdata.ptr) == 5);
    assert(fflush(f) == 0);
    assert(memcmp(ptr, testdata.ptr, testdata.length) == 0);
    assert(fclose(f) == 0);
}

version (CRuntime_UClibc) {} else
version (HaveMemstream)
unittest
{ /* Note: open_wmemstream is only useful for writing */
    import core.stdc.string : memcmp;
    import core.stdc.wchar_ : fwprintf;
    wchar_t* ptr = null;
    wchar_t[6] testdata = ['h', 'e', 'l', 'l', 'o', 0];
    size_t sz = 0;
    auto f = open_wmemstream(&ptr, &sz);
    assert(f !is null);
    assert(fwprintf(f, testdata.ptr) == 5);
    assert(fflush(f) == 0);
    assert(memcmp(ptr, testdata.ptr, testdata.length*wchar_t.sizeof) == 0);
    assert(fclose(f) == 0);
}
