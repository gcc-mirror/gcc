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

private import core.sys.posix.config;
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
FILE*  fopen(in char*, in char*);
int    fprintf(FILE*, in char*, ...);
int    fputc(int, FILE*);
int    fputs(in char*, FILE*);
size_t fread(void *, size_t, size_t, FILE*);
FILE*  freopen(in char*, in char*, FILE*);
int    fscanf(FILE*, in char*, ...);
int    fseek(FILE*, c_long, int);
int    fsetpos(FILE*, in fpos_t*);
c_long ftell(FILE*);
size_t fwrite(in void *, size_t, size_t, FILE*);
int    getc(FILE*);
int    getchar();
char*  gets(char*);
void   perror(in char*);
int    printf(in char*, ...);
int    putc(int, FILE*);
int    putchar(int);
int    puts(in char*);
int    remove(in char*);
int    rename(in char*, in char*);
void   rewind(FILE*);
int    scanf(in char*, ...);
void   setbuf(FILE*, char*);
int    setvbuf(FILE*, char*, int, size_t);
int    snprintf(char*, size_t, in char*, ...);
int    sprintf(char*, in char*, ...);
int    sscanf(in char*, in char*, int ...);
FILE*  tmpfile();
char*  tmpnam(char*);
int    ungetc(int, FILE*);
int    vfprintf(FILE*, in char*, va_list);
int    vfscanf(FILE*, in char*, va_list);
int    vprintf(in char*, va_list);
int    vscanf(in char*, va_list);
int    vsnprintf(char*, size_t, in char*, va_list);
int    vsprintf(char*, in char*, va_list);
int    vsscanf(in char*, in char*, va_list arg);
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

        FILE* fopen64(in char*, in char*);
        alias fopen64 fopen;

        FILE* freopen64(in char*, in char*, FILE*);
        alias freopen64 freopen;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, in fpos_t*);
        alias fsetpos64 fsetpos;

        FILE* tmpfile64();
        alias tmpfile64 tmpfile;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(in char*, in char*);
        FILE* freopen(in char*, in char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, in fpos_t*);
        FILE* tmpfile();
    }
}
else version (CRuntime_Bionic)
{
    int   fgetpos(FILE*, fpos_t *);
    FILE* fopen(in char*, in char*);
    FILE* freopen(in char*, in char*, FILE*);
    int   fseek(FILE*, c_long, int);
    int   fsetpos(FILE*, in fpos_t*);
}
else version (CRuntime_UClibc)
{
    static if ( __USE_FILE_OFFSET64 )
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos64 fgetpos;

        FILE* fopen64(in char*, in char*);
        alias fopen64 fopen;

        FILE* freopen64(in char*, in char*, FILE*);
        alias freopen64 freopen;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, in fpos_t*);
        alias fsetpos64 fsetpos;

        FILE* tmpfile64();
        alias tmpfile64 tmpfile;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(in char*, in char*);
        FILE* freopen(in char*, in char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, in fpos_t*);
        FILE* tmpfile();
    }
}
else version (Solaris)
{
    static if (__USE_FILE_OFFSET64 && __WORDSIZE != 64)
    {
        int   fgetpos64(FILE*, fpos_t *);
        alias fgetpos = fgetpos64;

        FILE* fopen64(in char*, in char*);
        alias fopen = fopen64;

        FILE* freopen64(in char*, in char*, FILE*);
        alias freopen = freopen64;

        int   fseek(FILE*, c_long, int);

        int   fsetpos64(FILE*, in fpos_t*);
        alias fsetpos = fsetpos64;

        FILE* tmpfile64();
        alias tmpfile = tmpfile64;
    }
    else
    {
        int   fgetpos(FILE*, fpos_t *);
        FILE* fopen(in char*, in char*);
        FILE* freopen(in char*, in char*, FILE*);
        int   fseek(FILE*, c_long, int);
        int   fsetpos(FILE*, in fpos_t*);
        FILE* tmpfile();
    }
}

//
// C Extension (CX)
//
/*
L_ctermid

char*  ctermid(char*);
FILE*  fdopen(int, in char*);
int    fileno(FILE*);
int    fseeko(FILE*, off_t, int);
off_t  ftello(FILE*);
char*  gets(char*);
int    pclose(FILE*);
FILE*  popen(in char*, in char*);
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
}
else version (Posix)
{
    int   fseeko(FILE*, off_t, int);
    off_t ftello(FILE*);
}

char*  ctermid(char*);
FILE*  fdopen(int, in char*);
int    fileno(FILE*);
//int    fseeko(FILE*, off_t, int);
//off_t  ftello(FILE*);
char*  gets(char*);
int    pclose(FILE*);
FILE*  popen(in char*, in char*);


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

version (HaveMemstream)
{
    FILE*  fmemopen(in void* buf, in size_t size, in char* mode);
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

char*  tempnam(in char*, in char*);
*/

char*  tempnam(in char*, in char*);

version (CRuntime_Glibc)
{
    enum P_tmpdir  = "/tmp";
}
version (CRuntime_Musl)
{
    enum P_tmpdir  = "/tmp";
}
version (Darwin)
{
    enum P_tmpdir  = "/var/tmp";
}
version (FreeBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
version (NetBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
version (OpenBSD)
{
    enum P_tmpdir  = "/tmp/";
}
version (DragonFlyBSD)
{
    enum P_tmpdir  = "/var/tmp/";
}
version (Solaris)
{
    enum P_tmpdir  = "/var/tmp/";
}
version (CRuntime_UClibc)
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


ssize_t getdelim (char** lineptr, size_t* n, int delimiter, FILE* stream);
ssize_t getline (char** lineptr, size_t* n, FILE* stream);
