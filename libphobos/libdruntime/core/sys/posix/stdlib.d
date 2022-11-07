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
module core.sys.posix.stdlib;

import core.sys.posix.config;
public import core.stdc.stdlib;
public import core.sys.posix.sys.wait;

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
// Required (defined in core.stdc.stdlib)
//
/*
EXIT_FAILURE
EXIT_SUCCESS
NULL
RAND_MAX
MB_CUR_MAX
div_t
ldiv_t
lldiv_t
size_t
wchar_t

void    _Exit(int);
void    abort();
int     abs(int);
int     atexit(void function());
double  atof(const scope char*);
int     atoi(const scope char*);
c_long  atol(const scope char*);
long    atoll(const scope char*);
void*   bsearch(const scope void*, const scope void*, size_t, size_t, int function(const scope void*, const scope void*));
void*   calloc(size_t, size_t);
div_t   div(int, int);
void    exit(int);
void    free(void*);
char*   getenv(const scope char*);
c_long  labs(c_long);
ldiv_t  ldiv(c_long, c_long);
long    llabs(long);
lldiv_t lldiv(long, long);
void*   malloc(size_t);
int     mblen(const scope char*, size_t);
size_t  mbstowcs(wchar_t*, const scope char*, size_t);
int     mbtowc(wchar_t*, const scope char*, size_t);
void    qsort(void*, size_t, size_t, int function(const scope void*, const scope void*));
int     rand();
void*   realloc(void*, size_t);
void    srand(uint);
double  strtod(const scope char*, char**);
float   strtof(const scope char*, char**);
c_long  strtol(const scope char*, char**, int);
real    strtold(const scope char*, char**);
long    strtoll(const scope char*, char**, int);
c_ulong strtoul(const scope char*, char**, int);
ulong   strtoull(const scope char*, char**, int);
int     system(const scope char*);
size_t  wcstombs(char*, const scope wchar_t*, size_t);
int     wctomb(char*, wchar_t);
*/

//
// Advisory Information (ADV)
//
/*
int posix_memalign(void**, size_t, size_t);
*/

version (CRuntime_Glibc)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (FreeBSD)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (NetBSD)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (OpenBSD)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (DragonFlyBSD)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (Solaris)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (Darwin)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (CRuntime_Bionic)
{
    // Added since Lollipop
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (CRuntime_Musl)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}
else version (CRuntime_UClibc)
{
    int posix_memalign(scope void**, size_t, size_t) pure;
}

//
// C Extension (CX)
//
/*
int setenv(const scope char*, const scope char*, int);
int unsetenv(const scope char*);
*/

version (CRuntime_Glibc)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (Darwin)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (FreeBSD)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (NetBSD)
{
    int setenv(const scope char*, const scope char*, int);
    int __unsetenv13(const scope char*);
    alias __unsetenv13 unsetenv;
    void* valloc(size_t); // LEGACY non-standard
}
else version (OpenBSD)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (DragonFlyBSD)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (CRuntime_Bionic)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t);
}
else version (Solaris)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);

    void* valloc(size_t); // LEGACY non-standard
}
else version (CRuntime_Musl)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);
}
else version (CRuntime_UClibc)
{
    int setenv(const scope char*, const scope char*, int);
    int unsetenv(const scope char*);
    void* valloc(size_t);
}

//
// Thread-Safe Functions (TSF)
//
/*
int rand_r(uint*);
*/

version (CRuntime_Glibc)
{
    int rand_r(uint*);
}
else version (Darwin)
{
    int rand_r(uint*);
}
else version (FreeBSD)
{
    int rand_r(uint*);
}
else version (NetBSD)
{
    int rand_r(uint*);
}
else version (OpenBSD)
{
    int rand_r(uint*);
}
else version (DragonFlyBSD)
{
    int rand_r(uint*);
}
else version (Solaris)
{
    int rand_r(uint*);
}
else version (CRuntime_UClibc)
{
    int rand_r(uint*);
}

//
// XOpen (XSI)
//
/*
WNOHANG     (defined in core.sys.posix.sys.wait)
WUNTRACED   (defined in core.sys.posix.sys.wait)
WEXITSTATUS (defined in core.sys.posix.sys.wait)
WIFEXITED   (defined in core.sys.posix.sys.wait)
WIFSIGNALED (defined in core.sys.posix.sys.wait)
WIFSTOPPED  (defined in core.sys.posix.sys.wait)
WSTOPSIG    (defined in core.sys.posix.sys.wait)
WTERMSIG    (defined in core.sys.posix.sys.wait)

c_long a64l(const scope char*);
double drand48();
char*  ecvt(double, int, int *, int *); // LEGACY
double erand48(ref ushort[3]);
char*  fcvt(double, int, int *, int *); // LEGACY
char*  gcvt(double, int, char*); // LEGACY
// per spec: int getsubopt(char** char* const*, char**);
int    getsubopt(char**, const scope char**, char**);
int    grantpt(int);
char*  initstate(uint, char*, size_t);
c_long jrand48(ref ushort[3]);
char*  l64a(c_long);
void   lcong48(ref ushort[7]);
c_long lrand48();
char*  mktemp(char*); // LEGACY
int    mkstemp(char*);
int    mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
c_long mrand48();
c_long nrand48(ref ushort[3]);
int    posix_openpt(int);
char*  ptsname(int);
int    putenv(char*);
c_long random();
char*  realpath(const scope char*, char*);
ushort *seed48(ref ushort[3]);
void   setkey(const scope char*);
char*  setstate(const scope char*);
void   srand48(c_long);
void   srandom(uint);
int    unlockpt(int);
*/

version (CRuntime_Glibc)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    char*  fcvt(double, int, int *, int *); // LEGACY
    char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    //int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);

  static if ( __USE_LARGEFILE64 )
  {
    int    mkstemp64(char*);
    alias  mkstemp64 mkstemp;
  }
  else
  {
    int    mkstemp(char*);
  }
}
else version (Darwin)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    char*  fcvt(double, int, int *, int *); // LEGACY
    char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);
}
else version (FreeBSD)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    //char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    //char*  fcvt(double, int, int *, int *); // LEGACY
    //char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);
}
else version (NetBSD)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    //char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    //char*  fcvt(double, int, int *, int *); // LEGACY
    //char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);
}
else version (OpenBSD)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    //char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    //char*  fcvt(double, int, int *, int *); // LEGACY
    //char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    // void   setkey(const scope char*); // not implemented
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);
}
else version (DragonFlyBSD)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    //char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    //char*  fcvt(double, int, int *, int *); // LEGACY
    //char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);
}
else version (CRuntime_Bionic)
{
    double  drand48();
    double  erand48(ref ushort[3]);
    //int   grantpt(int); defined inline, but seems to do nothing in bionic
    c_long  jrand48(ref ushort[3]);
    c_long  lrand48();
    char*   mktemp(char*); // LEGACY
    int     mkstemp(char*);
    char*   mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long  mrand48();
    c_long  nrand48(ref ushort[3]);
    char*   ptsname(int);
    int     putenv(const scope char*);
    c_long  random() { return lrand48(); }
    char*   realpath(const scope char*, char*);
    ushort* seed48(ref ushort[3]);
    void    srand48(c_long);
    void    srandom(uint s) { srand48(s); }
    int     unlockpt(int);
}
else version (CRuntime_Musl)
{
    c_long a64l(const scope char*);
    double drand48();
    char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    char*  fcvt(double, int, int *, int *); // LEGACY
    char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    int    mkstemp(char*);
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);

  static if ( __USE_LARGEFILE64 )
  {
    int    mkstemp64(char*);
    alias  mkstemp64 mkstemp;
  }
  else
  {
    int    mkstemp(char*);
  }

}
else version (Solaris)
{
    //WNOHANG     (defined in core.sys.posix.sys.wait)
    //WUNTRACED   (defined in core.sys.posix.sys.wait)
    //WEXITSTATUS (defined in core.sys.posix.sys.wait)
    //WIFEXITED   (defined in core.sys.posix.sys.wait)
    //WIFSIGNALED (defined in core.sys.posix.sys.wait)
    //WIFSTOPPED  (defined in core.sys.posix.sys.wait)
    //WSTOPSIG    (defined in core.sys.posix.sys.wait)
    //WTERMSIG    (defined in core.sys.posix.sys.wait)

    c_long a64l(const scope char*);
    double drand48();
    char*  ecvt(double, int, int *, int *); // LEGACY
    double erand48(ref ushort[3]);
    char*  fcvt(double, int, int *, int *); // LEGACY
    char*  gcvt(double, int, char*); // LEGACY
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*); // LEGACY
    //int    mkstemp(char*);
    char*  mkdtemp(char*); // Defined in IEEE 1003.1, 2008 Edition
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort *seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);

    version (D_LP64)
    {
        int mkstemp(char*);

        static if ( __USE_LARGEFILE64 )
            alias mkstemp mkstemp64;
    }
    else
    {
        int mkstemp64(char*);

        static if ( __USE_LARGEFILE64 )
            alias mkstemp64 mkstemp;
        else
            int mkstemp(char*);
    }
}
else version (CRuntime_UClibc)
{
    c_long a64l(const scope char*);
    double drand48();
    char*  ecvt(double, int, int *, int *);
    double erand48(ref ushort[3]);
    char*  fcvt(double, int, int *, int *);
    char*  gcvt(double, int, char*);
    int    getsubopt(char**, const scope char**, char**);
    int    grantpt(int);
    char*  initstate(uint, char*, size_t);
    c_long jrand48(ref ushort[3]);
    char*  l64a(c_long);
    void   lcong48(ref ushort[7]);
    c_long lrand48();
    char*  mktemp(char*);
    char*  mkdtemp(char*);
    c_long mrand48();
    c_long nrand48(ref ushort[3]);
    int    posix_openpt(int);
    char*  ptsname(int);
    int    putenv(char*);
    c_long random();
    char*  realpath(const scope char*, char*);
    ushort* seed48(ref ushort[3]);
    void   setkey(const scope char*);
    char*  setstate(const scope char*);
    void   srand48(c_long);
    void   srandom(uint);
    int    unlockpt(int);

  static if ( __USE_LARGEFILE64 )
  {
    int    mkstemp64(char*);
    alias  mkstemp64 mkstemp;
  }
  else
  {
    int    mkstemp(char*);
  }
}
