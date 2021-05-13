/**
 * D header file for DragonFlyBSD
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: Martin Nowak,Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.dlfcn;

version (DragonFlyBSD):

public import core.sys.posix.dlfcn;

extern (C) nothrow @nogc @system:

/*
 * Modes and flags for dlopen().
 */
static assert(RTLD_LAZY   == 1);
static assert(RTLD_NOW    == 2);
enum RTLD_MODEMASK        =  0x3;
static assert(RTLD_GLOBAL == 0x100);
static assert(RTLD_LOCAL  == 0);
enum RTLD_TRACE           =  0x200;
enum RTLD_NODELETE        =  0x01000;
enum RTLD_NOLOAD          =  0x02000;

/*
 * Request arguments for dlinfo().
 */
enum RTLD_DI_LINKMAP     = 2;    /* Obtain link map. */
enum RTLD_DI_SERINFO     = 4;    /* Obtain search path info. */
enum RTLD_DI_SERINFOSIZE = 5;    /*  ... query for required space. */
enum RTLD_DI_ORIGIN      = 6;    /* Obtain object origin */
enum RTLD_DI_MAX         = RTLD_DI_ORIGIN;

/*
 * Special handle arguments for dlsym()/dlinfo().
 */
enum RTLD_NEXT    = cast(void *)-1;    /* Search subsequent objects. */
enum RTLD_DEFAULT = cast(void *)-2;    /* Use default search algorithm. */
enum RTLD_SELF    = cast(void *)-3;    /* Search the caller itself. */

/*
 * Structure filled in by dladdr().
 */
struct Dl_info {
    const(char)     *dli_fname;     /* Pathname of shared object. */
    void            *dli_fbase;     /* Base address of shared object. */
    const(char)     *dli_sname;     /* Name of nearest symbol. */
    void            *dli_saddr;     /* Address of nearest symbol. */
}


/*
 * Structures, returned by the RTLD_DI_SERINFO dlinfo() request.
 */
struct Dl_serpath {
    char *          dls_name;       /* single search path entry */
    uint            dls_flags;      /* path information */
}

struct Dl_serinfo {
    size_t          dls_size;       /* total buffer size */
    uint            dls_cnt;        /* number of path entries */
    Dl_serpath[1]   dls_serpath;    /* there may be more than one */
}

/*-
 * The actual type declared by this typedef is immaterial, provided that
 * it is a function pointer.  Its purpose is to provide a return type for
 * dlfunc() which can be cast to a function pointer type without depending
 * on behavior undefined by the C standard, which might trigger a compiler
 * diagnostic.  We intentionally declare a unique type signature to force
 * a diagnostic should the application not cast the return value of dlfunc()
 * appropriately.
 */
struct __dlfunc_arg {
    int     __dlfunc_dummy;
}

alias dlfunc_t = void function(__dlfunc_arg);

/* XSI functions first. */
extern(C) {
    static assert(is(typeof(&dlclose) == int function(void*)));
    static assert(is(typeof(&dlerror) == char* function()));
    static assert(is(typeof(&dlopen)  == void* function(const scope char*, int)));
    static assert(is(typeof(&dlsym)   == void* function(void*, const scope char*)));
}

void*    fdlopen(int, int);
int      dladdr(const(void)*, Dl_info*);
dlfunc_t dlfunc(void*, const(char)*);
int      dlinfo(void*, int, void*);
/*void     dllockinit(void* _context,
    void* function(void* _context) _lock_create,
    void  function(void* _lock)    _rlock_acquire,
    void  function(void* _lock)    _wlock_acquire,
    void  function(void* _lock)    _lock_release,
    void  function(void* _lock)    _lock_destroy,
    void  function(void* _context) _context_destroy);*/
void*    dlvsym(void*, const(char)*, const(char)*);
