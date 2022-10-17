/**
 * D header file for NetBSD.
 *
 * Copyright: Copyright Martin Nowak 2012.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Martin Nowak
 *
 * http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/include/dlfcn.h
 */
module core.sys.netbsd.dlfcn;

public import core.sys.posix.dlfcn;

version (NetBSD):
extern (C):
nothrow:
@nogc:

enum __BSD_VISIBLE = true;

/*
 * Request arguments for dlinfo().
 */
enum RTLD_DI_LINKMAP     = 3;    /* Obtain link map. */
enum RTLD_DI_SERINFO     = 5;    /* Obtain search path info. */
enum RTLD_DI_SERINFOSIZE = 6;    /*  ... query for required space. */
enum RTLD_DI_ORIGIN      = 7;    /* Obtain object origin */
enum RTLD_DI_MAX         = RTLD_DI_ORIGIN;

/*
 * Special handle arguments for dlsym()/dlinfo().
 */
enum RTLD_NEXT    = cast(void *)-1;    /* Search subsequent objects. */
enum RTLD_DEFAULT = cast(void *)-2;    /* Use default search algorithm. */
enum RTLD_SELF    = cast(void *)-3;    /* Search the caller itself. */

static if (__BSD_VISIBLE)
{
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
}

/* XSI functions first. */
extern(C) {
    static assert(is(typeof(&dlclose) == int function(void*)));
    static assert(is(typeof(&dlerror) == char* function()));
    static assert(is(typeof(&dlopen)  == void* function(const scope char*, int)));
    static assert(is(typeof(&dlsym)   == void* function(void*, const scope char*)));
}

static if (__BSD_VISIBLE)
{
    int      dlinfo(void*, int, void*);
    void*    dlvsym(void*, const(char)*, const(char)*);
}
