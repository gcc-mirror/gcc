/**
 * D header file for Solaris
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/head/dlfcn.h, illumos dlfcn.h)
 */

module core.sys.solaris.dlfcn;

version (Solaris):
extern (C):
nothrow:
@nogc:

public import core.sys.posix.dlfcn;
import core.stdc.config;

// enum RTLD_LAZY = 0x00001; // POSIX
// enum RTLD_NOW = 0x00002; // POSIX
enum RTLD_NOLOAD = 0x00004;
enum RTLD_DEEPBIND = 0x00008;

// enum RTLD_GLOBAL = 0x00100; // POSIX
// enum RTLD_LOCAL = 0; // POSIX
enum RTLD_PARENT   = 0x00200;
enum RTLD_GROUP    = 0x00400;
enum RTLD_WORLD    = 0x00800;
enum RTLD_NODELETE = 0x01000;
enum RTLD_FIRST    = 0x02000;
enum RTLD_CONFGEN  = 0x10000;


enum
{
    RTLD_NEXT    = cast(void *)-1,
    RTLD_DEFAULT = cast(void *)-2,
    RTLD_SELF    = cast(void *)-3,
    RTLD_PROBE   = cast(void *)-4,
}

alias c_ulong Lmid_t;

void* dlmopen(Lmid_t, const scope char*, int);

enum
{
    RTLD_REL_RELATIVE = 0x00001,
    RTLD_REL_EXEC     = 0x00002,
    RTLD_REL_DEPENDS  = 0x00004,
    RTLD_REL_PRELOAD  = 0x00008,
    RTLD_REL_SELF     = 0x00010,
    RTLD_REL_WEAK     = 0x00020,
    RTLD_REL_ALL      = 0x00fff,
    RTLD_MEMORY       = 0x01000,
    RTLD_STRIP        = 0x02000,
    RTLD_NOHEAP       = 0x04000,
    RTLD_CONFSET      = 0x10000,
}

int dldump(const scope char*, const scope char*, int);

struct Dl_info
{
    const(char)* dli_fname;
    void*        dli_fbase;
    const(char)* dli_sname;
    void*        dli_saddr;
}

enum
{
    RTLD_DL_SYMENT = 1,
    RTLD_DL_LINKMAP = 2,
}

int dladdr(const(void)*, Dl_info*);
int dladdr1(void*, Dl_info*, void**, int);

enum
{
    RTLD_DI_LMID         = 1,
    RTLD_DI_LINKMAP      = 2,
    RTLD_DI_CONFIGADDR   = 3,
    RTLD_DI_SERINFO      = 4,
    RTLD_DI_SERINFOSIZE  = 5,
    RTLD_DI_ORIGIN       = 6,
    RTLD_DI_PROFILENAME  = 7,
    RTLD_DI_PROFILEOUT   = 8,
    RTLD_DI_GETSIGNAL    = 9,
    RTLD_DI_SETSIGNAL    = 10,
    RTLD_DI_ARGSINFO     = 11,
    RTLD_DI_MMAPS        = 12,
    RTLD_DI_MMAPCNT      = 13,
    RTLD_DI_DEFERRED     = 14,
    RTLD_DI_DEFERRED_SYM = 15,
    RTLD_DI_MAX          = 15,
}

int dlinfo(void*, int, void*);

struct Dl_serpath
{
    char*  dls_name;
    uint   dls_flags;
}

struct Dl_serinfo
{
    size_t         dls_size;
    uint           dls_cnt;
    Dl_serpath[1]  dls_serpath;
}

// FIXME: Dl_argsinfo, Dl_mapinfo, Dl_amd64_unwindinfo are missing
