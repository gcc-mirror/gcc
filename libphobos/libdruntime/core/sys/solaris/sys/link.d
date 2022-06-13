/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/link.h, illumos sys/link.h)
 */
module core.sys.solaris.sys.link;

version (Solaris):
extern (C):
nothrow:

public import core.sys.solaris.sys.elftypes;
public import core.sys.elf;
import core.stdc.config;

struct Elf32_Dyn
{
    Elf32_Sword d_tag;
    union _d_un
    {
        Elf32_Word d_val;
        Elf32_Addr d_ptr;
        Elf32_Off  d_off;
    } _d_un d_un;
}

struct Elf64_Dyn
{
    Elf64_Xword d_tag;
    union _d_un
    {
        Elf64_Xword d_val;
        Elf64_Addr  d_ptr;
    } _d_un d_un;
}

enum DT_MAXPOSTAGS = 34;

enum DT_SUNW_AUXILIARY = 0x6000000d;
enum DT_SUNW_RTLDINF   = 0x6000000e;
enum DT_SUNW_FILTER    = 0x6000000f;
enum DT_SUNW_CAP       = 0x60000010;
enum DT_SUNW_SYMTAB    = 0x60000011;
enum DT_SUNW_SYMSZ     = 0x60000012;

enum DT_SUNW_ENCODING    = 0x60000013;
enum DT_SUNW_SORTENT     = 0x60000013;
enum DT_SUNW_SYMSORT     = 0x60000014;
enum DT_SUNW_SYMSORTSZ   = 0x60000015;
enum DT_SUNW_TLSSORT     = 0x60000016;
enum DT_SUNW_TLSSORTSZ   = 0x60000017;
enum DT_SUNW_CAPINFO     = 0x60000018;
enum DT_SUNW_STRPAD      = 0x60000019;
enum DT_SUNW_CAPCHAIN    = 0x6000001a;
enum DT_SUNW_LDMACH      = 0x6000001b;
enum DT_SUNW_CAPCHAINENT = 0x6000001d;
enum DT_SUNW_CAPCHAINSZ  = 0x6000001f;

enum DT_DEPRECATED_SPARC_REGISTER = 0x7000001;

enum DT_USED      = 0x7ffffffe;

enum DF_P1_DEFERRED  = 0x00000004;

enum VER_FLG_INFO = 0x4;

enum SYMINFO_FLG_FILTER      = 0x0002;
enum SYMINFO_FLG_DIRECTBIND  = 0x0010;
enum SYMINFO_FLG_NOEXTDIRECT = 0x0020;
enum SYMINFO_FLG_AUXILIARY   = 0x0040;
enum SYMINFO_FLG_INTERPOSE   = 0x0080;
enum SYMINFO_FLG_CAP         = 0x0100;
enum SYMINFO_FLG_DEFERRED    = 0x0200;

enum SYMINFO_BT_NONE       = 0xfffd;
enum SYMINFO_BT_EXTERN     = 0xfffc;

alias link_map Link_map;

struct link_map
{
    c_ulong  l_addr;
    char*    l_name;
    version (D_LP64)
        Elf64_Dyn*  l_ld;
    else
        Elf32_Dyn*  l_ld;
    Link_map*  l_next;
    Link_map*  l_prev;
    char*      l_refname;
}

version (_SYSCALL32)
{
alias link_map32 Link_map32;

struct link_map32
{
    Elf32_Word  l_addr;
    Elf32_Addr  l_name;
    Elf32_Addr  l_ld;
    Elf32_Addr  l_next;
    Elf32_Addr  l_prev;
    Elf32_Addr  l_refname;
}
}

enum r_state_e
{
    RT_CONSISTENT,
    RT_ADD,
    RT_DELETE
}

enum rd_flags_e
{
    RD_FL_NONE = 0,
    RD_FL_ODBG = (1<<0),
    RD_FL_DBG  = (1<<1)
}

enum rd_event_e
{
    RD_NONE = 0,
    RD_PREINIT,
    RD_POSTINIT,
    RD_DLACTIVITY
}

struct r_debug
{
    int         r_version;
    Link_map*   r_map;
    c_ulong     r_brk;
    r_state_e   r_state;
    c_ulong     r_ldbase;
    Link_map*   r_ldsomap;
    rd_event_e  r_rdevent;
    rd_flags_e  r_flags;
}

version (_SYSCALL32)
{
struct r_debug32
{
    Elf32_Word  r_version;
    Elf32_Addr  r_map;
    Elf32_Word  r_brk;
    r_state_e   r_state;
    Elf32_Word  r_ldbase;
    Elf32_Addr  r_ldsomap;
    rd_event_e  r_rdevent;
    rd_flags_e  r_flags;
}
}

enum R_DEBUG_VERSION = 2;

struct Elf32_Boot
{
    Elf32_Sword eb_tag;
    union eb_un
    {
        Elf32_Word eb_val;
        Elf32_Addr eb_ptr;
        Elf32_Off  eb_off;
    }
}

struct Elf64_Boot
{
    Elf64_Xword eb_tag;
    union eb_un
    {
        Elf64_Xword eb_val;
        Elf64_Addr eb_ptr;
        Elf64_Off eb_off;
    }
}

enum EB_NULL       = 0;
enum EB_DYNAMIC    = 1;
enum EB_LDSO_BASE  = 2;
enum EB_ARGV       = 3;
enum EB_ENVP       = 4;
enum EB_AUXV       = 5;
enum EB_DEVZERO    = 6;
enum EB_PAGESIZE   = 7;
enum EB_MAX        = 8;
enum EB_MAX_SIZE32 = 64;
enum EB_MAX_SIZE64 = 128;

void _ld_libc(void *);
