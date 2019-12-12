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

enum DT_NULL         = 0;
enum DT_NEEDED       = 1;
enum DT_PLTRELSZ     = 2;
enum DT_PLTGOT       = 3;
enum DT_HASH         = 4;
enum DT_STRTAB       = 5;
enum DT_SYMTAB       = 6;
enum DT_RELA         = 7;
enum DT_RELASZ       = 8;
enum DT_RELAENT      = 9;
enum DT_STRSZ        = 10;
enum DT_SYMENT       = 11;
enum DT_INIT         = 12;
enum DT_FINI         = 13;
enum DT_SONAME       = 14;
enum DT_RPATH        = 15;
enum DT_SYMBOLIC     = 16;
enum DT_REL          = 17;
enum DT_RELSZ        = 18;
enum DT_RELENT       = 19;
enum DT_PLTREL       = 20;
enum DT_DEBUG        = 21;
enum DT_TEXTREL      = 22;
enum DT_JMPREL       = 23;
enum DT_BIND_NOW     = 24;
enum DT_INIT_ARRAY   = 25;
enum DT_FINI_ARRAY   = 26;
enum DT_INIT_ARRAYSZ = 27;
enum DT_FINI_ARRAYSZ = 28;
enum DT_RUNPATH      = 29;
enum DT_FLAGS        = 30;

enum DT_ENCODING        = 32;
enum DT_PREINIT_ARRAY   = 32;
enum DT_PREINIT_ARRAYSZ = 33;

enum DT_MAXPOSTAGS = 34;

enum DT_LOOS           = 0x6000000d;
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

enum DT_HIOS = 0x6ffff000;

enum DT_DEPRECATED_SPARC_REGISTER = 0x7000001;

enum DT_VALRNGLO = 0x6ffffd00;

enum DT_GNU_PRELINKED  = 0x6ffffdf5;
enum DT_GNU_CONFLICTSZ = 0x6ffffdf6;
enum DT_GNU_LIBLISTSZ  = 0x6ffffdf7;
enum DT_CHECKSUM       = 0x6ffffdf8;
enum DT_PLTPADSZ       = 0x6ffffdf9;
enum DT_MOVEENT        = 0x6ffffdfa;
enum DT_MOVESZ         = 0x6ffffdfb;
enum DT_FEATURE_1      = 0x6ffffdfc;
enum DT_POSFLAG_1      = 0x6ffffdfd;
enum DT_SYMINSZ        = 0x6ffffdfe;
enum DT_SYMINENT       = 0x6ffffdff;
enum DT_VALRNGHI       = 0x6ffffdff;

enum DT_ADDRRNGLO = 0x6ffffe00;

enum DT_GNU_HASH     = 0x6ffffef5;
enum DT_TLSDESC_PLT  = 0x6ffffef6;
enum DT_TLSDESC_GOT  = 0x6ffffef7;
enum DT_GNU_CONFLICT = 0x6ffffef8;
enum DT_GNU_LIBLIST  = 0x6ffffef9;

enum DT_CONFIG    = 0x6ffffefa;
enum DT_DEPAUDIT  = 0x6ffffefb;
enum DT_AUDIT     = 0x6ffffefc;
enum DT_PLTPAD    = 0x6ffffefd;
enum DT_MOVETAB   = 0x6ffffefe;
enum DT_SYMINFO   = 0x6ffffeff;
enum DT_ADDRRNGHI = 0x6ffffeff;

enum DT_VERSYM = 0x6ffffff0;

enum DT_RELACOUNT  = 0x6ffffff9;
enum DT_RELCOUNT   = 0x6ffffffa;
enum DT_FLAGS_1    = 0x6ffffffb;
enum DT_VERDEF     = 0x6ffffffc;
enum DT_VERDEFNUM  = 0x6ffffffd;
enum DT_VERNEED    = 0x6ffffffe;
enum DT_VERNEEDNUM = 0x6fffffff;

enum DT_LOPROC    = 0x70000000;
enum DT_AUXILIARY = 0x7ffffffd;
enum DT_USED      = 0x7ffffffe;
enum DT_FILTER    = 0x7fffffff;
enum DT_HIPROC    = 0x7fffffff;

enum DF_ORIGIN     = 0x00000001;
enum DF_SYMBOLIC   = 0x00000002;
enum DF_TEXTREL    = 0x00000004;
enum DF_BIND_NOW   = 0x00000008;
enum DF_STATIC_TLS = 0x00000010;

enum DF_P1_LAZYLOAD  = 0x00000001;
enum DF_P1_GROUPPERM = 0x00000002;
enum DF_P1_DEFERRED  = 0x00000004;

enum DF_1_NOW        = 0x00000001;
enum DF_1_GLOBAL     = 0x00000002;
enum DF_1_GROUP      = 0x00000004;
enum DF_1_NODELETE   = 0x00000008;
enum DF_1_LOADFLTR   = 0x00000010;
enum DF_1_INITFIRST  = 0x00000020;
enum DF_1_NOOPEN     = 0x00000040;
enum DF_1_ORIGIN     = 0x00000080;
enum DF_1_DIRECT     = 0x00000100;
enum DF_1_TRANS      = 0x00000200;
enum DF_1_INTERPOSE  = 0x00000400;
enum DF_1_NODEFLIB   = 0x00000800;
enum DF_1_NODUMP     = 0x00001000;
enum DF_1_CONFALT    = 0x00002000;
enum DF_1_ENDFILTEE  = 0x00004000;
enum DF_1_DISPRELDNE = 0x00008000;
enum DF_1_DISPRELPND = 0x00010000;
enum DF_1_NODIRECT   = 0x00020000;
enum DF_1_IGNMULDEF  = 0x00040000;
enum DF_1_NOKSYMS    = 0x00080000;
enum DF_1_NOHDR      = 0x00100000;
enum DF_1_EDITED     = 0x00200000;
enum DF_1_NORELOC    = 0x00400000;
enum DF_1_SYMINTPOSE = 0x00800000;
enum DF_1_GLOBAUDIT  = 0x01000000;
enum DF_1_SINGLETON  = 0x02000000;

enum DTF_1_PARINIT = 0x00000001;
enum DTF_1_CONFEXP = 0x00000002;

struct Elf32_Verdef
{
    Elf32_Half  vd_version;
    Elf32_Half  vd_flags;
    Elf32_Half  vd_ndx;
    Elf32_Half  vd_cnt;
    Elf32_Word  vd_hash;
    Elf32_Word  vd_aux;
    Elf32_Word  vd_next;
}

struct Elf32_Verdaux
{
    Elf32_Word  vda_name;
    Elf32_Word  vda_next;
}

struct Elf32_Verneed
{
    Elf32_Half  vn_version;
    Elf32_Half  vn_cnt;
    Elf32_Word  vn_file;
    Elf32_Word  vn_aux;
    Elf32_Word  vn_next;
}

struct Elf32_Vernaux
{
    Elf32_Word  vna_hash;
    Elf32_Half  vna_flags;
    Elf32_Half  vna_other;
    Elf32_Word  vna_name;
    Elf32_Word  vna_next;
}

alias Elf32_Half  Elf32_Versym;

struct Elf32_Syminfo
{
    Elf32_Half  si_boundto;
    Elf32_Half  si_flags;
}

struct Elf64_Verdef
{
    Elf64_Half  vd_version;
    Elf64_Half  vd_flags;
    Elf64_Half  vd_ndx;
    Elf64_Half  vd_cnt;
    Elf64_Word  vd_hash;
    Elf64_Word  vd_aux;
    Elf64_Word  vd_next;
}

struct Elf64_Verdaux
{
    Elf64_Word  vda_name;
    Elf64_Word  vda_next;
}

struct Elf64_Verneed
{
    Elf64_Half  vn_version;
    Elf64_Half  vn_cnt;
    Elf64_Word  vn_file;
    Elf64_Word  vn_aux;
    Elf64_Word  vn_next;
}

struct Elf64_Vernaux
{
    Elf64_Word  vna_hash;
    Elf64_Half  vna_flags;
    Elf64_Half  vna_other;
    Elf64_Word  vna_name;
    Elf64_Word  vna_next;
}

alias Elf64_Half  Elf64_Versym;

struct Elf64_Syminfo
{
    Elf64_Half  si_boundto;
    Elf64_Half  si_flags;
}

enum VER_NDX_LOCAL     = 0;
enum VER_NDX_GLOBAL    = 1;
enum VER_NDX_LORESERVE = 0xff00;
enum VER_NDX_ELIMINATE = 0xff01;

enum VER_FLG_BASE = 0x1;
enum VER_FLG_WEAK = 0x2;
enum VER_FLG_INFO = 0x4;

enum VER_DEF_NONE    = 0;
enum VER_DEF_CURRENT = 1;
enum VER_DEF_NUM     = 2;

enum VER_NEED_NONE    = 0;
enum VER_NEED_CURRENT = 1;
enum VER_NEED_NUM     = 2;

enum SYMINFO_FLG_DIRECT      = 0x0001;
enum SYMINFO_FLG_FILTER      = 0x0002;
enum SYMINFO_FLG_PASSTHRU    = SYMINFO_FLG_FILTER;
enum SYMINFO_FLG_COPY        = 0x0004;
enum SYMINFO_FLG_LAZYLOAD    = 0x0008;
enum SYMINFO_FLG_DIRECTBIND  = 0x0010;
enum SYMINFO_FLG_NOEXTDIRECT = 0x0020;
enum SYMINFO_FLG_AUXILIARY   = 0x0040;
enum SYMINFO_FLG_INTERPOSE   = 0x0080;
enum SYMINFO_FLG_CAP         = 0x0100;
enum SYMINFO_FLG_DEFERRED    = 0x0200;

enum SYMINFO_BT_SELF       = 0xffff;
enum SYMINFO_BT_PARENT     = 0xfffe;
enum SYMINFO_BT_NONE       = 0xfffd;
enum SYMINFO_BT_EXTERN     = 0xfffc;
enum SYMINFO_BT_LOWRESERVE = 0xff00;

enum SYMINFO_NONE    = 0;
enum SYMINFO_CURRENT = 1;
enum SYMINFO_NUM     = 2;

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
