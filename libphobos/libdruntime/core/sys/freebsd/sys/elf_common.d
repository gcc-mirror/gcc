/**
 * D header file for FreeBSD.
 *
 * $(LINK2 http://svnweb.freebsd.org/base/head/sys/sys/elf_common.h?view=markup, sys/elf_common.h)
 */
module core.sys.freebsd.sys.elf_common;

version (FreeBSD):
extern (C):
pure:
nothrow:

import core.stdc.stdint;
public import core.sys.elf;

struct Elf_Note
{
    uint32_t      n_namesz;
    uint32_t      n_descsz;
    uint32_t      n_type;
}

struct Elf_GNU_Hash_Header
{
    uint32_t      gh_nbuckets;
    uint32_t      gh_symndx;
    uint32_t      gh_maskwords;
    uint32_t      gh_shift2;
}

enum OLD_EI_BRAND =    8;

extern (D)
{
    auto IS_ELF(T)(T ehdr) { return ehdr.e_ident[EI_MAG0] == ELFMAG0 &&
                                    ehdr.e_ident[EI_MAG1] == ELFMAG1 &&
                                    ehdr.e_ident[EI_MAG2] == ELFMAG2 &&
                                    ehdr.e_ident[EI_MAG3] == ELFMAG3; }
}

enum EM_486 =           6;
enum EM_MIPS_RS4_BE =  10;
enum EM_ALPHA_STD =    41;
enum SHT_SUNW_dof =      0x6ffffff4;
enum SHT_SUNW_cap =      0x6ffffff5;
enum SHT_SUNW_SIGNATURE = 0x6ffffff6;
enum SHT_SUNW_verdef =   0x6ffffffd;
enum SHT_SUNW_verneed =  0x6ffffffe;
enum SHT_SUNW_versym =   0x6fffffff;

enum PT_SUNWDTRACE =   0x6ffffffc;
enum PT_SUNWCAP =      0x6ffffffd;

enum DT_MAXPOSTAGS =   34;
enum DT_SUNW_AUXILIARY = 0x6000000d;
enum DT_SUNW_RTLDINF = 0x6000000e;
enum DT_SUNW_FILTER =  0x6000000f;
enum DT_SUNW_CAP =     0x60000010;
enum DT_DEPRECATED_SPARC_REGISTER = 0x7000001;
enum DT_USED =         0x7ffffffe;

enum DF_1_BIND_NOW =   0x00000001;

enum NT_PRSTATUS =     1;
enum NT_FPREGSET =     2;
enum NT_PRPSINFO =     3;
enum NT_THRMISC =      7;
enum NT_PROCSTAT_PROC = 8;
enum NT_PROCSTAT_FILES = 9;
enum NT_PROCSTAT_VMMAP = 10;
enum NT_PROCSTAT_GROUPS = 11;
enum NT_PROCSTAT_UMASK = 12;
enum NT_PROCSTAT_RLIMIT = 13;
enum NT_PROCSTAT_OSREL = 14;
enum NT_PROCSTAT_PSSTRINGS = 15;
enum NT_PROCSTAT_AUXV = 16;

enum STV_EXPORTED =    4;
enum STV_SINGLETON =   5;
enum STV_ELIMINATE =   6;

alias VER_NDX VER_DEF_IDX;

enum VER_NEED_WEAK =    32768;
enum VER_NEED_HIDDEN = VER_NDX_HIDDEN;
alias VER_NDX VER_NEED_IDX;

enum VER_NDX_GIVEN =           2;
enum VER_NDX_HIDDEN =      32768;

extern (D)
{
    auto VER_NDX(V)(V v) { return v & ~(1u << 15); }
}

enum CA_SUNW_NULL =    0;
enum CA_SUNW_HW_1 =    1;
enum CA_SUNW_SF_1 =    2;

enum SYMINFO_FLG_DIRECTBIND =  0x0010;
enum SYMINFO_FLG_NOEXTDIRECT = 0x0020;
enum SYMINFO_FLG_FILTER =      0x0002;
enum SYMINFO_FLG_AUXILIARY =   0x0040;

enum SYMINFO_BT_NONE =         0xfffd;
enum SYMINFO_BT_EXTERN =       0xfffc;

version (PPC64)
{
    enum __ELF_WORD_SIZE = 64;
    enum ELF_ARCH = EM_PPC64;
    enum ELF_MACHINE_OK(x) = ((x) == EM_PPC64);
}
version (PPC)
{
    enum __ELF_WORD_SIZE = 32;
    enum ELF_ARCH = EM_PPC;
    enum ELF_ARCH32 = EM_PPC;
    enum ELF_MACHINE_OK(x) = ((x) == EM_PPC);
}

struct Elf32_Auxinfo
{
    int a_type;
    union a_un
    {
        long    a_val;
        void    *a_ptr;
        void function(void* a) a_fcn;
    }
}

struct Elf64_Auxinfo
{
    long a_type;
    union a_un
    {
        long    a_val;
        void    *a_ptr;
        void function(void* a) a_fcn;
    }
}

enum AT_NULL   = 0;
enum AT_IGNORE = 1;
enum AT_EXECFD = 2;
enum AT_PHDR  = 3;
enum AT_PHENT = 4;
enum AT_PHNUM = 5;
enum AT_PAGESZ = 6;
enum AT_BASE  = 7;
enum AT_FLAGS = 8;
enum AT_ENTRY = 9;
enum AT_DCACHEBSIZE = 10;
enum AT_ICACHEBSIZE = 11;
enum AT_UCACHEBSIZE = 12;
enum AT_EXECPATH  = 13;
enum AT_CANARY    = 14;
enum AT_CANARYLEN = 15;
enum AT_OSRELDATE = 16;
enum AT_NCPUS = 17;
enum AT_PAGESIZES = 18;
enum AT_PAGESIZESLEN = 19;
enum AT_STACKPROT = 21;
enum AT_TIMEKEEP  = 22;
enum AT_COUNT = 23;

enum R_PPC_COUNT =  37;
enum R_PPC_EMB_COUNT =  (R_PPC_EMB_RELSDA - R_PPC_EMB_NADDR32 + 1);

version (PPC64)
{
    enum ELF_TARG_CLASS =   ELFCLASS64;
    enum ELF_TARG_DATA =    ELFDATA2MSB;
    enum ELF_TARG_MACH =    EM_PPC64;
    enum ELF_TARG_VER =     1;
}

version (PPC)
{
    enum ELF_TARG_CLASS =   ELFCLASS32;
    enum ELF_TARG_DATA =    ELFDATA2MSB;
    enum ELF_TARG_MACH =    EM_PPC;
    enum ELF_TARG_VER =     1;
}

enum ET_DYN_LOAD_ADDR = 0x01010000;
