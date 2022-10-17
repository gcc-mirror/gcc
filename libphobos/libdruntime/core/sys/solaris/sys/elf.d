/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_386.h, illumos sys/elf_386.h)
 */
module core.sys.solaris.sys.elf;

version (Solaris):
extern (C):
nothrow:

public import core.sys.elf;

enum ELF32_FSZ_ADDR  = 4;
enum ELF32_FSZ_HALF  = 2;
enum ELF32_FSZ_OFF   = 4;
enum ELF32_FSZ_SWORD = 4;
enum ELF32_FSZ_WORD  = 4;

enum ELF64_FSZ_ADDR   = 8;
enum ELF64_FSZ_HALF   = 2;
enum ELF64_FSZ_OFF    = 8;
enum ELF64_FSZ_SWORD  = 4;
enum ELF64_FSZ_WORD   = 4;
enum ELF64_FSZ_SXWORD = 8;
enum ELF64_FSZ_XWORD  = 8;

enum ET_LOSUNW     = 0xfeff;
enum ET_SUNWPSEUDO = 0xfeff;
enum ET_HISUNW     = 0xfeff;

enum EAV_SUNW_NONE    = 0;
enum EAV_SUNW_CURRENT = 1;
enum EAV_SUNW_NUM     = 2;

enum PT_SUNW_UNWIND   = 0x6464e550;
enum PT_SUNW_EH_FRAME = 0x6474e550;

enum PT_SUNWDTRACE = 0x6ffffffc;
enum PT_SUNWCAP    = 0x6ffffffd;

enum PF_SUNW_FAILURE = 0x00100000;
enum PF_SUNW_KILLED  = 0x00200000;
enum PF_SUNW_SIGINFO = 0x00400000;

enum SHT_SUNW_capchain  = 0x6fffffef;
enum SHT_SUNW_capinfo   = 0x6ffffff0;
enum SHT_SUNW_symsort   = 0x6ffffff1;
enum SHT_SUNW_tlssort   = 0x6ffffff2;
enum SHT_SUNW_LDYNSYM   = 0x6ffffff3;
enum SHT_SUNW_dof       = 0x6ffffff4;
enum SHT_SUNW_cap       = 0x6ffffff5;
enum SHT_SUNW_SIGNATURE = 0x6ffffff6;
enum SHT_SUNW_ANNOTATE  = 0x6ffffff7;
enum SHT_SUNW_DEBUGSTR  = 0x6ffffff8;
enum SHT_SUNW_DEBUG     = 0x6ffffff9;
enum SHT_SUNW_verdef    = 0x6ffffffd;
enum SHT_SUNW_verneed   = 0x6ffffffe;
enum SHT_SUNW_versym    = 0x6fffffff;

extern (D)
{
    auto ELF32_ST_VISIBILITY(O)(O o) { return o & 0x07; }
    alias ELF32_ST_VISIBILITY ELF64_ST_VISIBILITY;
}

enum STV_EXPORTED  = 4;
enum STV_SINGLETON = 5;
enum STV_ELIMINATE = 6;
enum STV_NUM       = 7;

extern (D)
{
    auto ELF64_R_TYPE_DATA(I)(I i) { return (i << 32) >> 40; }
    auto ELF64_R_TYPE_ID(I)(I i) { return (i << 56) >> 56; }
    auto ELF64_R_TYPE_INFO(S, T)(S sym, T type) { return (sym <<8) + (type); }
}

struct Elf32_Nhdr
{
    Elf32_Word n_namesz;
    Elf32_Word n_descsz;
    Elf32_Word n_type;
}

struct Elf64_Nhdr
{
    Elf64_Word n_namesz;
    Elf64_Word n_descsz;
    Elf64_Word n_type;
}

struct Elf32_Cap
{
    Elf32_Word  c_tag;
    union c_un
    {
        Elf32_Word  c_val;
        Elf32_Addr  c_ptr;
    }
}

alias Elf32_Word Elf32_Capinfo;
alias Elf32_Word Elf32_Capchain;

alias ELF32_M_SYM ELF32_C_SYM;
alias ELF32_M_SIZE ELF32_C_GROUP;
alias ELF32_M_INFO ELF32_C_INFO;

struct Elf64_Cap
{
    Elf64_Xword c_tag;
    union c_un
    {
        Elf64_Xword c_val;
        Elf64_Addr  c_ptr;
    }
}

alias Elf64_Xword Elf64_Capinfo;
alias Elf64_Word  Elf64_Capchain;

extern (D)
{
    auto ELF64_C_SYM(I)(I info) { return info >> 32; }
    auto ELF64_C_GROUP(I)(I info) { return cast(Elf64_Word)info; }
    auto ELF64_C_INFO(S, G)(S sym, G grp) { return (sym << 32) + grp; }
}

enum CAPINFO_NONE    = 0;
enum CAPINFO_CURRENT = 1;
enum CAPINFO_NUM     = 2;

enum CAPCHAIN_NONE    = 0;
enum CAPCHAIN_CURRENT = 1;
enum CAPCHAIN_NUM     = 2;

enum CAPINFO_SUNW_GLOB = 0xff;

enum CA_SUNW_NULL = 0;
enum CA_SUNW_HW_1 = 1;
enum CA_SUNW_SF_1 = 2;
enum CA_SUNW_HW_2 = 3;
enum CA_SUNW_PLAT = 4;
enum CA_SUNW_MACH = 5;
enum CA_SUNW_ID   = 6;
enum CA_SUNW_NUM  = 7;

enum SF1_SUNW_FPKNWN = 0x001;
enum SF1_SUNW_FPUSED = 0x002;
enum SF1_SUNW_ADDR32 = 0x004;
enum SF1_SUNW_MASK   = 0x007;

enum NT_PRSTATUS   = 1;
enum NT_PRFPREG    = 2;
enum NT_PRPSINFO   = 3;
enum NT_PRXREG     = 4;
enum NT_PLATFORM   = 5;
enum NT_AUXV       = 6;
enum NT_GWINDOWS   = 7;
enum NT_ASRS       = 8;
enum NT_LDT        = 9;
enum NT_PSTATUS    = 10;
enum NT_PSINFO     = 13;
enum NT_PRCRED     = 14;
enum NT_UTSNAME    = 15;
enum NT_LWPSTATUS  = 16;
enum NT_LWPSINFO   = 17;
enum NT_PRPRIV     = 18;
enum NT_PRPRIVINFO = 19;
enum NT_CONTENT    = 20;
enum NT_ZONENAME   = 21;
enum NT_FDINFO     = 22;
enum NT_SPYMASTER  = 23;
enum NT_NUM        = 23;
