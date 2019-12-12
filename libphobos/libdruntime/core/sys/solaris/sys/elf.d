/**
 * D header file for Solaris.
 *
 * $(LINK2 http://src.illumos.org/source/xref/illumos-gate/usr/src/uts/common/sys/elf_386.h, illumos sys/elf_386.h)
 */
module core.sys.solaris.sys.elf;

version (Solaris):
extern (C):
nothrow:

public import core.sys.solaris.sys.elftypes;

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

enum EI_NIDENT = 16;

struct Elf32_Ehdr
{
    char[EI_NIDENT] e_ident = 0;
    Elf32_Half    e_type;
    Elf32_Half    e_machine;
    Elf32_Word    e_version;
    Elf32_Addr    e_entry;
    Elf32_Off     e_phoff;
    Elf32_Off     e_shoff;
    Elf32_Word    e_flags;
    Elf32_Half    e_ehsize;
    Elf32_Half    e_phentsize;
    Elf32_Half    e_phnum;
    Elf32_Half    e_shentsize;
    Elf32_Half    e_shnum;
    Elf32_Half    e_shstrndx;
}

struct Elf64_Ehdr
{
    char[EI_NIDENT] e_ident = 0;
    Elf64_Half    e_type;
    Elf64_Half    e_machine;
    Elf64_Word    e_version;
    Elf64_Addr    e_entry;
    Elf64_Off     e_phoff;
    Elf64_Off     e_shoff;
    Elf64_Word    e_flags;
    Elf64_Half    e_ehsize;
    Elf64_Half    e_phentsize;
    Elf64_Half    e_phnum;
    Elf64_Half    e_shentsize;
    Elf64_Half    e_shnum;
    Elf64_Half    e_shstrndx;
}

enum EI_MAG0       = 0;
enum EI_MAG1       = 1;
enum EI_MAG2       = 2;
enum EI_MAG3       = 3;
enum EI_CLASS      = 4;
enum EI_DATA       = 5;
enum EI_VERSION    = 6;
enum EI_OSABI      = 7;
enum EI_ABIVERSION = 8;
enum EI_PAD        = 9;

enum ELFMAG0 = 0x7f;
enum ELFMAG1 = 'E';
enum ELFMAG2 = 'L';
enum ELFMAG3 = 'F';
enum ELFMAG  = "\177ELF";
enum SELFMAG = 4;

enum ELFCLASSNONE = 0;
enum ELFCLASS32   = 1;
enum ELFCLASS64   = 2;
enum ELFCLASSNUM  = 3;

enum ELFDATANONE = 0;
enum ELFDATA2LSB = 1;
enum ELFDATA2MSB = 2;
enum ELFDATANUM  = 3;

enum ET_NONE       = 0;
enum ET_REL        = 1;
enum ET_EXEC       = 2;
enum ET_DYN        = 3;
enum ET_CORE       = 4;
enum ET_NUM        = 5;
enum ET_LOOS       = 0xfe00;
enum ET_LOSUNW     = 0xfeff;
enum ET_SUNWPSEUDO = 0xfeff;
enum ET_HISUNW     = 0xfeff;
enum ET_HIOS       = 0xfeff;
enum ET_LOPROC     = 0xff00;
enum ET_HIPROC     = 0xffff;

enum EM_NONE        = 0;
enum EM_M32         = 1;
enum EM_SPARC       = 2;
enum EM_386         = 3;
enum EM_68K         = 4;
enum EM_88K         = 5;
enum EM_486         = 6;
enum EM_860         = 7;
enum EM_MIPS        = 8;
enum EM_S370        = 9;
enum EM_MIPS_RS3_LE = 10;
enum EM_RS6000      = 11;
enum EM_UNKNOWN12   = 12;
enum EM_UNKNOWN13   = 13;
enum EM_UNKNOWN14   = 14;
enum EM_PA_RISC     = 15;
enum EM_PARISC      = EM_PA_RISC;
enum EM_nCUBE       = 16;
enum EM_VPP500      = 17;
enum EM_SPARC32PLUS = 18;
enum EM_960         = 19;
enum EM_PPC         = 20;
enum EM_PPC64       = 21;
enum EM_S390        = 22;
enum EM_UNKNOWN22   = EM_S390;
enum EM_UNKNOWN23   = 23;
enum EM_UNKNOWN24   = 24;
enum EM_UNKNOWN25   = 25;
enum EM_UNKNOWN26   = 26;
enum EM_UNKNOWN27   = 27;
enum EM_UNKNOWN28   = 28;
enum EM_UNKNOWN29   = 29;
enum EM_UNKNOWN30   = 30;
enum EM_UNKNOWN31   = 31;
enum EM_UNKNOWN32   = 32;
enum EM_UNKNOWN33   = 33;
enum EM_UNKNOWN34   = 34;
enum EM_UNKNOWN35   = 35;
enum EM_V800        = 36;
enum EM_FR20        = 37;
enum EM_RH32        = 38;
enum EM_RCE         = 39;
enum EM_ARM         = 40;
enum EM_ALPHA       = 41;
enum EM_SH          = 42;
enum EM_SPARCV9     = 43;
enum EM_TRICORE     = 44;
enum EM_ARC         = 45;
enum EM_H8_300      = 46;
enum EM_H8_300H     = 47;
enum EM_H8S         = 48;
enum EM_H8_500      = 49;
enum EM_IA_64       = 50;
enum EM_MIPS_X      = 51;
enum EM_COLDFIRE    = 52;
enum EM_68HC12      = 53;
enum EM_MMA         = 54;
enum EM_PCP         = 55;
enum EM_NCPU        = 56;
enum EM_NDR1        = 57;
enum EM_STARCORE    = 58;
enum EM_ME16        = 59;
enum EM_ST100       = 60;
enum EM_TINYJ       = 61;
enum EM_AMD64       = 62;
enum EM_X86_64      = EM_AMD64;
enum EM_PDSP        = 63;
enum EM_UNKNOWN64   = 64;
enum EM_UNKNOWN65   = 65;
enum EM_FX66        = 66;
enum EM_ST9PLUS     = 67;
enum EM_ST7         = 68;
enum EM_68HC16      = 69;
enum EM_68HC11      = 70;
enum EM_68HC08      = 71;
enum EM_68HC05      = 72;
enum EM_SVX         = 73;
enum EM_ST19        = 74;
enum EM_VAX         = 75;
enum EM_CRIS        = 76;
enum EM_JAVELIN     = 77;
enum EM_FIREPATH    = 78;
enum EM_ZSP         = 79;
enum EM_MMIX        = 80;
enum EM_HUANY       = 81;
enum EM_PRISM       = 82;
enum EM_AVR         = 83;
enum EM_FR30        = 84;
enum EM_D10V        = 85;
enum EM_D30V        = 86;
enum EM_V850        = 87;
enum EM_M32R        = 88;
enum EM_MN10300     = 89;
enum EM_MN10200     = 90;
enum EM_PJ          = 91;
enum EM_OPENRISC    = 92;
enum EM_ARC_A5      = 93;
enum EM_XTENSA      = 94;
enum EM_NUM         = 95;

enum EV_NONE    = 0;
enum EV_CURRENT = 1;
enum EV_NUM     = 2;


enum ELFOSABI_NONE        = 0;
enum ELFOSABI_SYSV        = ELFOSABI_NONE;
enum ELFOSABI_HPUX        = 1;
enum ELFOSABI_NETBSD      = 2;
enum ELFOSABI_LINUX       = 3;
enum ELFOSABI_UNKNOWN4    = 4;
enum ELFOSABI_UNKNOWN5    = 5;
enum ELFOSABI_SOLARIS     = 6;
enum ELFOSABI_AIX         = 7;
enum ELFOSABI_IRIX        = 8;
enum ELFOSABI_FREEBSD     = 9;
enum ELFOSABI_TRU64       = 10;
enum ELFOSABI_MODESTO     = 11;
enum ELFOSABI_OPENBSD     = 12;
enum ELFOSABI_OPENVMS     = 13;
enum ELFOSABI_NSK         = 14;
enum ELFOSABI_AROS        = 15;
enum ELFOSABI_ARM         = 97;
enum ELFOSABI_STANDALONE  = 255;
enum ELFOSABI_DRAGONFLYBSD= ELFOSABI_NONE;

enum EAV_SUNW_NONE    = 0;
enum EAV_SUNW_CURRENT = 1;
enum EAV_SUNW_NUM     = 2;

struct Elf32_Phdr
{
    Elf32_Word    p_type;
    Elf32_Off     p_offset;
    Elf32_Addr    p_vaddr;
    Elf32_Addr    p_paddr;
    Elf32_Word    p_filesz;
    Elf32_Word    p_memsz;
    Elf32_Word    p_flags;
    Elf32_Word    p_align;
}

struct Elf64_Phdr
{
    Elf64_Word    p_type;
    Elf64_Word    p_flags;
    Elf64_Off     p_offset;
    Elf64_Addr    p_vaddr;
    Elf64_Addr    p_paddr;
    Elf64_Xword   p_filesz;
    Elf64_Xword   p_memsz;
    Elf64_Xword   p_align;
}

enum PT_NULL    = 0;
enum PT_LOAD    = 1;
enum PT_DYNAMIC = 2;
enum PT_INTERP  = 3;
enum PT_NOTE    = 4;
enum PT_SHLIB   = 5;
enum PT_PHDR    = 6;
enum PT_TLS     = 7;
enum PT_NUM     = 8;

enum PT_LOOS    = 0x60000000;

enum PT_SUNW_UNWIND   = 0x6464e550;
enum PT_SUNW_EH_FRAME = 0x6474e550;
enum PT_GNU_EH_FRAME  = PT_SUNW_EH_FRAME;

enum PT_GNU_STACK = 0x6474e551;
enum PT_GNU_RELRO = 0x6474e552;

enum PT_LOSUNW     = 0x6ffffffa;
enum PT_SUNWBSS    = 0x6ffffffa;
enum PT_SUNWSTACK  = 0x6ffffffb;
enum PT_SUNWDTRACE = 0x6ffffffc;
enum PT_SUNWCAP    = 0x6ffffffd;
enum PT_HISUNW     = 0x6fffffff;
enum PT_HIOS       = 0x6fffffff;
enum PT_LOPROC     = 0x70000000;
enum PT_HIPROC     = 0x7fffffff;

enum PF_R = 0x4;
enum PF_W = 0x2;
enum PF_X = 0x1;

enum PF_MASKOS   = 0x0ff00000;
enum PF_MASKPROC = 0xf0000000;

enum PF_SUNW_FAILURE = 0x00100000;
enum PF_SUNW_KILLED  = 0x00200000;
enum PF_SUNW_SIGINFO = 0x00400000;

enum PN_XNUM = 0xffff;

struct Elf32_Shdr
{
    Elf32_Word    sh_name;
    Elf32_Word    sh_type;
    Elf32_Word    sh_flags;
    Elf32_Addr    sh_addr;
    Elf32_Off     sh_offset;
    Elf32_Word    sh_size;
    Elf32_Word    sh_link;
    Elf32_Word    sh_info;
    Elf32_Word    sh_addralign;
    Elf32_Word    sh_entsize;
}

struct Elf64_Shdr
{
    Elf64_Word    sh_name;
    Elf64_Word    sh_type;
    Elf64_Xword   sh_flags;
    Elf64_Addr    sh_addr;
    Elf64_Off     sh_offset;
    Elf64_Xword   sh_size;
    Elf64_Word    sh_link;
    Elf64_Word    sh_info;
    Elf64_Xword   sh_addralign;
    Elf64_Xword   sh_entsize;
}

enum SHT_NULL          = 0;
enum SHT_PROGBITS      = 1;
enum SHT_SYMTAB        = 2;
enum SHT_STRTAB        = 3;
enum SHT_RELA          = 4;
enum SHT_HASH          = 5;
enum SHT_DYNAMIC       = 6;
enum SHT_NOTE          = 7;
enum SHT_NOBITS        = 8;
enum SHT_REL           = 9;
enum SHT_SHLIB         = 10;
enum SHT_DYNSYM        = 11;
enum SHT_UNKNOWN12     = 12;
enum SHT_UNKNOWN13     = 13;
enum SHT_INIT_ARRAY    = 14;
enum SHT_FINI_ARRAY    = 15;
enum SHT_PREINIT_ARRAY = 16;
enum SHT_GROUP         = 17;
enum SHT_SYMTAB_SHNDX  = 18;
enum SHT_NUM           = 19;

enum SHT_LOOS           = 0x60000000;
enum SHT_LOSUNW         = 0x6fffffef;
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
enum SHT_SUNW_move      = 0x6ffffffa;
enum SHT_SUNW_COMDAT    = 0x6ffffffb;
enum SHT_SUNW_syminfo   = 0x6ffffffc;
enum SHT_SUNW_verdef    = 0x6ffffffd;
enum SHT_GNU_verdef     = SHT_SUNW_verdef;
enum SHT_SUNW_verneed   = 0x6ffffffe;
enum SHT_GNU_verneed    = SHT_SUNW_verneed;
enum SHT_SUNW_versym    = 0x6fffffff;
enum SHT_GNU_versym     = SHT_SUNW_versym;
enum SHT_HISUNW         = 0x6fffffff;
enum SHT_HIOS           = 0x6fffffff;
enum SHT_GNU_ATTRIBUTES = 0x6ffffff5;
enum SHT_GNU_HASH       = 0x6ffffff6;
enum SHT_GNU_LIBLIST    = 0x6ffffff7;
enum SHT_CHECKSUM       = 0x6ffffff8;
enum SHT_LOPROC         = 0x70000000;
enum SHT_HIPROC         = 0x7fffffff;
enum SHT_LOUSER         = 0x80000000;
enum SHT_HIUSER         = 0xffffffff;

enum SHF_WRITE            = 0x01;
enum SHF_ALLOC            = 0x02;
enum SHF_EXECINSTR        = 0x04;
enum SHF_MERGE            = 0x10;
enum SHF_STRINGS          = 0x20;
enum SHF_INFO_LINK        = 0x40;
enum SHF_LINK_ORDER       = 0x80;
enum SHF_OS_NONCONFORMING = 0x100;
enum SHF_GROUP            = 0x200;
enum SHF_TLS              = 0x400;

enum SHF_MASKOS = 0x0ff00000;

enum SHF_MASKPROC = 0xf0000000;

enum SHN_UNDEF       = 0;
enum SHN_LORESERVE   = 0xff00;
enum SHN_LOPROC      = 0xff00;
enum SHN_HIPROC      = 0xff1f;
enum SHN_LOOS        = 0xff20;
enum SHN_LOSUNW      = 0xff3f;
enum SHN_SUNW_IGNORE = 0xff3f;
enum SHN_HISUNW      = 0xff3f;
enum SHN_HIOS        = 0xff3f;
enum SHN_ABS         = 0xfff1;
enum SHN_COMMON      = 0xfff2;
enum SHN_XINDEX      = 0xffff;
enum SHN_HIRESERVE   = 0xffff;

struct Elf32_Sym
{
    Elf32_Word    st_name;
    Elf32_Addr    st_value;
    Elf32_Word    st_size;
    ubyte         st_info;
    ubyte         st_other;
    Elf32_Half    st_shndx;
}

struct Elf64_Sym
{
    Elf64_Word    st_name;
    ubyte         st_info;
    ubyte         st_other;
    Elf64_Half    st_shndx;
    Elf64_Addr    st_value;
    Elf64_Xword   st_size;
}

enum STN_UNDEF  = 0;

extern (D)
{
    auto ELF32_ST_BIND(T)(T val) { return cast(ubyte)val >> 4; }
    auto ELF32_ST_TYPE(T)(T val) { return val & 0xf; }
    auto ELF32_ST_INFO(B, T)(B bind, T type) { return (bind << 4) + (type & 0xf); }
    alias ELF32_ST_BIND ELF64_ST_BIND;
    alias ELF32_ST_TYPE ELF64_ST_TYPE;
    alias ELF32_ST_INFO ELF64_ST_INFO;
}

enum STB_LOCAL  = 0;
enum STB_GLOBAL = 1;
enum STB_WEAK   = 2;
enum STB_NUM    = 3;
enum STB_LOPROC = 13;
enum STB_HIPROC = 15;

enum STT_NOTYPE  = 0;
enum STT_OBJECT  = 1;
enum STT_FUNC    = 2;
enum STT_SECTION = 3;
enum STT_FILE    = 4;
enum STT_COMMON  = 5;
enum STT_TLS     = 6;
enum STT_NUM     = 7;
enum STT_LOOS    = 10;
enum STT_HIOS    = 12;
enum STT_LOPROC  = 13;
enum STT_HIPROC  = 15;

extern (D)
{
    auto ELF32_ST_VISIBILITY(O)(O o) { return o & 0x07; }
    alias ELF32_ST_VISIBILITY ELF64_ST_VISIBILITY;
}

enum STV_DEFAULT   = 0;
enum STV_INTERNAL  = 1;
enum STV_HIDDEN    = 2;
enum STV_PROTECTED = 3;
enum STV_EXPORTED  = 4;
enum STV_SINGLETON = 5;
enum STV_ELIMINATE = 6;
enum STV_NUM       = 7;

struct Elf32_Rel
{
    Elf32_Addr    r_offset;
    Elf32_Word    r_info;
}

struct Elf32_Rela
{
    Elf32_Addr    r_offset;
    Elf32_Word    r_info;
    Elf32_Sword   r_addend;
}

struct Elf64_Rel
{
    Elf64_Addr    r_offset;
    Elf64_Xword   r_info;
}

struct Elf64_Rela
{
    Elf64_Addr    r_offset;
    Elf64_Xword   r_info;
    Elf64_Sxword  r_addend;
}

extern (D)
{
    auto ELF32_R_SYM(V)(V val) { return val >> 8; }
    auto ELF32_R_TYPE(V)(V val) { return val & 0xff; }
    auto ELF32_R_INFO(S, T)(S sym, T type) { return (sym << 8) + (type & 0xff); }

    auto ELF64_R_SYM(I)(I i) { return i >> 32; }
    auto ELF64_R_TYPE(I)(I i) { return i & 0xffffffff; }
    auto ELF64_R_INFO(S, T)(S sym, T type) { return (sym << 32) + (type); }

    auto ELF64_R_TYPE_DATA(I)(I i) { return (i << 32) >> 40; }
    auto ELF64_R_TYPE_ID(I)(I i) { return (i << 56) >> 56; }
    auto ELF64_R_TYPE_INFO(S, T)(S sym, T type) { return (sym <<8) + (type); }
}

enum GRP_COMDAT = 0x01;

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

struct Elf32_Move
{
    Elf32_Lword m_value;
    Elf32_Word  m_info;
    Elf32_Word  m_poffset;
    Elf32_Half  m_repeat;
    Elf32_Half  m_stride;
}

extern (D)
{
    auto ELF32_M_SYM(I)(I info) { return info >> 8; }
    auto ELF32_M_SIZE(I)(I info) { return cast(ubyte)info; }
    auto ELF32_M_INFO(S, SZ)(S sym, SZ size) { return (sym << 8) + cast(ubyte)size; }
}

struct Elf64_Move
{
    Elf64_Lword m_value;
    Elf64_Xword m_info;
    Elf64_Xword m_poffset;
    Elf64_Half  m_repeat;
    Elf64_Half  m_stride;
}

alias ELF32_M_SYM ELF64_M_SYM;
alias ELF32_M_SIZE ELF64_M_SIZE;
alias ELF32_M_INFO ELF64_M_INFO;

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

/*
 *  Macros to compose and decompose values for capabilities info.
 *
 *  sym = ELF64_C_SYM(info)
 *  grp = ELF64_C_GROUP(info)
 *  info = ELF64_C_INFO(sym, grp)
 */
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
