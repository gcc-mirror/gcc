/**
 * D header file for standard ELF types, structures, and macros.
 *
 * $(LINK2 http://sourceware.org/git/?p=glibc.git;a=blob;f=elf/elf.h, glibc elf/elf.h)
 */
module core.sys.elf;

extern (C):
pure:
nothrow:

import core.stdc.stdint;

alias uint16_t Elf32_Half;
alias uint16_t Elf64_Half;

alias uint32_t Elf32_Word;
alias int32_t  Elf32_Sword;
alias uint32_t Elf64_Word;
alias int32_t  Elf64_Sword;

alias uint64_t Elf32_Xword;
alias int64_t  Elf32_Sxword;
alias uint64_t Elf64_Xword;
alias int64_t  Elf64_Sxword;

alias uint32_t Elf32_Addr;
alias uint64_t Elf64_Addr;

alias uint32_t Elf32_Off;
alias uint64_t Elf64_Off;

alias uint16_t Elf32_Section;
alias uint16_t Elf64_Section;

alias Elf32_Half Elf32_Versym;
alias Elf64_Half Elf64_Versym;

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

enum EI_MAG0 =         0;
enum ELFMAG0 =         0x7f;

enum EI_MAG1 =         1;
enum ELFMAG1 =         'E';

enum EI_MAG2 =         2;
enum ELFMAG2 =         'L';

enum EI_MAG3 =         3;
enum ELFMAG3 =         'F';
enum ELFMAG =          "\177ELF";
enum SELFMAG =         4;

enum EI_CLASS =        4;
enum ELFCLASSNONE =    0;
enum ELFCLASS32 =      1;
enum ELFCLASS64 =      2;
enum ELFCLASSNUM =     3;

enum EI_DATA =         5;
enum ELFDATANONE =     0;
enum ELFDATA2LSB =     1;
enum ELFDATA2MSB =     2;
enum ELFDATANUM =      3;

enum EI_VERSION =      6;

enum EI_OSABI =        7;
enum ELFOSABI_NONE =           0;
enum ELFOSABI_SYSV =           0;
enum ELFOSABI_HPUX =           1;
enum ELFOSABI_NETBSD =         2;
enum ELFOSABI_GNU =            3;
enum ELFOSABI_LINUX =          ELFOSABI_GNU;
enum ELFOSABI_HURD =           4;
enum ELFOSABI_86OPEN =         5;
enum ELFOSABI_SOLARIS =        6;
enum ELFOSABI_AIX =            7;
enum ELFOSABI_MONTEREY =       7;
enum ELFOSABI_IRIX =           8;
enum ELFOSABI_FREEBSD =        9;
enum ELFOSABI_TRU64 =          10;
enum ELFOSABI_MODESTO =        11;
enum ELFOSABI_OPENBSD =        12;
enum ELFOSABI_OPENVMS =        13;
enum ELFOSABI_NSK =            14;
enum ELFOSABI_AROS =           15;
enum ELFOSABI_ARM_AEABI =      64;
enum ELFOSABI_ARM =            97;
enum ELFOSABI_STANDALONE =     255;
enum ELFOSABI_DRAGONFLYBSD =   ELFOSABI_NONE;

enum EI_ABIVERSION =   8;

enum EI_PAD =          9;

enum ET_NONE =         0;
enum ET_REL =          1;
enum ET_EXEC =         2;
enum ET_DYN =          3;
enum ET_CORE =         4;
enum ET_NUM =          5;
enum ET_LOOS =         0xfe00;
enum ET_HIOS =         0xfeff;
enum ET_LOPROC =       0xff00;
enum ET_HIPROC =       0xffff;

enum EM_NONE =          0;
enum EM_M32 =           1;
enum EM_SPARC =         2;
enum EM_386 =           3;
enum EM_68K =           4;
enum EM_88K =           5;
enum EM_860 =           7;
enum EM_MIPS =          8;
enum EM_S370 =          9;
enum EM_MIPS_RS3_LE =  10;

enum EM_PARISC =       15;
enum EM_VPP500 =       17;
enum EM_SPARC32PLUS =  18;
enum EM_960 =          19;
enum EM_PPC =          20;
enum EM_PPC64 =        21;
enum EM_S390 =         22;

enum EM_V800 =         36;
enum EM_FR20 =         37;
enum EM_RH32 =         38;
enum EM_RCE =          39;
enum EM_ARM =          40;
enum EM_FAKE_ALPHA =   41;
enum EM_SH =           42;
enum EM_SPARCV9 =      43;
enum EM_TRICORE =      44;
enum EM_ARC =          45;
enum EM_H8_300 =       46;
enum EM_H8_300H =      47;
enum EM_H8S =          48;
enum EM_H8_500 =       49;
enum EM_IA_64 =        50;
enum EM_MIPS_X =       51;
enum EM_COLDFIRE =     52;
enum EM_68HC12 =       53;
enum EM_MMA =          54;
enum EM_PCP =          55;
enum EM_NCPU =         56;
enum EM_NDR1 =         57;
enum EM_STARCORE =     58;
enum EM_ME16 =         59;
enum EM_ST100 =        60;
enum EM_TINYJ =        61;
enum EM_X86_64 =       62;
enum EM_PDSP =         63;

enum EM_FX66 =         66;
enum EM_ST9PLUS =      67;
enum EM_ST7 =          68;
enum EM_68HC16 =       69;
enum EM_68HC11 =       70;
enum EM_68HC08 =       71;
enum EM_68HC05 =       72;
enum EM_SVX =          73;
enum EM_ST19 =         74;
enum EM_VAX =          75;
enum EM_CRIS =         76;
enum EM_JAVELIN =      77;
enum EM_FIREPATH =     78;
enum EM_ZSP =          79;
enum EM_MMIX =         80;
enum EM_HUANY =        81;
enum EM_PRISM =        82;
enum EM_AVR =          83;
enum EM_FR30 =         84;
enum EM_D10V =         85;
enum EM_D30V =         86;
enum EM_V850 =         87;
enum EM_M32R =         88;
enum EM_MN10300 =      89;
enum EM_MN10200 =      90;
enum EM_PJ =           91;
enum EM_OPENRISC =     92;
enum EM_ARC_A5 =       93;
enum EM_XTENSA =       94;
enum EM_VIDEOCORE =    95;
enum EM_TMM_GPP =      96;
enum EM_NS32K =        97;
enum EM_TPC =          98;
enum EM_SNP1K =        99;
enum EM_ST200 =       100;
enum EM_IP2K =        101;
enum EM_MAX =         102;
enum EM_CR =          103;
enum EM_F2MC16 =      104;
enum EM_MSP430 =      105;
enum EM_BLACKFIN =    106;
enum EM_SE_C33 =      107;
enum EM_SEP =         108;
enum EM_ARCA =        109;
enum EM_UNICORE =     110;
enum EM_DXP =         112;
enum EM_ALTERA_NIOS2 = 113;
enum EM_CRX =         114;
enum EM_XGATE =       115;
enum EM_C166  =       116;
enum EM_M16C  =       117;
enum EM_DSPIC30F =    118;
enum EM_CE =          119;
enum EM_M32C  =       120;
enum EM_res121 =      121;
enum EM_res122 =      122;
enum EM_res123 =      123;
enum EM_res124 =      124;
enum EM_res125 =      125;
enum EM_res126 =      126;
enum EM_res127 =      127;
enum EM_res128 =      128;
enum EM_res129 =      129;
enum EM_res130 =      130;
enum EM_TSK3000 =     131;
enum EM_RS08  =       132;
enum EM_res133 =      133;
enum EM_ECOG2 =       134;
enum EM_SCORE =       135;
enum EM_SCORE7 =      135;
enum EM_DSP24 =       136;
enum EM_VIDEOCORE3 =  137;
enum EM_LATTICEMICO32 = 138;
enum EM_SE_C17 =      139;
enum EM_TI_C6000 =    140;
enum EM_TI_C2000 =    141;
enum EM_TI_C5500 =    142;
enum EM_TI_ARP32 =    143;
enum EM_TI_PRU =      144;
enum EM_MMDSP_PLUS =  160;
enum EM_CYPRESS_M8C = 161;
enum EM_R32C  =       162;
enum EM_TRIMEDIA =    163;
enum EM_QDSP6 =       164;
enum EM_8051  =       165;
enum EM_STXP7X =      166;
enum EM_NDS32 =       167;
enum EM_ECOG1 =       168;
enum EM_ECOG1X =      168;
enum EM_MAXQ30 =      169;
enum EM_XIMO16 =      170;
enum EM_MANIK =       171;
enum EM_CRAYNV2 =     172;
enum EM_RX =          173;
enum EM_METAG =       174;
enum EM_MCST_ELBRUS = 175;
enum EM_ECOG16 =      176;
enum EM_CR16  =       177;
enum EM_ETPU  =       178;
enum EM_SLE9X =       179;
enum EM_L1OM  =       180;
enum EM_K1OM  =       181;
enum EM_INTEL182 =    182;
enum EM_AARCH64 =     183;
enum EM_AVR32 =       185;
enum EM_STM8  =       186;
enum EM_TILE64 =      187;
enum EM_TILEPRO =     188;
enum EM_MICROBLAZE =  189;
enum EM_CUDA  =       190;
enum EM_TILEGX =      191;
enum EM_CLOUDSHIELD = 192;
enum EM_COREA_1ST =   193;
enum EM_COREA_2ND =   194;
enum EM_ARCV2 =       195;
enum EM_OPEN8 =       196;
enum EM_RL78 =        197;
enum EM_VIDEOCORE5 =  198;
enum EM_78KOR =       199;
enum EM_56800EX =     200;
enum EM_BA1 =         201;
enum EM_BA2 =         202;
enum EM_XCORE =       203;
enum EM_MCHP_PIC =    204;
enum EM_INTELGT =     205;

enum EM_KM32 =        210;
enum EM_KMX32 =       211;
enum EM_EMX16 =       212;
enum EM_EMX8 =        213;
enum EM_KVARC =       214;
enum EM_CDP =         215;
enum EM_COGE =        216;
enum EM_COOL =        217;
enum EM_NORC =        218;
enum EM_CSR_KALIMBA = 219;
enum EM_Z80 =         220;
enum EM_VISIUM =      221;
enum EM_FT32 =        222;
enum EM_MOXIE =       223;
enum EM_AMDGPU =      224;

enum EM_RISCV =       243;

enum EM_BPF =         247;
enum EM_CSKY =        252;

enum EM_NUM =         253;

enum EM_LOONGARCH =   258;

enum EM_ALPHA =        0x9026;

enum EV_NONE =         0;
enum EV_CURRENT =      1;
enum EV_NUM =          2;

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

enum SHN_UNDEF =       0;
enum SHN_LORESERVE =   0xff00;
enum SHN_LOPROC =      0xff00;
enum SHN_BEFORE =      0xff00;
enum SHN_AFTER =       0xff01;
enum SHN_HIPROC =      0xff1f;
enum SHN_LOOS =        0xff20;
enum SHN_HIOS =        0xff3f;
enum SHN_ABS =         0xfff1;
enum SHN_COMMON =      0xfff2;
enum SHN_XINDEX =      0xffff;
enum SHN_HIRESERVE =   0xffff;

enum SHT_NULL =          0;
enum SHT_PROGBITS =      1;
enum SHT_SYMTAB =        2;
enum SHT_STRTAB =        3;
enum SHT_RELA =          4;
enum SHT_HASH =          5;
enum SHT_DYNAMIC =       6;
enum SHT_NOTE =          7;
enum SHT_NOBITS =        8;
enum SHT_REL =           9;
enum SHT_SHLIB =         10;
enum SHT_DYNSYM =        11;
enum SHT_INIT_ARRAY =    14;
enum SHT_FINI_ARRAY =    15;
enum SHT_PREINIT_ARRAY = 16;
enum SHT_GROUP =         17;
enum SHT_SYMTAB_SHNDX =  18;
enum SHT_NUM =           19;
enum SHT_LOOS =          0x60000000;
enum SHT_GNU_ATTRIBUTES = 0x6ffffff5;
enum SHT_GNU_HASH =      0x6ffffff6;
enum SHT_GNU_LIBLIST =   0x6ffffff7;
enum SHT_CHECKSUM =      0x6ffffff8;
enum SHT_LOSUNW =        0x6ffffffa;
enum SHT_SUNW_move =     0x6ffffffa;
enum SHT_SUNW_COMDAT =   0x6ffffffb;
enum SHT_SUNW_syminfo =  0x6ffffffc;
enum SHT_GNU_verdef =    0x6ffffffd;
enum SHT_GNU_verneed =   0x6ffffffe;
enum SHT_GNU_versym =    0x6fffffff;
enum SHT_HISUNW =        0x6fffffff;
enum SHT_HIOS =          0x6fffffff;
enum SHT_LOPROC =        0x70000000;
enum SHT_HIPROC =        0x7fffffff;
enum SHT_LOUSER =        0x80000000;
enum SHT_HIUSER =        0x8fffffff;

enum SHF_WRITE =            (1 << 0);
enum SHF_ALLOC =            (1 << 1);
enum SHF_EXECINSTR =        (1 << 2);
enum SHF_MERGE =            (1 << 4);
enum SHF_STRINGS =          (1 << 5);
enum SHF_INFO_LINK =        (1 << 6);
enum SHF_LINK_ORDER =       (1 << 7);
enum SHF_OS_NONCONFORMING = (1 << 8);
enum SHF_GROUP =            (1 << 9);
enum SHF_TLS =              (1 << 10);
enum SHF_COMPRESSED =       (1 << 11);
enum SHF_MASKOS =           0x0ff00000;
enum SHF_MASKPROC =         0xf0000000;
enum SHF_ORDERED =          (1 << 30);
enum SHF_EXCLUDE =          (1 << 31);
enum GRP_COMDAT =      0x1;

struct Elf32_Sym
{
  Elf32_Word    st_name;
  Elf32_Addr    st_value;
  Elf32_Word    st_size;
  ubyte st_info;
  ubyte st_other;
  Elf32_Section st_shndx;
}

struct Elf64_Sym
{
  Elf64_Word    st_name;
  ubyte st_info;
  ubyte st_other;
  Elf64_Section st_shndx;
  Elf64_Addr    st_value;
  Elf64_Xword   st_size;
}

struct Elf32_Syminfo
{
  Elf32_Half si_boundto;
  Elf32_Half si_flags;
}

struct Elf64_Syminfo
{
  Elf64_Half si_boundto;
  Elf64_Half si_flags;
}

enum SYMINFO_BT_SELF =         0xffff;
enum SYMINFO_BT_PARENT =       0xfffe;
enum SYMINFO_BT_LOWRESERVE =   0xff00;

enum SYMINFO_FLG_DIRECT =      0x0001;
enum SYMINFO_FLG_PASSTHRU =    0x0002;
enum SYMINFO_FLG_COPY =        0x0004;
enum SYMINFO_FLG_LAZYLOAD =    0x0008;

enum SYMINFO_NONE =            0;
enum SYMINFO_CURRENT =         1;
enum SYMINFO_NUM =             2;

extern (D)
{
    auto ELF32_ST_BIND(T)(T val) { return cast(ubyte)val >> 4; }
    auto ELF32_ST_TYPE(T)(T val) { return val & 0xf; }
    auto ELF32_ST_INFO(B, T)(B bind, T type) { return (bind << 4) + (type & 0xf); }
    alias ELF32_ST_BIND ELF64_ST_BIND;
    alias ELF32_ST_TYPE ELF64_ST_TYPE;
    alias ELF32_ST_INFO ELF64_ST_INFO;
}

enum STB_LOCAL =       0;
enum STB_GLOBAL =      1;
enum STB_WEAK =        2;
enum STB_NUM =         3;
enum STB_LOOS =        10;
enum STB_GNU_UNIQUE =  10;
enum STB_HIOS =        12;
enum STB_LOPROC =      13;
enum STB_HIPROC =      15;

enum STT_NOTYPE =      0;
enum STT_OBJECT =      1;
enum STT_FUNC =        2;
enum STT_SECTION =     3;
enum STT_FILE =        4;
enum STT_COMMON =      5;
enum STT_TLS =         6;
enum STT_NUM =         7;
enum STT_LOOS =        10;
enum STT_GNU_IFUNC =   10;
enum STT_HIOS =        12;
enum STT_LOPROC =      13;
enum STT_HIPROC =      15;

enum STN_UNDEF =       0;

enum STV_DEFAULT =     0;
enum STV_INTERNAL =    1;
enum STV_HIDDEN =      2;
enum STV_PROTECTED =   3;

struct Elf32_Rel
{
  Elf32_Addr    r_offset;
  Elf32_Word    r_info;
}

struct Elf64_Rel
{
  Elf64_Addr    r_offset;
  Elf64_Xword   r_info;
}

struct Elf32_Rela
{
  Elf32_Addr    r_offset;
  Elf32_Word    r_info;
  Elf32_Sword   r_addend;
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
}

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

enum PN_XNUM =         0xffff;

enum PT_NULL =         0;
enum PT_LOAD =         1;
enum PT_DYNAMIC =      2;
enum PT_INTERP =       3;
enum PT_NOTE =         4;
enum PT_SHLIB =        5;
enum PT_PHDR =         6;
enum PT_TLS =          7;
enum PT_NUM =          8;
enum PT_LOOS =         0x60000000;
enum PT_GNU_EH_FRAME = 0x6474e550;
enum PT_GNU_STACK =    0x6474e551;
enum PT_GNU_RELRO =    0x6474e552;
enum PT_LOSUNW =       0x6ffffffa;
enum PT_SUNWBSS =      0x6ffffffa;
enum PT_SUNWSTACK =    0x6ffffffb;
enum PT_HISUNW =       0x6fffffff;
enum PT_HIOS =         0x6fffffff;
enum PT_LOPROC =       0x70000000;
enum PT_HIPROC =       0x7fffffff;

enum PF_X =            (1 << 0);
enum PF_W =            (1 << 1);
enum PF_R =            (1 << 2);
enum PF_MASKOS =       0x0ff00000;
enum PF_MASKPROC =     0xf0000000;

enum DT_NULL =         0;
enum DT_NEEDED =       1;
enum DT_PLTRELSZ =     2;
enum DT_PLTGOT =       3;
enum DT_HASH =         4;
enum DT_STRTAB =       5;
enum DT_SYMTAB =       6;
enum DT_RELA =         7;
enum DT_RELASZ =       8;
enum DT_RELAENT =      9;
enum DT_STRSZ =        10;
enum DT_SYMENT =       11;
enum DT_INIT =         12;
enum DT_FINI =         13;
enum DT_SONAME =       14;
enum DT_RPATH =        15;
enum DT_SYMBOLIC =     16;
enum DT_REL =          17;
enum DT_RELSZ =        18;
enum DT_RELENT =       19;
enum DT_PLTREL =       20;
enum DT_DEBUG =        21;
enum DT_TEXTREL =      22;
enum DT_JMPREL =       23;
enum DT_BIND_NOW =     24;
enum DT_INIT_ARRAY =   25;
enum DT_FINI_ARRAY =   26;
enum DT_INIT_ARRAYSZ = 27;
enum DT_FINI_ARRAYSZ = 28;
enum DT_RUNPATH =      29;
enum DT_FLAGS =        30;
enum DT_ENCODING =     32;
enum DT_PREINIT_ARRAY = 32;
enum DT_PREINIT_ARRAYSZ = 33;
enum DT_NUM =          34;
enum DT_LOOS =         0x6000000d;
enum DT_HIOS =         0x6ffff000;
enum DT_LOPROC =       0x70000000;
enum DT_HIPROC =       0x7fffffff;
enum DT_PROCNUM =      DT_MIPS_NUM;
enum DT_VALRNGLO =     0x6ffffd00;
enum DT_GNU_PRELINKED = 0x6ffffdf5;
enum DT_GNU_CONFLICTSZ = 0x6ffffdf6;
enum DT_GNU_LIBLISTSZ = 0x6ffffdf7;
enum DT_CHECKSUM =     0x6ffffdf8;
enum DT_PLTPADSZ =     0x6ffffdf9;
enum DT_MOVEENT =      0x6ffffdfa;
enum DT_MOVESZ =       0x6ffffdfb;
enum DT_FEATURE_1 =    0x6ffffdfc;
enum DT_POSFLAG_1 =    0x6ffffdfd;
enum DT_SYMINSZ =      0x6ffffdfe;
enum DT_SYMINENT =     0x6ffffdff;
enum DT_VALRNGHI =     0x6ffffdff;
extern (D) auto DT_VALTAGIDX(T)(T tag)
{
    return DT_VALRNGHI - tag;
}
enum DT_VALNUM = 12;
enum DT_ADDRRNGLO =    0x6ffffe00;
enum DT_GNU_HASH =     0x6ffffef5;
enum DT_TLSDESC_PLT =  0x6ffffef6;
enum DT_TLSDESC_GOT =  0x6ffffef7;
enum DT_GNU_CONFLICT = 0x6ffffef8;
enum DT_GNU_LIBLIST =  0x6ffffef9;
enum DT_CONFIG =       0x6ffffefa;
enum DT_DEPAUDIT =     0x6ffffefb;
enum DT_AUDIT =        0x6ffffefc;
enum DT_PLTPAD =       0x6ffffefd;
enum DT_MOVETAB =      0x6ffffefe;
enum DT_SYMINFO =      0x6ffffeff;
enum DT_ADDRRNGHI =    0x6ffffeff;
extern (D) auto DT_ADDRTAGIDX(T)(T tag)
{
    return DT_ADDRRNGHI - tag;
}
enum DT_ADDRNUM = 11;
enum DT_VERSYM =       0x6ffffff0;

enum DT_RELACOUNT =    0x6ffffff9;
enum DT_RELCOUNT =     0x6ffffffa;
enum DT_FLAGS_1 =      0x6ffffffb;
enum DT_VERDEF =       0x6ffffffc;
enum DT_VERDEFNUM =    0x6ffffffd;
enum DT_VERNEED =      0x6ffffffe;
enum DT_VERNEEDNUM =   0x6fffffff;
extern (D) auto DT_VERSIONTAGIDX(T)(T tag)
{
    return DT_VERNEEDNUM - tag;
}
enum DT_VERSIONTAGNUM = 16;
enum DT_AUXILIARY =    0x7ffffffd;
enum DT_FILTER =       0x7fffffff;
extern (D) auto DT_EXTRATAGIDX(T)(T tag)
{
    return cast(Elf32_Word)(-(cast(Elf32_Sword)(tag) <<1>>1)-1);
}
enum DT_EXTRANUM =     3;
enum DF_ORIGIN =       0x00000001;
enum DF_SYMBOLIC =     0x00000002;
enum DF_TEXTREL =      0x00000004;
enum DF_BIND_NOW =     0x00000008;
enum DF_STATIC_TLS =   0x00000010;
enum DF_1_NOW =        0x00000001;
enum DF_1_GLOBAL =     0x00000002;
enum DF_1_GROUP =      0x00000004;
enum DF_1_NODELETE =   0x00000008;
enum DF_1_LOADFLTR =   0x00000010;
enum DF_1_INITFIRST =  0x00000020;
enum DF_1_NOOPEN =     0x00000040;
enum DF_1_ORIGIN =     0x00000080;
enum DF_1_DIRECT =     0x00000100;
enum DF_1_TRANS =      0x00000200;
enum DF_1_INTERPOSE =  0x00000400;
enum DF_1_NODEFLIB =   0x00000800;
enum DF_1_NODUMP =     0x00001000;
enum DF_1_CONFALT =    0x00002000;
enum DF_1_ENDFILTEE =  0x00004000;
enum DF_1_DISPRELDNE = 0x00008000;
enum DF_1_DISPRELPND = 0x00010000;
enum DF_1_NODIRECT =   0x00020000;
enum DF_1_IGNMULDEF =  0x00040000;
enum DF_1_NOKSYMS =    0x00080000;
enum DF_1_NOHDR =      0x00100000;
enum DF_1_EDITED =     0x00200000;
enum DF_1_NORELOC =    0x00400000;
enum DF_1_SYMINTPOSE = 0x00800000;
enum DF_1_GLOBAUDIT =  0x01000000;
enum DF_1_SINGLETON =  0x02000000;
enum DTF_1_PARINIT =   0x00000001;
enum DTF_1_CONFEXP =   0x00000002;
enum DF_P1_LAZYLOAD =  0x00000001;
enum DF_P1_GROUPPERM = 0x00000002;

struct Elf32_Verdef
{
    Elf32_Half    vd_version;
    Elf32_Half    vd_flags;
    Elf32_Half    vd_ndx;
    Elf32_Half    vd_cnt;
    Elf32_Word    vd_hash;
    Elf32_Word    vd_aux;
    Elf32_Word    vd_next;
}

struct Elf64_Verdef
{
    Elf64_Half    vd_version;
    Elf64_Half    vd_flags;
    Elf64_Half    vd_ndx;
    Elf64_Half    vd_cnt;
    Elf64_Word    vd_hash;
    Elf64_Word    vd_aux;
    Elf64_Word    vd_next;
}
enum VER_DEF_NONE =    0;
enum VER_DEF_CURRENT = 1;
enum VER_DEF_NUM =     2;
enum VER_FLG_BASE =    0x1;
enum VER_FLG_WEAK =    0x2;
enum VER_NDX_LOCAL =           0;
enum VER_NDX_GLOBAL =          1;
enum VER_NDX_LORESERVE =       0xff00;
enum VER_NDX_ELIMINATE =       0xff01;

struct Elf32_Verdaux
{
    Elf32_Word    vda_name;
    Elf32_Word    vda_next;
}

struct Elf64_Verdaux
{
    Elf64_Word    vda_name;
    Elf64_Word    vda_next;
}

struct Elf32_Verneed
{
    Elf32_Half    vn_version;
    Elf32_Half    vn_cnt;
    Elf32_Word    vn_file;
    Elf32_Word    vn_aux;
    Elf32_Word    vn_next;
}

struct Elf64_Verneed
{
    Elf64_Half    vn_version;
    Elf64_Half    vn_cnt;
    Elf64_Word    vn_file;
    Elf64_Word    vn_aux;
    Elf64_Word    vn_next;
}
enum VER_NEED_NONE =    0;
enum VER_NEED_CURRENT = 1;
enum VER_NEED_NUM =     2;

struct Elf32_Vernaux
{
    Elf32_Word    vna_hash;
    Elf32_Half    vna_flags;
    Elf32_Half    vna_other;
    Elf32_Word    vna_name;
    Elf32_Word    vna_next;
}

struct Elf64_Vernaux
{
    Elf64_Word    vna_hash;
    Elf64_Half    vna_flags;
    Elf64_Half    vna_other;
    Elf64_Word    vna_name;
    Elf64_Word    vna_next;
}

enum ELF_NOTE_SOLARIS =        "SUNW Solaris";
enum ELF_NOTE_GNU =            "GNU";
enum ELF_NOTE_PAGESIZE_HINT =  1;
enum ELF_NOTE_ABI =    1;
enum ELF_NOTE_OS_LINUX =       0;
enum ELF_NOTE_OS_GNU =         1;
enum ELF_NOTE_OS_SOLARIS2 =    2;
enum ELF_NOTE_OS_FREEBSD =     3;

struct Elf32_Move
{
  Elf32_Xword m_value;
  Elf32_Word m_info;
  Elf32_Word m_poffset;
  Elf32_Half m_repeat;
  Elf32_Half m_stride;
}

struct Elf64_Move
{
  Elf64_Xword m_value;
  Elf64_Xword m_info;
  Elf64_Xword m_poffset;
  Elf64_Half m_repeat;
  Elf64_Half m_stride;
}
extern (D)
{
    auto ELF32_M_SYM(I)(I info) { return info >> 8; }
    auto ELF32_M_SIZE(I)(I info) { return cast(ubyte)info; }
    auto ELF32_M_INFO(S, SZ)(S sym, SZ size) { return (sym << 8) + cast(ubyte)size; }
}

alias ELF32_M_SYM ELF64_M_SYM;
alias ELF32_M_SIZE ELF64_M_SIZE;
alias ELF32_M_INFO ELF64_M_INFO;
enum EF_CPU32 =        0x00810000;

enum R_68K_NONE =      0;
enum R_68K_32 =        1;
enum R_68K_16 =        2;
enum R_68K_8 =         3;
enum R_68K_PC32 =      4;
enum R_68K_PC16 =      5;
enum R_68K_PC8 =       6;
enum R_68K_GOT32 =     7;
enum R_68K_GOT16 =     8;
enum R_68K_GOT8 =      9;
enum R_68K_GOT32O =    10;
enum R_68K_GOT16O =    11;
enum R_68K_GOT8O =     12;
enum R_68K_PLT32 =     13;
enum R_68K_PLT16 =     14;
enum R_68K_PLT8 =      15;
enum R_68K_PLT32O =    16;
enum R_68K_PLT16O =    17;
enum R_68K_PLT8O =     18;
enum R_68K_COPY =      19;
enum R_68K_GLOB_DAT =  20;
enum R_68K_JMP_SLOT =  21;
enum R_68K_RELATIVE =  22;
enum R_68K_TLS_GD32 =      25;
enum R_68K_TLS_GD16 =      26;
enum R_68K_TLS_GD8 =       27;
enum R_68K_TLS_LDM32 =     28;
enum R_68K_TLS_LDM16 =     29;
enum R_68K_TLS_LDM8 =      30;
enum R_68K_TLS_LDO32 =     31;
enum R_68K_TLS_LDO16 =     32;
enum R_68K_TLS_LDO8 =      33;
enum R_68K_TLS_IE32 =      34;
enum R_68K_TLS_IE16 =      35;
enum R_68K_TLS_IE8 =       36;
enum R_68K_TLS_LE32 =      37;
enum R_68K_TLS_LE16 =      38;
enum R_68K_TLS_LE8 =       39;
enum R_68K_TLS_DTPMOD32 =  40;
enum R_68K_TLS_DTPREL32 =  41;
enum R_68K_TLS_TPREL32 =   42;
enum R_68K_NUM =       43;

enum R_386_NONE =         0;
enum R_386_32 =           1;
enum R_386_PC32 =         2;
enum R_386_GOT32 =        3;
enum R_386_PLT32 =        4;
enum R_386_COPY =         5;
enum R_386_GLOB_DAT =     6;
enum R_386_JMP_SLOT =     7;
enum R_386_RELATIVE =     8;
enum R_386_GOTOFF =       9;
enum R_386_GOTPC =        10;
enum R_386_32PLT =        11;
enum R_386_TLS_TPOFF =    14;
enum R_386_TLS_IE =       15;
enum R_386_TLS_GOTIE =    16;
enum R_386_TLS_LE =       17;
enum R_386_TLS_GD =       18;
enum R_386_TLS_LDM =      19;
enum R_386_16 =           20;
enum R_386_PC16 =         21;
enum R_386_8 =            22;
enum R_386_PC8 =          23;
enum R_386_TLS_GD_32 =    24;
enum R_386_TLS_GD_PUSH =  25;
enum R_386_TLS_GD_CALL =  26;
enum R_386_TLS_GD_POP =   27;
enum R_386_TLS_LDM_32 =   28;
enum R_386_TLS_LDM_PUSH = 29;
enum R_386_TLS_LDM_CALL = 30;
enum R_386_TLS_LDM_POP =  31;
enum R_386_TLS_LDO_32 =   32;
enum R_386_TLS_IE_32 =    33;
enum R_386_TLS_LE_32 =    34;
enum R_386_TLS_DTPMOD32 = 35;
enum R_386_TLS_DTPOFF32 = 36;
enum R_386_TLS_TPOFF32 =  37;
enum R_386_SIZE32 =       38;
enum R_386_TLS_GOTDESC =  39;
enum R_386_TLS_DESC_CALL = 40;
enum R_386_TLS_DESC =     41;
enum R_386_IRELATIVE =    42;
enum R_386_NUM =          43;

enum STT_SPARC_REGISTER =      13;

enum EF_SPARCV9_MM =           3;
enum EF_SPARCV9_TSO =          0;
enum EF_SPARCV9_PSO =          1;
enum EF_SPARCV9_RMO =          2;
enum EF_SPARC_LEDATA =         0x800000;
enum EF_SPARC_EXT_MASK =       0xFFFF00;
enum EF_SPARC_32PLUS =         0x000100;
enum EF_SPARC_SUN_US1 =        0x000200;
enum EF_SPARC_HAL_R1 =         0x000400;
enum EF_SPARC_SUN_US3 =        0x000800;

enum R_SPARC_NONE =            0;
enum R_SPARC_8 =               1;
enum R_SPARC_16 =              2;
enum R_SPARC_32 =              3;
enum R_SPARC_DISP8 =           4;
enum R_SPARC_DISP16 =          5;
enum R_SPARC_DISP32 =          6;
enum R_SPARC_WDISP30 =         7;
enum R_SPARC_WDISP22 =         8;
enum R_SPARC_HI22 =            9;
enum R_SPARC_22 =              10;
enum R_SPARC_13 =              11;
enum R_SPARC_LO10 =            12;
enum R_SPARC_GOT10 =           13;
enum R_SPARC_GOT13 =           14;
enum R_SPARC_GOT22 =           15;
enum R_SPARC_PC10 =            16;
enum R_SPARC_PC22 =            17;
enum R_SPARC_WPLT30 =          18;
enum R_SPARC_COPY =            19;
enum R_SPARC_GLOB_DAT =        20;
enum R_SPARC_JMP_SLOT =        21;
enum R_SPARC_RELATIVE =        22;
enum R_SPARC_UA32 =            23;

enum R_SPARC_PLT32 =           24;
enum R_SPARC_HIPLT22 =         25;
enum R_SPARC_LOPLT10 =         26;
enum R_SPARC_PCPLT32 =         27;
enum R_SPARC_PCPLT22 =         28;
enum R_SPARC_PCPLT10 =         29;
enum R_SPARC_10 =              30;
enum R_SPARC_11 =              31;
enum R_SPARC_64 =              32;
enum R_SPARC_OLO10 =           33;
enum R_SPARC_HH22 =            34;
enum R_SPARC_HM10 =            35;
enum R_SPARC_LM22 =            36;
enum R_SPARC_PC_HH22 =         37;
enum R_SPARC_PC_HM10 =         38;
enum R_SPARC_PC_LM22 =         39;
enum R_SPARC_WDISP16 =         40;
enum R_SPARC_WDISP19 =         41;
enum R_SPARC_GLOB_JMP =        42;
enum R_SPARC_7 =               43;
enum R_SPARC_5 =               44;
enum R_SPARC_6 =               45;
enum R_SPARC_DISP64 =          46;
enum R_SPARC_PLT64 =           47;
enum R_SPARC_HIX22 =           48;
enum R_SPARC_LOX10 =           49;
enum R_SPARC_H44 =             50;
enum R_SPARC_M44 =             51;
enum R_SPARC_L44 =             52;
enum R_SPARC_REGISTER =        53;
enum R_SPARC_UA64 =            54;
enum R_SPARC_UA16 =            55;
enum R_SPARC_TLS_GD_HI22 =     56;
enum R_SPARC_TLS_GD_LO10 =     57;
enum R_SPARC_TLS_GD_ADD =      58;
enum R_SPARC_TLS_GD_CALL =     59;
enum R_SPARC_TLS_LDM_HI22 =    60;
enum R_SPARC_TLS_LDM_LO10 =    61;
enum R_SPARC_TLS_LDM_ADD =     62;
enum R_SPARC_TLS_LDM_CALL =    63;
enum R_SPARC_TLS_LDO_HIX22 =   64;
enum R_SPARC_TLS_LDO_LOX10 =   65;
enum R_SPARC_TLS_LDO_ADD =     66;
enum R_SPARC_TLS_IE_HI22 =     67;
enum R_SPARC_TLS_IE_LO10 =     68;
enum R_SPARC_TLS_IE_LD =       69;
enum R_SPARC_TLS_IE_LDX =      70;
enum R_SPARC_TLS_IE_ADD =      71;
enum R_SPARC_TLS_LE_HIX22 =    72;
enum R_SPARC_TLS_LE_LOX10 =    73;
enum R_SPARC_TLS_DTPMOD32 =    74;
enum R_SPARC_TLS_DTPMOD64 =    75;
enum R_SPARC_TLS_DTPOFF32 =    76;
enum R_SPARC_TLS_DTPOFF64 =    77;
enum R_SPARC_TLS_TPOFF32 =     78;
enum R_SPARC_TLS_TPOFF64 =     79;
enum R_SPARC_GOTDATA_HIX22 =   80;
enum R_SPARC_GOTDATA_LOX10 =   81;
enum R_SPARC_GOTDATA_OP_HIX22 =        82;
enum R_SPARC_GOTDATA_OP_LOX10 =        83;
enum R_SPARC_GOTDATA_OP =      84;
enum R_SPARC_H34 =             85;
enum R_SPARC_SIZE32 =          86;
enum R_SPARC_SIZE64 =          87;
enum R_SPARC_WDISP10 =         88;
enum R_SPARC_JMP_IREL =        248;
enum R_SPARC_IRELATIVE =       249;
enum R_SPARC_GNU_VTINHERIT =   250;
enum R_SPARC_GNU_VTENTRY =     251;
enum R_SPARC_REV32 =           252;
enum R_SPARC_NUM =             253;

enum DT_SPARC_REGISTER =       0x70000001;
enum DT_SPARC_NUM =            2;

enum EF_MIPS_NOREORDER =       1;
enum EF_MIPS_PIC =             2;
enum EF_MIPS_CPIC =            4;
enum EF_MIPS_XGOT =            8;
enum EF_MIPS_64BIT_WHIRL =     16;
enum EF_MIPS_ABI2 =            32;
enum EF_MIPS_ABI_ON32 =        64;
enum EF_MIPS_ARCH =            0xf0000000;

enum EF_MIPS_ARCH_1 =          0x00000000;
enum EF_MIPS_ARCH_2 =          0x10000000;
enum EF_MIPS_ARCH_3 =          0x20000000;
enum EF_MIPS_ARCH_4 =          0x30000000;
enum EF_MIPS_ARCH_5 =          0x40000000;
enum EF_MIPS_ARCH_32 =         0x50000000;
enum EF_MIPS_ARCH_64 =         0x60000000;
enum EF_MIPS_ARCH_32R2 =       0x70000000;
enum EF_MIPS_ARCH_64R2 =       0x80000000;

enum E_MIPS_ARCH_1 =           EF_MIPS_ARCH_1;
enum E_MIPS_ARCH_2 =           EF_MIPS_ARCH_2;
enum E_MIPS_ARCH_3 =           EF_MIPS_ARCH_3;
enum E_MIPS_ARCH_4 =           EF_MIPS_ARCH_4;
enum E_MIPS_ARCH_5 =           EF_MIPS_ARCH_5;
enum E_MIPS_ARCH_32 =          EF_MIPS_ARCH_32;
enum E_MIPS_ARCH_64 =          EF_MIPS_ARCH_64;

enum SHN_MIPS_ACOMMON =        0xff00;
enum SHN_MIPS_TEXT =           0xff01;
enum SHN_MIPS_DATA =           0xff02;
enum SHN_MIPS_SCOMMON =        0xff03;
enum SHN_MIPS_SUNDEFINED =     0xff04;

enum SHT_MIPS_LIBLIST =        0x70000000;
enum SHT_MIPS_MSYM =           0x70000001;
enum SHT_MIPS_CONFLICT =       0x70000002;
enum SHT_MIPS_GPTAB =          0x70000003;
enum SHT_MIPS_UCODE =          0x70000004;
enum SHT_MIPS_DEBUG =          0x70000005;
enum SHT_MIPS_REGINFO =        0x70000006;
enum SHT_MIPS_PACKAGE =        0x70000007;
enum SHT_MIPS_PACKSYM =        0x70000008;
enum SHT_MIPS_RELD =           0x70000009;
enum SHT_MIPS_IFACE =          0x7000000b;
enum SHT_MIPS_CONTENT =        0x7000000c;
enum SHT_MIPS_OPTIONS =        0x7000000d;
enum SHT_MIPS_SHDR =           0x70000010;
enum SHT_MIPS_FDESC =          0x70000011;
enum SHT_MIPS_EXTSYM =         0x70000012;
enum SHT_MIPS_DENSE =          0x70000013;
enum SHT_MIPS_PDESC =          0x70000014;
enum SHT_MIPS_LOCSYM =         0x70000015;
enum SHT_MIPS_AUXSYM =         0x70000016;
enum SHT_MIPS_OPTSYM =         0x70000017;
enum SHT_MIPS_LOCSTR =         0x70000018;
enum SHT_MIPS_LINE =           0x70000019;
enum SHT_MIPS_RFDESC =         0x7000001a;
enum SHT_MIPS_DELTASYM =       0x7000001b;
enum SHT_MIPS_DELTAINST =      0x7000001c;
enum SHT_MIPS_DELTACLASS =     0x7000001d;
enum SHT_MIPS_DWARF =          0x7000001e;
enum SHT_MIPS_DELTADECL =      0x7000001f;
enum SHT_MIPS_SYMBOL_LIB =     0x70000020;
enum SHT_MIPS_EVENTS =         0x70000021;
enum SHT_MIPS_TRANSLATE =      0x70000022;
enum SHT_MIPS_PIXIE =          0x70000023;
enum SHT_MIPS_XLATE =          0x70000024;
enum SHT_MIPS_XLATE_DEBUG =    0x70000025;
enum SHT_MIPS_WHIRL =          0x70000026;
enum SHT_MIPS_EH_REGION =      0x70000027;
enum SHT_MIPS_XLATE_OLD =      0x70000028;
enum SHT_MIPS_PDR_EXCEPTION =  0x70000029;

enum SHF_MIPS_GPREL =          0x10000000;
enum SHF_MIPS_MERGE =          0x20000000;
enum SHF_MIPS_ADDR =           0x40000000;
enum SHF_MIPS_STRINGS =        0x80000000;
enum SHF_MIPS_NOSTRIP =        0x08000000;
enum SHF_MIPS_LOCAL =          0x04000000;
enum SHF_MIPS_NAMES =          0x02000000;
enum SHF_MIPS_NODUPE =         0x01000000;
enum STO_MIPS_DEFAULT =                0x0;
enum STO_MIPS_INTERNAL =               0x1;
enum STO_MIPS_HIDDEN =                 0x2;
enum STO_MIPS_PROTECTED =              0x3;
enum STO_MIPS_PLT =                    0x8;
enum STO_MIPS_SC_ALIGN_UNUSED =        0xff;
enum STB_MIPS_SPLIT_COMMON =           13;

union Elf32_gptab
{
    struct _gt_header
    {
        Elf32_Word gt_current_g_value;
        Elf32_Word gt_unused;
    } _gt_header gt_header;
    struct _gt_entry
    {
        Elf32_Word gt_g_value;
        Elf32_Word gt_bytes;
    } _gt_entry gt_entry;
}

struct Elf32_RegInfo
{
    Elf32_Word ri_gprmask;
    Elf32_Word[4] ri_cprmask;
    Elf32_Sword ri_gp_value;
}

struct Elf_Options
{
    ubyte kind;
    ubyte size;
    Elf32_Section section;
    Elf32_Word info;
}

enum ODK_NULL =        0;
enum ODK_REGINFO =     1;
enum ODK_EXCEPTIONS =  2;
enum ODK_PAD =         3;
enum ODK_HWPATCH =     4;
enum ODK_FILL =        5;
enum ODK_TAGS =        6;
enum ODK_HWAND =       7;
enum ODK_HWOR =        8;

enum OEX_FPU_MIN =     0x1f;
enum OEX_FPU_MAX =     0x1f00;
enum OEX_PAGE0 =       0x10000;
enum OEX_SMM =         0x20000;
enum OEX_FPDBUG =      0x40000;
enum OEX_PRECISEFP =   OEX_FPDBUG;
enum OEX_DISMISS =     0x80000;

enum OEX_FPU_INVAL =   0x10;
enum OEX_FPU_DIV0 =    0x08;
enum OEX_FPU_OFLO =    0x04;
enum OEX_FPU_UFLO =    0x02;
enum OEX_FPU_INEX =    0x01;

enum OHW_R4KEOP =      0x1;
enum OHW_R8KPFETCH =   0x2;
enum OHW_R5KEOP =      0x4;
enum OHW_R5KCVTL =     0x8;

enum OPAD_PREFIX =     0x1;
enum OPAD_POSTFIX =    0x2;
enum OPAD_SYMBOL =     0x4;

struct Elf_Options_Hw
{
    Elf32_Word hwp_flags1;
    Elf32_Word hwp_flags2;
}

enum OHWA0_R4KEOP_CHECKED =    0x00000001;
enum OHWA1_R4KEOP_CLEAN =      0x00000002;

enum R_MIPS_NONE =             0;
enum R_MIPS_16 =               1;
enum R_MIPS_32 =               2;
enum R_MIPS_REL32 =            3;
enum R_MIPS_26 =               4;
enum R_MIPS_HI16 =             5;
enum R_MIPS_LO16 =             6;
enum R_MIPS_GPREL16 =          7;
enum R_MIPS_LITERAL =          8;
enum R_MIPS_GOT16 =            9;
enum R_MIPS_PC16 =             10;
enum R_MIPS_CALL16 =           11;
enum R_MIPS_GPREL32 =          12;

enum R_MIPS_SHIFT5 =           16;
enum R_MIPS_SHIFT6 =           17;
enum R_MIPS_64 =               18;
enum R_MIPS_GOT_DISP =         19;
enum R_MIPS_GOT_PAGE =         20;
enum R_MIPS_GOT_OFST =         21;
enum R_MIPS_GOT_HI16 =         22;
enum R_MIPS_GOT_LO16 =         23;
enum R_MIPS_SUB =              24;
enum R_MIPS_INSERT_A =         25;
enum R_MIPS_INSERT_B =         26;
enum R_MIPS_DELETE =           27;
enum R_MIPS_HIGHER =           28;
enum R_MIPS_HIGHEST =          29;
enum R_MIPS_CALL_HI16 =        30;
enum R_MIPS_CALL_LO16 =        31;
enum R_MIPS_SCN_DISP =         32;
enum R_MIPS_REL16 =            33;
enum R_MIPS_ADD_IMMEDIATE =    34;
enum R_MIPS_PJUMP =            35;
enum R_MIPS_RELGOT =           36;
enum R_MIPS_JALR =             37;
enum R_MIPS_TLS_DTPMOD32 =     38;
enum R_MIPS_TLS_DTPREL32 =     39;
enum R_MIPS_TLS_DTPMOD64 =     40;
enum R_MIPS_TLS_DTPREL64 =     41;
enum R_MIPS_TLS_GD =           42;
enum R_MIPS_TLS_LDM =          43;
enum R_MIPS_TLS_DTPREL_HI16 =  44;
enum R_MIPS_TLS_DTPREL_LO16 =  45;
enum R_MIPS_TLS_GOTTPREL =     46;
enum R_MIPS_TLS_TPREL32 =      47;
enum R_MIPS_TLS_TPREL64 =      48;
enum R_MIPS_TLS_TPREL_HI16 =   49;
enum R_MIPS_TLS_TPREL_LO16 =   50;
enum R_MIPS_GLOB_DAT =         51;
enum R_MIPS_COPY =             126;
enum R_MIPS_JUMP_SLOT =        127;
enum R_MIPS_NUM =              128;

enum PT_MIPS_REGINFO = 0x70000000;
enum PT_MIPS_RTPROC =  0x70000001;
enum PT_MIPS_OPTIONS = 0x70000002;

enum PF_MIPS_LOCAL =   0x10000000;

enum DT_MIPS_RLD_VERSION =  0x70000001;
enum DT_MIPS_TIME_STAMP =   0x70000002;
enum DT_MIPS_ICHECKSUM =    0x70000003;
enum DT_MIPS_IVERSION =     0x70000004;
enum DT_MIPS_FLAGS =        0x70000005;
enum DT_MIPS_BASE_ADDRESS = 0x70000006;
enum DT_MIPS_MSYM =         0x70000007;
enum DT_MIPS_CONFLICT =     0x70000008;
enum DT_MIPS_LIBLIST =      0x70000009;
enum DT_MIPS_LOCAL_GOTNO =  0x7000000a;
enum DT_MIPS_CONFLICTNO =   0x7000000b;
enum DT_MIPS_LIBLISTNO =    0x70000010;
enum DT_MIPS_SYMTABNO =     0x70000011;
enum DT_MIPS_UNREFEXTNO =   0x70000012;
enum DT_MIPS_GOTSYM =       0x70000013;
enum DT_MIPS_HIPAGENO =     0x70000014;
enum DT_MIPS_RLD_MAP =      0x70000016;
enum DT_MIPS_DELTA_CLASS =  0x70000017;
enum DT_MIPS_DELTA_CLASS_NO =    0x70000018;
enum DT_MIPS_DELTA_INSTANCE =    0x70000019;
enum DT_MIPS_DELTA_INSTANCE_NO = 0x7000001a;
enum DT_MIPS_DELTA_RELOC =  0x7000001b;
enum DT_MIPS_DELTA_RELOC_NO = 0x7000001c;
enum DT_MIPS_DELTA_SYM =    0x7000001d;
enum DT_MIPS_DELTA_SYM_NO = 0x7000001e;
enum DT_MIPS_DELTA_CLASSSYM = 0x70000020;
enum DT_MIPS_DELTA_CLASSSYM_NO = 0x70000021;
enum DT_MIPS_CXX_FLAGS =    0x70000022;
enum DT_MIPS_PIXIE_INIT =   0x70000023;
enum DT_MIPS_SYMBOL_LIB =   0x70000024;
enum DT_MIPS_LOCALPAGE_GOTIDX = 0x70000025;
enum DT_MIPS_LOCAL_GOTIDX = 0x70000026;
enum DT_MIPS_HIDDEN_GOTIDX = 0x70000027;
enum DT_MIPS_PROTECTED_GOTIDX = 0x70000028;
enum DT_MIPS_OPTIONS =      0x70000029;
enum DT_MIPS_INTERFACE =    0x7000002a;
enum DT_MIPS_DYNSTR_ALIGN = 0x7000002b;
enum DT_MIPS_INTERFACE_SIZE = 0x7000002c;
enum DT_MIPS_RLD_TEXT_RESOLVE_ADDR = 0x7000002d;
enum DT_MIPS_PERF_SUFFIX =  0x7000002e;
enum DT_MIPS_COMPACT_SIZE = 0x7000002f;
enum DT_MIPS_GP_VALUE =     0x70000030;
enum DT_MIPS_AUX_DYNAMIC =  0x70000031;
enum DT_MIPS_PLTGOT =       0x70000032;
enum DT_MIPS_RWPLT =        0x70000034;
enum DT_MIPS_NUM =          0x35;

enum RHF_NONE =                   0;
enum RHF_QUICKSTART =             (1 << 0);
enum RHF_NOTPOT =                 (1 << 1);
enum RHF_NO_LIBRARY_REPLACEMENT = (1 << 2);
enum RHF_NO_MOVE =                (1 << 3);
enum RHF_SGI_ONLY =               (1 << 4);
enum RHF_GUARANTEE_INIT =         (1 << 5);
enum RHF_DELTA_C_PLUS_PLUS =      (1 << 6);
enum RHF_GUARANTEE_START_INIT =   (1 << 7);
enum RHF_PIXIE =                  (1 << 8);
enum RHF_DEFAULT_DELAY_LOAD =     (1 << 9);
enum RHF_REQUICKSTART =           (1 << 10);
enum RHF_REQUICKSTARTED =         (1 << 11);
enum RHF_CORD =                   (1 << 12);
enum RHF_NO_UNRES_UNDEF =         (1 << 13);
enum RHF_RLD_ORDER_SAFE =         (1 << 14);

struct Elf32_Lib
{
    Elf32_Word l_name;
    Elf32_Word l_time_stamp;
    Elf32_Word l_checksum;
    Elf32_Word l_version;
    Elf32_Word l_flags;
}

struct Elf64_Lib
{
    Elf64_Word l_name;
    Elf64_Word l_time_stamp;
    Elf64_Word l_checksum;
    Elf64_Word l_version;
    Elf64_Word l_flags;
}

enum LL_NONE =           0;
enum LL_EXACT_MATCH =    (1 << 0);
enum LL_IGNORE_INT_VER = (1 << 1);
enum LL_REQUIRE_MINOR =  (1 << 2);
enum LL_EXPORTS =        (1 << 3);
enum LL_DELAY_LOAD =     (1 << 4);
enum LL_DELTA =          (1 << 5);

alias Elf32_Addr Elf32_Conflict;

enum EF_PARISC_TRAPNIL =       0x00010000;
enum EF_PARISC_EXT =           0x00020000;
enum EF_PARISC_LSB =           0x00040000;
enum EF_PARISC_WIDE =          0x00080000;
enum EF_PARISC_NO_KABP =       0x00100000;
enum EF_PARISC_LAZYSWAP =      0x00400000;
enum EF_PARISC_ARCH =          0x0000ffff;

enum EFA_PARISC_1_0 =              0x020b;
enum EFA_PARISC_1_1 =              0x0210;
enum EFA_PARISC_2_0 =              0x0214;

enum SHN_PARISC_ANSI_COMMON =  0xff00;
enum SHN_PARISC_HUGE_COMMON =  0xff01;

enum SHT_PARISC_EXT =          0x70000000;
enum SHT_PARISC_UNWIND =       0x70000001;
enum SHT_PARISC_DOC =          0x70000002;

enum SHF_PARISC_SHORT =        0x20000000;
enum SHF_PARISC_HUGE =         0x40000000;
enum SHF_PARISC_SBP =          0x80000000;

enum STT_PARISC_MILLICODE =    13;

enum STT_HP_OPAQUE =           (STT_LOOS + 0x1);
enum STT_HP_STUB =             (STT_LOOS + 0x2);

enum R_PARISC_NONE =           0;
enum R_PARISC_DIR32 =          1;
enum R_PARISC_DIR21L =         2;
enum R_PARISC_DIR17R =         3;
enum R_PARISC_DIR17F =         4;
enum R_PARISC_DIR14R =         6;
enum R_PARISC_PCREL32 =        9;
enum R_PARISC_PCREL21L =       10;
enum R_PARISC_PCREL17R =       11;
enum R_PARISC_PCREL17F =       12;
enum R_PARISC_PCREL14R =       14;
enum R_PARISC_DPREL21L =       18;
enum R_PARISC_DPREL14R =       22;
enum R_PARISC_GPREL21L =       26;
enum R_PARISC_GPREL14R =       30;
enum R_PARISC_LTOFF21L =       34;
enum R_PARISC_LTOFF14R =       38;
enum R_PARISC_SECREL32 =       41;
enum R_PARISC_SEGBASE =        48;
enum R_PARISC_SEGREL32 =       49;
enum R_PARISC_PLTOFF21L =      50;
enum R_PARISC_PLTOFF14R =      54;
enum R_PARISC_LTOFF_FPTR32 =   57;
enum R_PARISC_LTOFF_FPTR21L =  58;
enum R_PARISC_LTOFF_FPTR14R =  62;
enum R_PARISC_FPTR64 =         64;
enum R_PARISC_PLABEL32 =       65;
enum R_PARISC_PLABEL21L =      66;
enum R_PARISC_PLABEL14R =      70;
enum R_PARISC_PCREL64 =        72;
enum R_PARISC_PCREL22F =       74;
enum R_PARISC_PCREL14WR =      75;
enum R_PARISC_PCREL14DR =      76;
enum R_PARISC_PCREL16F =       77;
enum R_PARISC_PCREL16WF =      78;
enum R_PARISC_PCREL16DF =      79;
enum R_PARISC_DIR64 =          80;
enum R_PARISC_DIR14WR =        83;
enum R_PARISC_DIR14DR =        84;
enum R_PARISC_DIR16F =         85;
enum R_PARISC_DIR16WF =        86;
enum R_PARISC_DIR16DF =        87;
enum R_PARISC_GPREL64 =        88;
enum R_PARISC_GPREL14WR =      91;
enum R_PARISC_GPREL14DR =      92;
enum R_PARISC_GPREL16F =       93;
enum R_PARISC_GPREL16WF =      94;
enum R_PARISC_GPREL16DF =      95;
enum R_PARISC_LTOFF64 =        96;
enum R_PARISC_LTOFF14WR =      99;
enum R_PARISC_LTOFF14DR =      100;
enum R_PARISC_LTOFF16F =       101;
enum R_PARISC_LTOFF16WF =      102;
enum R_PARISC_LTOFF16DF =      103;
enum R_PARISC_SECREL64 =       104;
enum R_PARISC_SEGREL64 =       112;
enum R_PARISC_PLTOFF14WR =     115;
enum R_PARISC_PLTOFF14DR =     116;
enum R_PARISC_PLTOFF16F =      117;
enum R_PARISC_PLTOFF16WF =     118;
enum R_PARISC_PLTOFF16DF =     119;
enum R_PARISC_LTOFF_FPTR64 =   120;
enum R_PARISC_LTOFF_FPTR14WR = 123;
enum R_PARISC_LTOFF_FPTR14DR = 124;
enum R_PARISC_LTOFF_FPTR16F =  125;
enum R_PARISC_LTOFF_FPTR16WF = 126;
enum R_PARISC_LTOFF_FPTR16DF = 127;
enum R_PARISC_LORESERVE =      128;
enum R_PARISC_COPY =           128;
enum R_PARISC_IPLT =           129;
enum R_PARISC_EPLT =           130;
enum R_PARISC_TPREL32 =        153;
enum R_PARISC_TPREL21L =       154;
enum R_PARISC_TPREL14R =       158;
enum R_PARISC_LTOFF_TP21L =    162;
enum R_PARISC_LTOFF_TP14R =    166;
enum R_PARISC_LTOFF_TP14F =    167;
enum R_PARISC_TPREL64 =        216;
enum R_PARISC_TPREL14WR =      219;
enum R_PARISC_TPREL14DR =      220;
enum R_PARISC_TPREL16F =       221;
enum R_PARISC_TPREL16WF =      222;
enum R_PARISC_TPREL16DF =      223;
enum R_PARISC_LTOFF_TP64 =     224;
enum R_PARISC_LTOFF_TP14WR =   227;
enum R_PARISC_LTOFF_TP14DR =   228;
enum R_PARISC_LTOFF_TP16F =    229;
enum R_PARISC_LTOFF_TP16WF =   230;
enum R_PARISC_LTOFF_TP16DF =   231;
enum R_PARISC_GNU_VTENTRY =    232;
enum R_PARISC_GNU_VTINHERIT =  233;
enum R_PARISC_TLS_GD21L =      234;
enum R_PARISC_TLS_GD14R =      235;
enum R_PARISC_TLS_GDCALL =     236;
enum R_PARISC_TLS_LDM21L =     237;
enum R_PARISC_TLS_LDM14R =     238;
enum R_PARISC_TLS_LDMCALL =    239;
enum R_PARISC_TLS_LDO21L =     240;
enum R_PARISC_TLS_LDO14R =     241;
enum R_PARISC_TLS_DTPMOD32 =   242;
enum R_PARISC_TLS_DTPMOD64 =   243;
enum R_PARISC_TLS_DTPOFF32 =   244;
enum R_PARISC_TLS_DTPOFF64 =   245;
enum R_PARISC_TLS_LE21L =      R_PARISC_TPREL21L;
enum R_PARISC_TLS_LE14R =      R_PARISC_TPREL14R;
enum R_PARISC_TLS_IE21L =      R_PARISC_LTOFF_TP21L;
enum R_PARISC_TLS_IE14R =      R_PARISC_LTOFF_TP14R;
enum R_PARISC_TLS_TPREL32 =    R_PARISC_TPREL32;
enum R_PARISC_TLS_TPREL64 =    R_PARISC_TPREL64;
enum R_PARISC_HIRESERVE =      255;

enum PT_HP_TLS =               (PT_LOOS + 0x0);
enum PT_HP_CORE_NONE =         (PT_LOOS + 0x1);
enum PT_HP_CORE_VERSION =      (PT_LOOS + 0x2);
enum PT_HP_CORE_KERNEL =       (PT_LOOS + 0x3);
enum PT_HP_CORE_COMM =         (PT_LOOS + 0x4);
enum PT_HP_CORE_PROC =         (PT_LOOS + 0x5);
enum PT_HP_CORE_LOADABLE =     (PT_LOOS + 0x6);
enum PT_HP_CORE_STACK =        (PT_LOOS + 0x7);
enum PT_HP_CORE_SHM =          (PT_LOOS + 0x8);
enum PT_HP_CORE_MMF =          (PT_LOOS + 0x9);
enum PT_HP_PARALLEL =          (PT_LOOS + 0x10);
enum PT_HP_FASTBIND =          (PT_LOOS + 0x11);
enum PT_HP_OPT_ANNOT =         (PT_LOOS + 0x12);
enum PT_HP_HSL_ANNOT =         (PT_LOOS + 0x13);
enum PT_HP_STACK =             (PT_LOOS + 0x14);

enum PT_PARISC_ARCHEXT =       0x70000000;
enum PT_PARISC_UNWIND =        0x70000001;

enum PF_PARISC_SBP =           0x08000000;

enum PF_HP_PAGE_SIZE =         0x00100000;
enum PF_HP_FAR_SHARED =        0x00200000;
enum PF_HP_NEAR_SHARED =       0x00400000;
enum PF_HP_CODE =              0x01000000;
enum PF_HP_MODIFY =            0x02000000;
enum PF_HP_LAZYSWAP =          0x04000000;
enum PF_HP_SBP =               0x08000000;

enum EF_ALPHA_32BIT =          1;
enum EF_ALPHA_CANRELAX =       2;
enum SHT_ALPHA_DEBUG =         0x70000001;
enum SHT_ALPHA_REGINFO =       0x70000002;

enum SHF_ALPHA_GPREL =         0x10000000;
enum STO_ALPHA_NOPV =          0x80;
enum STO_ALPHA_STD_GPLOAD =    0x88;

enum R_ALPHA_NONE =            0;
enum R_ALPHA_REFLONG =         1;
enum R_ALPHA_REFQUAD =         2;
enum R_ALPHA_GPREL32 =         3;
enum R_ALPHA_LITERAL =         4;
enum R_ALPHA_LITUSE =          5;
enum R_ALPHA_GPDISP =          6;
enum R_ALPHA_BRADDR =          7;
enum R_ALPHA_HINT =            8;
enum R_ALPHA_SREL16 =          9;
enum R_ALPHA_SREL32 =          10;
enum R_ALPHA_SREL64 =          11;
enum R_ALPHA_GPRELHIGH =       17;
enum R_ALPHA_GPRELLOW =        18;
enum R_ALPHA_GPREL16 =         19;
enum R_ALPHA_COPY =            24;
enum R_ALPHA_GLOB_DAT =        25;
enum R_ALPHA_JMP_SLOT =        26;
enum R_ALPHA_RELATIVE =        27;
enum R_ALPHA_TLS_GD_HI =       28;
enum R_ALPHA_TLSGD =           29;
enum R_ALPHA_TLS_LDM =         30;
enum R_ALPHA_DTPMOD64 =        31;
enum R_ALPHA_GOTDTPREL =       32;
enum R_ALPHA_DTPREL64 =        33;
enum R_ALPHA_DTPRELHI =        34;
enum R_ALPHA_DTPRELLO =        35;
enum R_ALPHA_DTPREL16 =        36;
enum R_ALPHA_GOTTPREL =        37;
enum R_ALPHA_TPREL64 =         38;
enum R_ALPHA_TPRELHI =         39;
enum R_ALPHA_TPRELLO =         40;
enum R_ALPHA_TPREL16 =         41;
enum R_ALPHA_NUM =             46;
enum LITUSE_ALPHA_ADDR =       0;
enum LITUSE_ALPHA_BASE =       1;
enum LITUSE_ALPHA_BYTOFF =     2;
enum LITUSE_ALPHA_JSR =        3;
enum LITUSE_ALPHA_TLS_GD =     4;
enum LITUSE_ALPHA_TLS_LDM =    5;
enum DT_ALPHA_PLTRO =          (DT_LOPROC + 0);
enum DT_ALPHA_NUM =            1;
enum EF_PPC_EMB =              0x80000000;
enum EF_PPC_RELOCATABLE =      0x00010000;
enum EF_PPC_RELOCATABLE_LIB =  0x00008000;
enum R_PPC_NONE =              0;
enum R_PPC_ADDR32 =            1;
enum R_PPC_ADDR24 =            2;
enum R_PPC_ADDR16 =            3;
enum R_PPC_ADDR16_LO =         4;
enum R_PPC_ADDR16_HI =         5;
enum R_PPC_ADDR16_HA =         6;
enum R_PPC_ADDR14 =            7;
enum R_PPC_ADDR14_BRTAKEN =    8;
enum R_PPC_ADDR14_BRNTAKEN =   9;
enum R_PPC_REL24 =             10;
enum R_PPC_REL14 =             11;
enum R_PPC_REL14_BRTAKEN =     12;
enum R_PPC_REL14_BRNTAKEN =    13;
enum R_PPC_GOT16 =             14;
enum R_PPC_GOT16_LO =          15;
enum R_PPC_GOT16_HI =          16;
enum R_PPC_GOT16_HA =          17;
enum R_PPC_PLTREL24 =          18;
enum R_PPC_COPY =              19;
enum R_PPC_GLOB_DAT =          20;
enum R_PPC_JMP_SLOT =          21;
enum R_PPC_RELATIVE =          22;
enum R_PPC_LOCAL24PC =         23;
enum R_PPC_UADDR32 =           24;
enum R_PPC_UADDR16 =           25;
enum R_PPC_REL32 =             26;
enum R_PPC_PLT32 =             27;
enum R_PPC_PLTREL32 =          28;
enum R_PPC_PLT16_LO =          29;
enum R_PPC_PLT16_HI =          30;
enum R_PPC_PLT16_HA =          31;
enum R_PPC_SDAREL16 =          32;
enum R_PPC_SECTOFF =           33;
enum R_PPC_SECTOFF_LO =        34;
enum R_PPC_SECTOFF_HI =        35;
enum R_PPC_SECTOFF_HA =        36;
enum R_PPC_TLS =               67;
enum R_PPC_DTPMOD32 =          68;
enum R_PPC_TPREL16 =           69;
enum R_PPC_TPREL16_LO =        70;
enum R_PPC_TPREL16_HI =        71;
enum R_PPC_TPREL16_HA =        72;
enum R_PPC_TPREL32 =           73;
enum R_PPC_DTPREL16 =          74;
enum R_PPC_DTPREL16_LO =       75;
enum R_PPC_DTPREL16_HI =       76;
enum R_PPC_DTPREL16_HA =       77;
enum R_PPC_DTPREL32 =          78;
enum R_PPC_GOT_TLSGD16 =       79;
enum R_PPC_GOT_TLSGD16_LO =    80;
enum R_PPC_GOT_TLSGD16_HI =    81;
enum R_PPC_GOT_TLSGD16_HA =    82;
enum R_PPC_GOT_TLSLD16 =       83;
enum R_PPC_GOT_TLSLD16_LO =    84;
enum R_PPC_GOT_TLSLD16_HI =    85;
enum R_PPC_GOT_TLSLD16_HA =    86;
enum R_PPC_GOT_TPREL16 =       87;
enum R_PPC_GOT_TPREL16_LO =    88;
enum R_PPC_GOT_TPREL16_HI =    89;
enum R_PPC_GOT_TPREL16_HA =    90;
enum R_PPC_GOT_DTPREL16 =      91;
enum R_PPC_GOT_DTPREL16_LO =   92;
enum R_PPC_GOT_DTPREL16_HI =   93;
enum R_PPC_GOT_DTPREL16_HA =   94;
enum R_PPC_EMB_NADDR32 =       101;
enum R_PPC_EMB_NADDR16 =       102;
enum R_PPC_EMB_NADDR16_LO =    103;
enum R_PPC_EMB_NADDR16_HI =    104;
enum R_PPC_EMB_NADDR16_HA =    105;
enum R_PPC_EMB_SDAI16 =        106;
enum R_PPC_EMB_SDA2I16 =       107;
enum R_PPC_EMB_SDA2REL =       108;
enum R_PPC_EMB_SDA21 =         109;
enum R_PPC_EMB_MRKREF =        110;
enum R_PPC_EMB_RELSEC16 =      111;
enum R_PPC_EMB_RELST_LO =      112;
enum R_PPC_EMB_RELST_HI =      113;
enum R_PPC_EMB_RELST_HA =      114;
enum R_PPC_EMB_BIT_FLD =       115;
enum R_PPC_EMB_RELSDA =        116;
enum R_PPC_DIAB_SDA21_LO =     180;
enum R_PPC_DIAB_SDA21_HI =     181;
enum R_PPC_DIAB_SDA21_HA =     182;
enum R_PPC_DIAB_RELSDA_LO =    183;
enum R_PPC_DIAB_RELSDA_HI =    184;
enum R_PPC_DIAB_RELSDA_HA =    185;
enum R_PPC_IRELATIVE =         248;
enum R_PPC_REL16 =             249;
enum R_PPC_REL16_LO =          250;
enum R_PPC_REL16_HI =          251;
enum R_PPC_REL16_HA =          252;
enum R_PPC_TOC16 =             255;
enum DT_PPC_GOT =              (DT_LOPROC + 0);
enum DT_PPC_NUM =              1;
enum R_PPC64_NONE =            R_PPC_NONE;
enum R_PPC64_ADDR32 =          R_PPC_ADDR32;
enum R_PPC64_ADDR24 =          R_PPC_ADDR24;
enum R_PPC64_ADDR16 =          R_PPC_ADDR16;
enum R_PPC64_ADDR16_LO =       R_PPC_ADDR16_LO;
enum R_PPC64_ADDR16_HI =       R_PPC_ADDR16_HI;
enum R_PPC64_ADDR16_HA =       R_PPC_ADDR16_HA;
enum R_PPC64_ADDR14 =          R_PPC_ADDR14;
enum R_PPC64_ADDR14_BRTAKEN =  R_PPC_ADDR14_BRTAKEN;
enum R_PPC64_ADDR14_BRNTAKEN = R_PPC_ADDR14_BRNTAKEN;
enum R_PPC64_REL24 =           R_PPC_REL24;
enum R_PPC64_REL14 =           R_PPC_REL14;
enum R_PPC64_REL14_BRTAKEN =   R_PPC_REL14_BRTAKEN;
enum R_PPC64_REL14_BRNTAKEN =  R_PPC_REL14_BRNTAKEN;
enum R_PPC64_GOT16 =           R_PPC_GOT16;
enum R_PPC64_GOT16_LO =        R_PPC_GOT16_LO;
enum R_PPC64_GOT16_HI =        R_PPC_GOT16_HI;
enum R_PPC64_GOT16_HA =        R_PPC_GOT16_HA;

enum R_PPC64_COPY =            R_PPC_COPY;
enum R_PPC64_GLOB_DAT =        R_PPC_GLOB_DAT;
enum R_PPC64_JMP_SLOT =        R_PPC_JMP_SLOT;
enum R_PPC64_RELATIVE =        R_PPC_RELATIVE;

enum R_PPC64_UADDR32 =         R_PPC_UADDR32;
enum R_PPC64_UADDR16 =         R_PPC_UADDR16;
enum R_PPC64_REL32 =           R_PPC_REL32;
enum R_PPC64_PLT32 =           R_PPC_PLT32;
enum R_PPC64_PLTREL32 =        R_PPC_PLTREL32;
enum R_PPC64_PLT16_LO =        R_PPC_PLT16_LO;
enum R_PPC64_PLT16_HI =        R_PPC_PLT16_HI;
enum R_PPC64_PLT16_HA =        R_PPC_PLT16_HA;

enum R_PPC64_SECTOFF =         R_PPC_SECTOFF;
enum R_PPC64_SECTOFF_LO =      R_PPC_SECTOFF_LO;
enum R_PPC64_SECTOFF_HI =      R_PPC_SECTOFF_HI;
enum R_PPC64_SECTOFF_HA =      R_PPC_SECTOFF_HA;
enum R_PPC64_ADDR30 =          37;
enum R_PPC64_ADDR64 =          38;
enum R_PPC64_ADDR16_HIGHER =   39;
enum R_PPC64_ADDR16_HIGHERA =  40;
enum R_PPC64_ADDR16_HIGHEST =  41;
enum R_PPC64_ADDR16_HIGHESTA = 42;
enum R_PPC64_UADDR64 =         43;
enum R_PPC64_REL64 =           44;
enum R_PPC64_PLT64 =           45;
enum R_PPC64_PLTREL64 =        46;
enum R_PPC64_TOC16 =           47;
enum R_PPC64_TOC16_LO =        48;
enum R_PPC64_TOC16_HI =        49;
enum R_PPC64_TOC16_HA =        50;
enum R_PPC64_TOC =             51;
enum R_PPC64_PLTGOT16 =        52;
enum R_PPC64_PLTGOT16_LO =     53;
enum R_PPC64_PLTGOT16_HI =     54;
enum R_PPC64_PLTGOT16_HA =     55;

enum R_PPC64_ADDR16_DS =       56;
enum R_PPC64_ADDR16_LO_DS =    57;
enum R_PPC64_GOT16_DS =        58;
enum R_PPC64_GOT16_LO_DS =     59;
enum R_PPC64_PLT16_LO_DS =     60;
enum R_PPC64_SECTOFF_DS =      61;
enum R_PPC64_SECTOFF_LO_DS =   62;
enum R_PPC64_TOC16_DS =        63;
enum R_PPC64_TOC16_LO_DS =     64;
enum R_PPC64_PLTGOT16_DS =     65;
enum R_PPC64_PLTGOT16_LO_DS =  66;
enum R_PPC64_TLS =             67;
enum R_PPC64_DTPMOD64 =        68;
enum R_PPC64_TPREL16 =         69;
enum R_PPC64_TPREL16_LO =      70;
enum R_PPC64_TPREL16_HI =      71;
enum R_PPC64_TPREL16_HA =      72;
enum R_PPC64_TPREL64 =         73;
enum R_PPC64_DTPREL16 =        74;
enum R_PPC64_DTPREL16_LO =     75;
enum R_PPC64_DTPREL16_HI =     76;
enum R_PPC64_DTPREL16_HA =     77;
enum R_PPC64_DTPREL64 =        78;
enum R_PPC64_GOT_TLSGD16 =     79;
enum R_PPC64_GOT_TLSGD16_LO =  80;
enum R_PPC64_GOT_TLSGD16_HI =  81;
enum R_PPC64_GOT_TLSGD16_HA =  82;
enum R_PPC64_GOT_TLSLD16 =     83;
enum R_PPC64_GOT_TLSLD16_LO =  84;
enum R_PPC64_GOT_TLSLD16_HI =  85;
enum R_PPC64_GOT_TLSLD16_HA =  86;
enum R_PPC64_GOT_TPREL16_DS =  87;
enum R_PPC64_GOT_TPREL16_LO_DS = 88;
enum R_PPC64_GOT_TPREL16_HI =  89;
enum R_PPC64_GOT_TPREL16_HA =  90;
enum R_PPC64_GOT_DTPREL16_DS = 91;
enum R_PPC64_GOT_DTPREL16_LO_DS = 92;
enum R_PPC64_GOT_DTPREL16_HI = 93;
enum R_PPC64_GOT_DTPREL16_HA = 94;
enum R_PPC64_TPREL16_DS =      95;
enum R_PPC64_TPREL16_LO_DS =   96;
enum R_PPC64_TPREL16_HIGHER =  97;
enum R_PPC64_TPREL16_HIGHERA = 98;
enum R_PPC64_TPREL16_HIGHEST = 99;
enum R_PPC64_TPREL16_HIGHESTA = 100;
enum R_PPC64_DTPREL16_DS =     101;
enum R_PPC64_DTPREL16_LO_DS =  102;
enum R_PPC64_DTPREL16_HIGHER = 103;
enum R_PPC64_DTPREL16_HIGHERA = 104;
enum R_PPC64_DTPREL16_HIGHEST = 105;
enum R_PPC64_DTPREL16_HIGHESTA = 106;
enum R_PPC64_JMP_IREL =        247;
enum R_PPC64_IRELATIVE =       248;
enum R_PPC64_REL16 =           249;
enum R_PPC64_REL16_LO =        250;
enum R_PPC64_REL16_HI =        251;
enum R_PPC64_REL16_HA =        252;
enum DT_PPC64_GLINK =  (DT_LOPROC + 0);
enum DT_PPC64_OPD =    (DT_LOPROC + 1);
enum DT_PPC64_OPDSZ =  (DT_LOPROC + 2);
enum DT_PPC64_NUM =    3;
enum EF_ARM_RELEXEC =          0x01;
enum EF_ARM_HASENTRY =         0x02;
enum EF_ARM_INTERWORK =        0x04;
enum EF_ARM_APCS_26 =          0x08;
enum EF_ARM_APCS_FLOAT =       0x10;
enum EF_ARM_PIC =              0x20;
enum EF_ARM_ALIGN8 =           0x40;
enum EF_ARM_NEW_ABI =          0x80;
enum EF_ARM_OLD_ABI =          0x100;
enum EF_ARM_SOFT_FLOAT =       0x200;
enum EF_ARM_VFP_FLOAT =        0x400;
enum EF_ARM_MAVERICK_FLOAT =   0x800;

enum EF_ARM_ABI_FLOAT_SOFT =   0x200;
enum EF_ARM_ABI_FLOAT_HARD =   0x400;
enum EF_ARM_SYMSARESORTED =    0x04;
enum EF_ARM_DYNSYMSUSESEGIDX = 0x08;
enum EF_ARM_MAPSYMSFIRST =     0x10;
enum EF_ARM_EABIMASK =         0XFF000000;
enum EF_ARM_BE8 =          0x00800000;
enum EF_ARM_LE8 =          0x00400000;

extern (D) auto EF_ARM_EABI_VERSION(F)(F flags) { return flags & EF_ARM_EABIMASK; }
enum EF_ARM_EABI_UNKNOWN =     0x00000000;
enum EF_ARM_EABI_VER1 =        0x01000000;
enum EF_ARM_EABI_VER2 =        0x02000000;
enum EF_ARM_EABI_VER3 =        0x03000000;
enum EF_ARM_EABI_VER4 =        0x04000000;
enum EF_ARM_EABI_VER5 =        0x05000000;
enum STT_ARM_TFUNC =           STT_LOPROC;
enum STT_ARM_16BIT =           STT_HIPROC;
enum SHF_ARM_ENTRYSECT =       0x10000000;
enum SHF_ARM_COMDEF =          0x80000000;
enum PF_ARM_SB =               0x10000000;
enum PF_ARM_PI =               0x20000000;
enum PF_ARM_ABS =              0x40000000;
enum PT_ARM_EXIDX =            (PT_LOPROC + 1);
enum SHT_ARM_EXIDX =           (SHT_LOPROC + 1);
enum SHT_ARM_PREEMPTMAP =      (SHT_LOPROC + 2);
enum SHT_ARM_ATTRIBUTES =      (SHT_LOPROC + 3);

enum R_AARCH64_NONE =            0;
enum R_AARCH64_ABS64 =         257;
enum R_AARCH64_ABS32 =         258;
enum R_AARCH64_COPY =         1024;
enum R_AARCH64_GLOB_DAT =     1025;
enum R_AARCH64_JUMP_SLOT =    1026;
enum R_AARCH64_RELATIVE =     1027;
enum R_AARCH64_TLS_DTPMOD64 = 1028;
enum R_AARCH64_TLS_DTPREL64 = 1029;
enum R_AARCH64_TLS_TPREL64 =  1030;
enum R_AARCH64_TLSDESC =      1031;

enum R_ARM_NONE =              0;
enum R_ARM_PC24 =              1;
enum R_ARM_ABS32 =             2;
enum R_ARM_REL32 =             3;
enum R_ARM_PC13 =              4;
enum R_ARM_ABS16 =             5;
enum R_ARM_ABS12 =             6;
enum R_ARM_THM_ABS5 =          7;
enum R_ARM_ABS8 =              8;
enum R_ARM_SBREL32 =           9;
enum R_ARM_THM_PC22 =          10;
enum R_ARM_THM_PC8 =           11;
enum R_ARM_AMP_VCALL9 =        12;
enum R_ARM_SWI24 =             13;
enum R_ARM_TLS_DESC =          13;
enum R_ARM_THM_SWI8 =          14;
enum R_ARM_XPC25 =             15;
enum R_ARM_THM_XPC22 =         16;
enum R_ARM_TLS_DTPMOD32 =      17;
enum R_ARM_TLS_DTPOFF32 =      18;
enum R_ARM_TLS_TPOFF32 =       19;
enum R_ARM_COPY =              20;
enum R_ARM_GLOB_DAT =          21;
enum R_ARM_JUMP_SLOT =         22;
enum R_ARM_RELATIVE =          23;
enum R_ARM_GOTOFF =            24;
enum R_ARM_GOTPC =             25;
enum R_ARM_GOT32 =             26;
enum R_ARM_PLT32 =             27;
enum R_ARM_ALU_PCREL_7_0 =     32;
enum R_ARM_ALU_PCREL_15_8 =    33;
enum R_ARM_ALU_PCREL_23_15 =   34;
enum R_ARM_LDR_SBREL_11_0 =    35;
enum R_ARM_ALU_SBREL_19_12 =   36;
enum R_ARM_ALU_SBREL_27_20 =   37;
enum R_ARM_TLS_GOTDESC =       90;
enum R_ARM_TLS_CALL =          91;
enum R_ARM_TLS_DESCSEQ =       92;
enum R_ARM_THM_TLS_CALL =      93;
enum R_ARM_GNU_VTENTRY =       100;
enum R_ARM_GNU_VTINHERIT =     101;
enum R_ARM_THM_PC11 =          102;
enum R_ARM_THM_PC9 =           103;
enum R_ARM_TLS_GD32 =          104;
enum R_ARM_TLS_LDM32 =         105;
enum R_ARM_TLS_LDO32 =         106;
enum R_ARM_TLS_IE32 =          107;
enum R_ARM_TLS_LE32 =          108;
enum R_ARM_THM_TLS_DESCSEQ =   129;
enum R_ARM_IRELATIVE =         160;
enum R_ARM_RXPC25 =            249;
enum R_ARM_RSBREL32 =          250;
enum R_ARM_THM_RPC22 =         251;
enum R_ARM_RREL32 =            252;
enum R_ARM_RABS22 =            253;
enum R_ARM_RPC24 =             254;
enum R_ARM_RBASE =             255;
enum R_ARM_NUM =               256;
enum EF_IA_64_MASKOS =         0x0000000f;
enum EF_IA_64_ABI64 =          0x00000010;
enum EF_IA_64_ARCH =           0xff000000;
enum PT_IA_64_ARCHEXT =        (PT_LOPROC + 0);
enum PT_IA_64_UNWIND =         (PT_LOPROC + 1);
enum PT_IA_64_HP_OPT_ANOT =    (PT_LOOS + 0x12);
enum PT_IA_64_HP_HSL_ANOT =    (PT_LOOS + 0x13);
enum PT_IA_64_HP_STACK =       (PT_LOOS + 0x14);
enum PF_IA_64_NORECOV =        0x80000000;
enum SHT_IA_64_EXT =           (SHT_LOPROC + 0);
enum SHT_IA_64_UNWIND =        (SHT_LOPROC + 1);
enum SHF_IA_64_SHORT =         0x10000000;
enum SHF_IA_64_NORECOV =       0x20000000;
enum DT_IA_64_PLT_RESERVE =    (DT_LOPROC + 0);
enum DT_IA_64_NUM =            1;
enum R_IA64_NONE =             0x00;
enum R_IA64_IMM14 =            0x21;
enum R_IA64_IMM22 =            0x22;
enum R_IA64_IMM64 =            0x23;
enum R_IA64_DIR32MSB =         0x24;
enum R_IA64_DIR32LSB =         0x25;
enum R_IA64_DIR64MSB =         0x26;
enum R_IA64_DIR64LSB =         0x27;
enum R_IA64_GPREL22 =          0x2a;
enum R_IA64_GPREL64I =         0x2b;
enum R_IA64_GPREL32MSB =       0x2c;
enum R_IA64_GPREL32LSB =       0x2d;
enum R_IA64_GPREL64MSB =       0x2e;
enum R_IA64_GPREL64LSB =       0x2f;
enum R_IA64_LTOFF22 =          0x32;
enum R_IA64_LTOFF64I =         0x33;
enum R_IA64_PLTOFF22 =         0x3a;
enum R_IA64_PLTOFF64I =        0x3b;
enum R_IA64_PLTOFF64MSB =      0x3e;
enum R_IA64_PLTOFF64LSB =      0x3f;
enum R_IA64_FPTR64I =          0x43;
enum R_IA64_FPTR32MSB =        0x44;
enum R_IA64_FPTR32LSB =        0x45;
enum R_IA64_FPTR64MSB =        0x46;
enum R_IA64_FPTR64LSB =        0x47;
enum R_IA64_PCREL60B =         0x48;
enum R_IA64_PCREL21B =         0x49;
enum R_IA64_PCREL21M =         0x4a;
enum R_IA64_PCREL21F =         0x4b;
enum R_IA64_PCREL32MSB =       0x4c;
enum R_IA64_PCREL32LSB =       0x4d;
enum R_IA64_PCREL64MSB =       0x4e;
enum R_IA64_PCREL64LSB =       0x4f;
enum R_IA64_LTOFF_FPTR22 =     0x52;
enum R_IA64_LTOFF_FPTR64I =    0x53;
enum R_IA64_LTOFF_FPTR32MSB =  0x54;
enum R_IA64_LTOFF_FPTR32LSB =  0x55;
enum R_IA64_LTOFF_FPTR64MSB =  0x56;
enum R_IA64_LTOFF_FPTR64LSB =  0x57;
enum R_IA64_SEGREL32MSB =      0x5c;
enum R_IA64_SEGREL32LSB =      0x5d;
enum R_IA64_SEGREL64MSB =      0x5e;
enum R_IA64_SEGREL64LSB =      0x5f;
enum R_IA64_SECREL32MSB =      0x64;
enum R_IA64_SECREL32LSB =      0x65;
enum R_IA64_SECREL64MSB =      0x66;
enum R_IA64_SECREL64LSB =      0x67;
enum R_IA64_REL32MSB =         0x6c;
enum R_IA64_REL32LSB =         0x6d;
enum R_IA64_REL64MSB =         0x6e;
enum R_IA64_REL64LSB =         0x6f;
enum R_IA64_LTV32MSB =         0x74;
enum R_IA64_LTV32LSB =         0x75;
enum R_IA64_LTV64MSB =         0x76;
enum R_IA64_LTV64LSB =         0x77;
enum R_IA64_PCREL21BI =        0x79;
enum R_IA64_PCREL22 =          0x7a;
enum R_IA64_PCREL64I =         0x7b;
enum R_IA64_IPLTMSB =          0x80;
enum R_IA64_IPLTLSB =          0x81;
enum R_IA64_COPY =             0x84;
enum R_IA64_SUB =              0x85;
enum R_IA64_LTOFF22X =         0x86;
enum R_IA64_LDXMOV =           0x87;
enum R_IA64_TPREL14 =          0x91;
enum R_IA64_TPREL22 =          0x92;
enum R_IA64_TPREL64I =         0x93;
enum R_IA64_TPREL64MSB =       0x96;
enum R_IA64_TPREL64LSB =       0x97;
enum R_IA64_LTOFF_TPREL22 =    0x9a;
enum R_IA64_DTPMOD64MSB =      0xa6;
enum R_IA64_DTPMOD64LSB =      0xa7;
enum R_IA64_LTOFF_DTPMOD22 =   0xaa;
enum R_IA64_DTPREL14 =         0xb1;
enum R_IA64_DTPREL22 =         0xb2;
enum R_IA64_DTPREL64I =        0xb3;
enum R_IA64_DTPREL32MSB =      0xb4;
enum R_IA64_DTPREL32LSB =      0xb5;
enum R_IA64_DTPREL64MSB =      0xb6;
enum R_IA64_DTPREL64LSB =      0xb7;
enum R_IA64_LTOFF_DTPREL22 =   0xba;
enum EF_SH_MACH_MASK =         0x1f;
enum EF_SH_UNKNOWN =           0x0;
enum EF_SH1 =                  0x1;
enum EF_SH2 =                  0x2;
enum EF_SH3 =                  0x3;
enum EF_SH_DSP =               0x4;
enum EF_SH3_DSP =              0x5;
enum EF_SH4AL_DSP =            0x6;
enum EF_SH3E =                 0x8;
enum EF_SH4 =                  0x9;
enum EF_SH2E =                 0xb;
enum EF_SH4A =                 0xc;
enum EF_SH2A =                 0xd;
enum EF_SH4_NOFPU =            0x10;
enum EF_SH4A_NOFPU =           0x11;
enum EF_SH4_NOMMU_NOFPU =      0x12;
enum EF_SH2A_NOFPU =           0x13;
enum EF_SH3_NOMMU =            0x14;
enum EF_SH2A_SH4_NOFPU =       0x15;
enum EF_SH2A_SH3_NOFPU =       0x16;
enum EF_SH2A_SH4 =             0x17;
enum EF_SH2A_SH3E =            0x18;
enum R_SH_NONE =               0;
enum R_SH_DIR32 =              1;
enum R_SH_REL32 =              2;
enum R_SH_DIR8WPN =            3;
enum R_SH_IND12W =             4;
enum R_SH_DIR8WPL =            5;
enum R_SH_DIR8WPZ =            6;
enum R_SH_DIR8BP =             7;
enum R_SH_DIR8W =              8;
enum R_SH_DIR8L =              9;
enum R_SH_SWITCH16 =           25;
enum R_SH_SWITCH32 =           26;
enum R_SH_USES =               27;
enum R_SH_COUNT =              28;
enum R_SH_ALIGN =              29;
enum R_SH_CODE =               30;
enum R_SH_DATA =               31;
enum R_SH_LABEL =              32;
enum R_SH_SWITCH8 =            33;
enum R_SH_GNU_VTINHERIT =      34;
enum R_SH_GNU_VTENTRY =        35;
enum R_SH_TLS_GD_32 =          144;
enum R_SH_TLS_LD_32 =          145;
enum R_SH_TLS_LDO_32 =         146;
enum R_SH_TLS_IE_32 =          147;
enum R_SH_TLS_LE_32 =          148;
enum R_SH_TLS_DTPMOD32 =       149;
enum R_SH_TLS_DTPOFF32 =       150;
enum R_SH_TLS_TPOFF32 =        151;
enum R_SH_GOT32 =              160;
enum R_SH_PLT32 =              161;
enum R_SH_COPY =               162;
enum R_SH_GLOB_DAT =           163;
enum R_SH_JMP_SLOT =           164;
enum R_SH_RELATIVE =           165;
enum R_SH_GOTOFF =             166;
enum R_SH_GOTPC =              167;
enum R_SH_NUM =                256;

enum EF_S390_HIGH_GPRS =    0x00000001;

enum R_390_NONE =              0;
enum R_390_8 =                 1;
enum R_390_12 =                2;
enum R_390_16 =                3;
enum R_390_32 =                4;
enum R_390_PC32 =              5;
enum R_390_GOT12 =             6;
enum R_390_GOT32 =             7;
enum R_390_PLT32 =             8;
enum R_390_COPY =              9;
enum R_390_GLOB_DAT =          10;
enum R_390_JMP_SLOT =          11;
enum R_390_RELATIVE =          12;
enum R_390_GOTOFF32 =          13;
enum R_390_GOTPC =             14;
enum R_390_GOT16 =             15;
enum R_390_PC16 =              16;
enum R_390_PC16DBL =           17;
enum R_390_PLT16DBL =          18;
enum R_390_PC32DBL =           19;
enum R_390_PLT32DBL =          20;
enum R_390_GOTPCDBL =          21;
enum R_390_64 =                22;
enum R_390_PC64 =              23;
enum R_390_GOT64 =             24;
enum R_390_PLT64 =             25;
enum R_390_GOTENT =            26;
enum R_390_GOTOFF16 =          27;
enum R_390_GOTOFF64 =          28;
enum R_390_GOTPLT12 =          29;
enum R_390_GOTPLT16 =          30;
enum R_390_GOTPLT32 =          31;
enum R_390_GOTPLT64 =          32;
enum R_390_GOTPLTENT =         33;
enum R_390_PLTOFF16 =          34;
enum R_390_PLTOFF32 =          35;
enum R_390_PLTOFF64 =          36;
enum R_390_TLS_LOAD =          37;
enum R_390_TLS_GDCALL =        38;
enum R_390_TLS_LDCALL =        39;
enum R_390_TLS_GD32 =          40;
enum R_390_TLS_GD64 =          41;
enum R_390_TLS_GOTIE12 =       42;
enum R_390_TLS_GOTIE32 =       43;
enum R_390_TLS_GOTIE64 =       44;
enum R_390_TLS_LDM32 =         45;
enum R_390_TLS_LDM64 =         46;
enum R_390_TLS_IE32 =          47;
enum R_390_TLS_IE64 =          48;
enum R_390_TLS_IEENT =         49;
enum R_390_TLS_LE32 =          50;
enum R_390_TLS_LE64 =          51;
enum R_390_TLS_LDO32 =         52;
enum R_390_TLS_LDO64 =         53;
enum R_390_TLS_DTPMOD =        54;
enum R_390_TLS_DTPOFF =        55;
enum R_390_TLS_TPOFF =         56;
enum R_390_20 =                57;
enum R_390_GOT20 =             58;
enum R_390_GOTPLT20 =          59;
enum R_390_TLS_GOTIE20 =       60;
enum R_390_IRELATIVE =         61;
enum R_390_NUM =               62;
enum R_CRIS_NONE =             0;
enum R_CRIS_8 =                1;
enum R_CRIS_16 =               2;
enum R_CRIS_32 =               3;
enum R_CRIS_8_PCREL =          4;
enum R_CRIS_16_PCREL =         5;
enum R_CRIS_32_PCREL =         6;
enum R_CRIS_GNU_VTINHERIT =    7;
enum R_CRIS_GNU_VTENTRY =      8;
enum R_CRIS_COPY =             9;
enum R_CRIS_GLOB_DAT =         10;
enum R_CRIS_JUMP_SLOT =        11;
enum R_CRIS_RELATIVE =         12;
enum R_CRIS_16_GOT =           13;
enum R_CRIS_32_GOT =           14;
enum R_CRIS_16_GOTPLT =        15;
enum R_CRIS_32_GOTPLT =        16;
enum R_CRIS_32_GOTREL =        17;
enum R_CRIS_32_PLT_GOTREL =    18;
enum R_CRIS_32_PLT_PCREL =     19;

enum R_CRIS_NUM =              20;
enum R_X86_64_NONE =           0;
enum R_X86_64_64 =             1;
enum R_X86_64_PC32 =           2;
enum R_X86_64_GOT32 =          3;
enum R_X86_64_PLT32 =          4;
enum R_X86_64_COPY =           5;
enum R_X86_64_GLOB_DAT =       6;
enum R_X86_64_JUMP_SLOT =      7;
enum R_X86_64_RELATIVE =       8;
enum R_X86_64_GOTPCREL =       9;
enum R_X86_64_32 =             10;
enum R_X86_64_32S =            11;
enum R_X86_64_16 =             12;
enum R_X86_64_PC16 =           13;
enum R_X86_64_8 =              14;
enum R_X86_64_PC8 =            15;
enum R_X86_64_DTPMOD64 =       16;
enum R_X86_64_DTPOFF64 =       17;
enum R_X86_64_TPOFF64 =        18;
enum R_X86_64_TLSGD =          19;
enum R_X86_64_TLSLD =          20;
enum R_X86_64_DTPOFF32 =       21;
enum R_X86_64_GOTTPOFF =       22;
enum R_X86_64_TPOFF32 =        23;
enum R_X86_64_PC64 =           24;
enum R_X86_64_GOTOFF64 =       25;
enum R_X86_64_GOTPC32 =        26;
enum R_X86_64_GOT64 =          27;
enum R_X86_64_GOTPCREL64 =     28;
enum R_X86_64_GOTPC64 =        29;
enum R_X86_64_GOTPLT64 =       30;
enum R_X86_64_PLTOFF64 =       31;
enum R_X86_64_SIZE32 =         32;
enum R_X86_64_SIZE64 =         33;
enum R_X86_64_GOTPC32_TLSDESC = 34;
enum R_X86_64_TLSDESC_CALL =   35;
enum R_X86_64_TLSDESC =        36;
enum R_X86_64_IRELATIVE =      37;
enum R_X86_64_RELATIVE64 =     38;

enum R_X86_64_NUM =            39;
enum R_MN10300_NONE =          0;
enum R_MN10300_32 =            1;
enum R_MN10300_16 =            2;
enum R_MN10300_8 =             3;
enum R_MN10300_PCREL32 =       4;
enum R_MN10300_PCREL16 =       5;
enum R_MN10300_PCREL8 =        6;
enum R_MN10300_GNU_VTINHERIT = 7;
enum R_MN10300_GNU_VTENTRY =   8;
enum R_MN10300_24 =            9;
enum R_MN10300_GOTPC32 =       10;
enum R_MN10300_GOTPC16 =       11;
enum R_MN10300_GOTOFF32 =      12;
enum R_MN10300_GOTOFF24 =      13;
enum R_MN10300_GOTOFF16 =      14;
enum R_MN10300_PLT32 =         15;
enum R_MN10300_PLT16 =         16;
enum R_MN10300_GOT32 =         17;
enum R_MN10300_GOT24 =         18;
enum R_MN10300_GOT16 =         19;
enum R_MN10300_COPY =          20;
enum R_MN10300_GLOB_DAT =      21;
enum R_MN10300_JMP_SLOT =      22;
enum R_MN10300_RELATIVE =      23;
enum R_MN10300_TLS_GD =        24;
enum R_MN10300_TLS_LD =        25;
enum R_MN10300_TLS_LDO =       26;
enum R_MN10300_TLS_GOTIE =     27;
enum R_MN10300_TLS_IE =        28;
enum R_MN10300_TLS_LE =        29;
enum R_MN10300_TLS_DTPMOD =    30;
enum R_MN10300_TLS_DTPOFF =    31;
enum R_MN10300_TLS_TPOFF =     32;
enum R_MN10300_SYM_DIFF =      33;
enum R_MN10300_ALIGN =         34;
enum R_MN10300_NUM =           35;
enum R_M32R_NONE =             0;
enum R_M32R_16 =               1;
enum R_M32R_32 =               2;
enum R_M32R_24 =               3;
enum R_M32R_10_PCREL =         4;
enum R_M32R_18_PCREL =         5;
enum R_M32R_26_PCREL =         6;
enum R_M32R_HI16_ULO =         7;
enum R_M32R_HI16_SLO =         8;
enum R_M32R_LO16 =             9;
enum R_M32R_SDA16 =            10;
enum R_M32R_GNU_VTINHERIT =    11;
enum R_M32R_GNU_VTENTRY =      12;
enum R_M32R_16_RELA =          33;
enum R_M32R_32_RELA =          34;
enum R_M32R_24_RELA =          35;
enum R_M32R_10_PCREL_RELA =    36;
enum R_M32R_18_PCREL_RELA =    37;
enum R_M32R_26_PCREL_RELA =    38;
enum R_M32R_HI16_ULO_RELA =    39;
enum R_M32R_HI16_SLO_RELA =    40;
enum R_M32R_LO16_RELA =        41;
enum R_M32R_SDA16_RELA =       42;
enum R_M32R_RELA_GNU_VTINHERIT =       43;
enum R_M32R_RELA_GNU_VTENTRY = 44;
enum R_M32R_REL32 =            45;

enum R_M32R_GOT24 =            48;
enum R_M32R_26_PLTREL =        49;
enum R_M32R_COPY =             50;
enum R_M32R_GLOB_DAT =         51;
enum R_M32R_JMP_SLOT =         52;
enum R_M32R_RELATIVE =         53;
enum R_M32R_GOTOFF =           54;
enum R_M32R_GOTPC24 =          55;
enum R_M32R_GOT16_HI_ULO =     56;
enum R_M32R_GOT16_HI_SLO =     57;
enum R_M32R_GOT16_LO =         58;
enum R_M32R_GOTPC_HI_ULO =     59;
enum R_M32R_GOTPC_HI_SLO =     60;
enum R_M32R_GOTPC_LO =         61;
enum R_M32R_GOTOFF_HI_ULO =    62;
enum R_M32R_GOTOFF_HI_SLO =    63;
enum R_M32R_GOTOFF_LO =        64;
enum R_M32R_NUM =              256;
enum R_TILEPRO_NONE =          0;
enum R_TILEPRO_32 =            1;
enum R_TILEPRO_16 =            2;
enum R_TILEPRO_8 =             3;
enum R_TILEPRO_32_PCREL =      4;
enum R_TILEPRO_16_PCREL =      5;
enum R_TILEPRO_8_PCREL =       6;
enum R_TILEPRO_LO16 =          7;
enum R_TILEPRO_HI16 =          8;
enum R_TILEPRO_HA16 =          9;
enum R_TILEPRO_COPY =          10;
enum R_TILEPRO_GLOB_DAT =      11;
enum R_TILEPRO_JMP_SLOT =      12;
enum R_TILEPRO_RELATIVE =      13;
enum R_TILEPRO_BROFF_X1 =      14;
enum R_TILEPRO_JOFFLONG_X1 =   15;
enum R_TILEPRO_JOFFLONG_X1_PLT = 16;
enum R_TILEPRO_IMM8_X0 =       17;
enum R_TILEPRO_IMM8_Y0 =       18;
enum R_TILEPRO_IMM8_X1 =       19;
enum R_TILEPRO_IMM8_Y1 =       20;
enum R_TILEPRO_MT_IMM15_X1 =   21;
enum R_TILEPRO_MF_IMM15_X1 =   22;
enum R_TILEPRO_IMM16_X0 =      23;
enum R_TILEPRO_IMM16_X1 =      24;
enum R_TILEPRO_IMM16_X0_LO =   25;
enum R_TILEPRO_IMM16_X1_LO =   26;
enum R_TILEPRO_IMM16_X0_HI =   27;
enum R_TILEPRO_IMM16_X1_HI =   28;
enum R_TILEPRO_IMM16_X0_HA =   29;
enum R_TILEPRO_IMM16_X1_HA =   30;
enum R_TILEPRO_IMM16_X0_PCREL = 31;
enum R_TILEPRO_IMM16_X1_PCREL = 32;
enum R_TILEPRO_IMM16_X0_LO_PCREL = 33;
enum R_TILEPRO_IMM16_X1_LO_PCREL = 34;
enum R_TILEPRO_IMM16_X0_HI_PCREL = 35;
enum R_TILEPRO_IMM16_X1_HI_PCREL = 36;
enum R_TILEPRO_IMM16_X0_HA_PCREL = 37;
enum R_TILEPRO_IMM16_X1_HA_PCREL = 38;
enum R_TILEPRO_IMM16_X0_GOT =  39;
enum R_TILEPRO_IMM16_X1_GOT =  40;
enum R_TILEPRO_IMM16_X0_GOT_LO = 41;
enum R_TILEPRO_IMM16_X1_GOT_LO = 42;
enum R_TILEPRO_IMM16_X0_GOT_HI = 43;
enum R_TILEPRO_IMM16_X1_GOT_HI = 44;
enum R_TILEPRO_IMM16_X0_GOT_HA = 45;
enum R_TILEPRO_IMM16_X1_GOT_HA = 46;
enum R_TILEPRO_MMSTART_X0 =    47;
enum R_TILEPRO_MMEND_X0 =      48;
enum R_TILEPRO_MMSTART_X1 =    49;
enum R_TILEPRO_MMEND_X1 =      50;
enum R_TILEPRO_SHAMT_X0 =      51;
enum R_TILEPRO_SHAMT_X1 =      52;
enum R_TILEPRO_SHAMT_Y0 =      53;
enum R_TILEPRO_SHAMT_Y1 =      54;
enum R_TILEPRO_DEST_IMM8_X1 =  55;
enum R_TILEPRO_TLS_GD_CALL =   60;
enum R_TILEPRO_IMM8_X0_TLS_GD_ADD = 61;
enum R_TILEPRO_IMM8_X1_TLS_GD_ADD = 62;
enum R_TILEPRO_IMM8_Y0_TLS_GD_ADD = 63;
enum R_TILEPRO_IMM8_Y1_TLS_GD_ADD = 64;
enum R_TILEPRO_TLS_IE_LOAD =   65;
enum R_TILEPRO_IMM16_X0_TLS_GD = 66;
enum R_TILEPRO_IMM16_X1_TLS_GD = 67;
enum R_TILEPRO_IMM16_X0_TLS_GD_LO = 68;
enum R_TILEPRO_IMM16_X1_TLS_GD_LO = 69;
enum R_TILEPRO_IMM16_X0_TLS_GD_HI = 70;
enum R_TILEPRO_IMM16_X1_TLS_GD_HI = 71;
enum R_TILEPRO_IMM16_X0_TLS_GD_HA = 72;
enum R_TILEPRO_IMM16_X1_TLS_GD_HA = 73;
enum R_TILEPRO_IMM16_X0_TLS_IE = 74;
enum R_TILEPRO_IMM16_X1_TLS_IE = 75;
enum R_TILEPRO_IMM16_X0_TLS_IE_LO = 76;
enum R_TILEPRO_IMM16_X1_TLS_IE_LO = 77;
enum R_TILEPRO_IMM16_X0_TLS_IE_HI = 78;
enum R_TILEPRO_IMM16_X1_TLS_IE_HI = 79;
enum R_TILEPRO_IMM16_X0_TLS_IE_HA = 80;
enum R_TILEPRO_IMM16_X1_TLS_IE_HA = 81;
enum R_TILEPRO_TLS_DTPMOD32 =  82;
enum R_TILEPRO_TLS_DTPOFF32 =  83;
enum R_TILEPRO_TLS_TPOFF32 =   84;
enum R_TILEPRO_IMM16_X0_TLS_LE = 85;
enum R_TILEPRO_IMM16_X1_TLS_LE = 86;
enum R_TILEPRO_IMM16_X0_TLS_LE_LO = 87;
enum R_TILEPRO_IMM16_X1_TLS_LE_LO = 88;
enum R_TILEPRO_IMM16_X0_TLS_LE_HI = 89;
enum R_TILEPRO_IMM16_X1_TLS_LE_HI = 90;
enum R_TILEPRO_IMM16_X0_TLS_LE_HA = 91;
enum R_TILEPRO_IMM16_X1_TLS_LE_HA = 92;

enum R_TILEPRO_GNU_VTINHERIT = 128;
enum R_TILEPRO_GNU_VTENTRY =   129;

enum R_TILEPRO_NUM =           130;
enum R_TILEGX_NONE =           0;
enum R_TILEGX_64 =             1;
enum R_TILEGX_32 =             2;
enum R_TILEGX_16 =             3;
enum R_TILEGX_8 =              4;
enum R_TILEGX_64_PCREL =       5;
enum R_TILEGX_32_PCREL =       6;
enum R_TILEGX_16_PCREL =       7;
enum R_TILEGX_8_PCREL =        8;
enum R_TILEGX_HW0 =            9;
enum R_TILEGX_HW1 =            10;
enum R_TILEGX_HW2 =            11;
enum R_TILEGX_HW3 =            12;
enum R_TILEGX_HW0_LAST =       13;
enum R_TILEGX_HW1_LAST =       14;
enum R_TILEGX_HW2_LAST =       15;
enum R_TILEGX_COPY =           16;
enum R_TILEGX_GLOB_DAT =       17;
enum R_TILEGX_JMP_SLOT =       18;
enum R_TILEGX_RELATIVE =       19;
enum R_TILEGX_BROFF_X1 =       20;
enum R_TILEGX_JUMPOFF_X1 =     21;
enum R_TILEGX_JUMPOFF_X1_PLT = 22;
enum R_TILEGX_IMM8_X0 =        23;
enum R_TILEGX_IMM8_Y0 =        24;
enum R_TILEGX_IMM8_X1 =        25;
enum R_TILEGX_IMM8_Y1 =        26;
enum R_TILEGX_DEST_IMM8_X1 =   27;
enum R_TILEGX_MT_IMM14_X1 =    28;
enum R_TILEGX_MF_IMM14_X1 =    29;
enum R_TILEGX_MMSTART_X0 =     30;
enum R_TILEGX_MMEND_X0 =       31;
enum R_TILEGX_SHAMT_X0 =       32;
enum R_TILEGX_SHAMT_X1 =       33;
enum R_TILEGX_SHAMT_Y0 =       34;
enum R_TILEGX_SHAMT_Y1 =       35;
enum R_TILEGX_IMM16_X0_HW0 =   36;
enum R_TILEGX_IMM16_X1_HW0 =   37;
enum R_TILEGX_IMM16_X0_HW1 =   38;
enum R_TILEGX_IMM16_X1_HW1 =   39;
enum R_TILEGX_IMM16_X0_HW2 =   40;
enum R_TILEGX_IMM16_X1_HW2 =   41;
enum R_TILEGX_IMM16_X0_HW3 =   42;
enum R_TILEGX_IMM16_X1_HW3 =   43;
enum R_TILEGX_IMM16_X0_HW0_LAST = 44;
enum R_TILEGX_IMM16_X1_HW0_LAST = 45;
enum R_TILEGX_IMM16_X0_HW1_LAST = 46;
enum R_TILEGX_IMM16_X1_HW1_LAST = 47;
enum R_TILEGX_IMM16_X0_HW2_LAST = 48;
enum R_TILEGX_IMM16_X1_HW2_LAST = 49;
enum R_TILEGX_IMM16_X0_HW0_PCREL = 50;
enum R_TILEGX_IMM16_X1_HW0_PCREL = 51;
enum R_TILEGX_IMM16_X0_HW1_PCREL = 52;
enum R_TILEGX_IMM16_X1_HW1_PCREL = 53;
enum R_TILEGX_IMM16_X0_HW2_PCREL = 54;
enum R_TILEGX_IMM16_X1_HW2_PCREL = 55;
enum R_TILEGX_IMM16_X0_HW3_PCREL = 56;
enum R_TILEGX_IMM16_X1_HW3_PCREL = 57;
enum R_TILEGX_IMM16_X0_HW0_LAST_PCREL = 58;
enum R_TILEGX_IMM16_X1_HW0_LAST_PCREL = 59;
enum R_TILEGX_IMM16_X0_HW1_LAST_PCREL = 60;
enum R_TILEGX_IMM16_X1_HW1_LAST_PCREL = 61;
enum R_TILEGX_IMM16_X0_HW2_LAST_PCREL = 62;
enum R_TILEGX_IMM16_X1_HW2_LAST_PCREL = 63;
enum R_TILEGX_IMM16_X0_HW0_GOT = 64;
enum R_TILEGX_IMM16_X1_HW0_GOT = 65;
enum R_TILEGX_IMM16_X0_HW0_PLT_PCREL = 66;
enum R_TILEGX_IMM16_X1_HW0_PLT_PCREL = 67;
enum R_TILEGX_IMM16_X0_HW1_PLT_PCREL = 68;
enum R_TILEGX_IMM16_X1_HW1_PLT_PCREL = 69;
enum R_TILEGX_IMM16_X0_HW2_PLT_PCREL = 70;
enum R_TILEGX_IMM16_X1_HW2_PLT_PCREL = 71;
enum R_TILEGX_IMM16_X0_HW0_LAST_GOT = 72;
enum R_TILEGX_IMM16_X1_HW0_LAST_GOT = 73;
enum R_TILEGX_IMM16_X0_HW1_LAST_GOT = 74;
enum R_TILEGX_IMM16_X1_HW1_LAST_GOT = 75;
enum R_TILEGX_IMM16_X0_HW3_PLT_PCREL = 76;
enum R_TILEGX_IMM16_X1_HW3_PLT_PCREL = 77;
enum R_TILEGX_IMM16_X0_HW0_TLS_GD = 78;
enum R_TILEGX_IMM16_X1_HW0_TLS_GD = 79;
enum R_TILEGX_IMM16_X0_HW0_TLS_LE = 80;
enum R_TILEGX_IMM16_X1_HW0_TLS_LE = 81;
enum R_TILEGX_IMM16_X0_HW0_LAST_TLS_LE = 82;
enum R_TILEGX_IMM16_X1_HW0_LAST_TLS_LE = 83;
enum R_TILEGX_IMM16_X0_HW1_LAST_TLS_LE = 84;
enum R_TILEGX_IMM16_X1_HW1_LAST_TLS_LE = 85;
enum R_TILEGX_IMM16_X0_HW0_LAST_TLS_GD = 86;
enum R_TILEGX_IMM16_X1_HW0_LAST_TLS_GD = 87;
enum R_TILEGX_IMM16_X0_HW1_LAST_TLS_GD = 88;
enum R_TILEGX_IMM16_X1_HW1_LAST_TLS_GD = 89;
enum R_TILEGX_IMM16_X0_HW0_TLS_IE = 92;
enum R_TILEGX_IMM16_X1_HW0_TLS_IE = 93;
enum R_TILEGX_IMM16_X0_HW0_LAST_PLT_PCREL = 94;
enum R_TILEGX_IMM16_X1_HW0_LAST_PLT_PCREL = 95;
enum R_TILEGX_IMM16_X0_HW1_LAST_PLT_PCREL = 96;
enum R_TILEGX_IMM16_X1_HW1_LAST_PLT_PCREL = 97;
enum R_TILEGX_IMM16_X0_HW2_LAST_PLT_PCREL = 98;
enum R_TILEGX_IMM16_X1_HW2_LAST_PLT_PCREL = 99;
enum R_TILEGX_IMM16_X0_HW0_LAST_TLS_IE = 100;
enum R_TILEGX_IMM16_X1_HW0_LAST_TLS_IE = 101;
enum R_TILEGX_IMM16_X0_HW1_LAST_TLS_IE = 102;
enum R_TILEGX_IMM16_X1_HW1_LAST_TLS_IE = 103;
enum R_TILEGX_TLS_DTPMOD64 =   106;
enum R_TILEGX_TLS_DTPOFF64 =   107;
enum R_TILEGX_TLS_TPOFF64 =    108;
enum R_TILEGX_TLS_DTPMOD32 =   109;
enum R_TILEGX_TLS_DTPOFF32 =   110;
enum R_TILEGX_TLS_TPOFF32 =    111;
enum R_TILEGX_TLS_GD_CALL =    112;
enum R_TILEGX_IMM8_X0_TLS_GD_ADD = 113;
enum R_TILEGX_IMM8_X1_TLS_GD_ADD = 114;
enum R_TILEGX_IMM8_Y0_TLS_GD_ADD = 115;
enum R_TILEGX_IMM8_Y1_TLS_GD_ADD = 116;
enum R_TILEGX_TLS_IE_LOAD =    117;
enum R_TILEGX_IMM8_X0_TLS_ADD = 118;
enum R_TILEGX_IMM8_X1_TLS_ADD = 119;
enum R_TILEGX_IMM8_Y0_TLS_ADD = 120;
enum R_TILEGX_IMM8_Y1_TLS_ADD = 121;

enum R_TILEGX_GNU_VTINHERIT =  128;
enum R_TILEGX_GNU_VTENTRY =    129;

enum R_TILEGX_NUM =            130;

enum EF_RISCV_RVC =              0x0001;
enum EF_RISCV_FLOAT_ABI =        0x0006;
enum EF_RISCV_FLOAT_ABI_SOFT =   0x0000;
enum EF_RISCV_FLOAT_ABI_SINGLE = 0x0002;
enum EF_RISCV_FLOAT_ABI_DOUBLE = 0x0004;
enum EF_RISCV_FLOAT_ABI_QUAD =   0x0006;
enum R_RISCV_NONE =            0;
enum R_RISCV_32 =              1;
enum R_RISCV_64 =              2;
enum R_RISCV_RELATIVE =        3;
enum R_RISCV_COPY =            4;
enum R_RISCV_JUMP_SLOT =       5;
enum R_RISCV_TLS_DTPMOD32 =    6;
enum R_RISCV_TLS_DTPMOD64 =    7;
enum R_RISCV_TLS_DTPREL32 =    8;
enum R_RISCV_TLS_DTPREL64 =    9;
enum R_RISCV_TLS_TPREL32 =     10;
enum R_RISCV_TLS_TPREL64 =     11;
enum R_RISCV_BRANCH =          16;
enum R_RISCV_JAL =             17;
enum R_RISCV_CALL =            18;
enum R_RISCV_CALL_PLT =        19;
enum R_RISCV_GOT_HI20 =        20;
enum R_RISCV_TLS_GOT_HI20 =    21;
enum R_RISCV_TLS_GD_HI20 =     22;
enum R_RISCV_PCREL_HI20 =      23;
enum R_RISCV_PCREL_LO12_I =    24;
enum R_RISCV_PCREL_LO12_S =    25;
enum R_RISCV_HI20 =            26;
enum R_RISCV_LO12_I =          27;
enum R_RISCV_LO12_S =          28;
enum R_RISCV_TPREL_HI20 =      29;
enum R_RISCV_TPREL_LO12_I =    30;
enum R_RISCV_TPREL_LO12_S =    31;
enum R_RISCV_TPREL_ADD =       32;
enum R_RISCV_ADD8 =            33;
enum R_RISCV_ADD16 =           34;
enum R_RISCV_ADD32 =           35;
enum R_RISCV_ADD64 =           36;
enum R_RISCV_SUB8 =            37;
enum R_RISCV_SUB16 =           38;
enum R_RISCV_SUB32 =           39;
enum R_RISCV_SUB64 =           40;
enum R_RISCV_GNU_VTINHERIT =   41;
enum R_RISCV_GNU_VTENTRY =     42;
enum R_RISCV_ALIGN =           43;
enum R_RISCV_RVC_BRANCH =      44;
enum R_RISCV_RVC_JUMP =        45;
enum R_RISCV_RVC_LUI =         46;
enum R_RISCV_GPREL_I =         47;
enum R_RISCV_GPREL_S =         48;
enum R_RISCV_TPREL_I =         49;
enum R_RISCV_TPREL_S =         50;
enum R_RISCV_RELAX =           51;
enum R_RISCV_SUB6 =            52;
enum R_RISCV_SET6 =            53;
enum R_RISCV_SET8 =            54;
enum R_RISCV_SET16 =           55;
enum R_RISCV_SET32 =           56;
enum R_RISCV_32_PCREL =        57;
enum R_RISCV_IRELATIVE =       58;
enum R_RISCV_NUM =             59;
