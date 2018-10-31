/**
 * D header file for DragonFlyBSD.
 *
 * Authors: Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys.elf_common;

version (DragonFlyBSD):

extern (C):

import core.stdc.stdint;

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

enum EI_MAG0 =         0;
enum EI_MAG1 =         1;
enum EI_MAG2 =         2;
enum EI_MAG3 =         3;
enum EI_CLASS =        4;
enum EI_DATA =         5;
enum EI_VERSION =      6;
enum EI_OSABI =        7;
enum EI_ABIVERSION =   8;
enum OLD_EI_BRAND =    8;
enum EI_PAD =          9;
enum EI_NIDENT =       16;

enum ELFMAG0 =         0x7f;
enum ELFMAG1 =         'E';
enum ELFMAG2 =         'L';
enum ELFMAG3 =         'F';
enum ELFMAG =          "\177ELF";
enum SELFMAG =         4;

enum EV_NONE =         0;
enum EV_CURRENT =      1;

enum ELFCLASSNONE =    0;
enum ELFCLASS32 =      1;
enum ELFCLASS64 =      2;

enum ELFDATANONE =     0;
enum ELFDATA2LSB =     1;
enum ELFDATA2MSB =     2;

enum ELFOSABI_NONE =           0;
enum ELFOSABI_SYSV =           0;
enum ELFOSABI_HPUX =           1;
enum ELFOSABI_NETBSD =         2;
enum ELFOSABI_LINUX =          3;
enum ELFOSABI_HURD  =          4;
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
enum ELFOSABI_ARM =            97;
enum ELFOSABI_STANDALONE =     255;

// just a pointer
enum ELFOSABI_DRAGONFLYBSD =   ELFOSABI_NONE;

extern (D) pure @safe
{
    auto IS_ELF(T)(T ehdr) { return ehdr.e_ident[EI_MAG0] == ELFMAG0 &&
                                    ehdr.e_ident[EI_MAG1] == ELFMAG1 &&
                                    ehdr.e_ident[EI_MAG2] == ELFMAG2 &&
                                    ehdr.e_ident[EI_MAG3] == ELFMAG3; }
}

enum ET_NONE =         0;
enum ET_REL =          1;
enum ET_EXEC =         2;
enum ET_DYN =          3;
enum ET_CORE =         4;
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
enum EM_AMD64 =        62;
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
enum EM_ALTERA_NIOS2 =113;
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
enum EM_res143 =      143;
enum EM_res144 =      144;
enum EM_res145 =      145;
enum EM_res146 =      146;
enum EM_res147 =      147;
enum EM_res148 =      148;
enum EM_res149 =      149;
enum EM_res150 =      150;
enum EM_res151 =      151;
enum EM_res152 =      152;
enum EM_res153 =      153;
enum EM_res154 =      154;
enum EM_res155 =      155;
enum EM_res156 =      156;
enum EM_res157 =      157;
enum EM_res158 =      158;
enum EM_res159 =      159;
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
enum EM_res183 =      183;
enum EM_res184 =      184;
enum EM_AVR32 =       185;
enum EM_STM8  =       186;
enum EM_TILE64 =      187;
enum EM_TILEPRO =     188;
enum EM_MICROBLAZE =  189;
enum EM_CUDA  =       190;
enum EM_TILEGX =      191;

enum EM_486 =           6;
enum EM_MIPS_RS4_BE =  10;
enum EM_ALPHA_STD =    41;
enum EM_ALPHA =    0x9026;

enum SHN_UNDEF =       0;
enum SHN_LORESERVE =   0xff00;
enum SHN_LOPROC =      0xff00;
enum SHN_HIPROC =      0xff1f;
enum SHN_LOOS =        0xff20;
enum SHN_HIOS =        0xff3f;
enum SHN_ABS =         0xfff1;
enum SHN_COMMON =      0xfff2;
enum SHN_XINDEX =      0xffff;
enum SHN_HIRESERVE =   0xffff;

enum PT_NULL =         0;
enum PT_LOAD =         1;
enum PT_DYNAMIC =      2;
enum PT_INTERP =       3;
enum PT_NOTE =         4;
enum PT_SHLIB =        5;
enum PT_PHDR =         6;
enum PT_TLS =          7;
enum PT_LOOS =         0x60000000;
enum PT_HIOS =         0x6fffffff;
enum PT_LOPROC =       0x70000000;
enum PT_HIPROC =       0x7fffffff;

enum PT_GNU_EH_FRAME =  PT_LOOS + 0x474e550; /* Frame unwind information */
enum PT_SUNW_EH_FRAME = PT_GNU_EH_FRAME;     /* Solaris uses the same value */
enum PT_GNU_STACK =     PT_LOOS + 0x474e551; /* Stack flags */
enum PT_GNU_RELRO =     PT_LOOS + 0x474e552;  /* Read-only after relocation */

enum PF_X =            0x1;
enum PF_W =            0x2;
enum PF_R =            0x4;
enum PF_MASKOS =       0x0ff00000;
enum PF_MASKPROC =     0xf0000000;

enum PN_XNUM =         0xffff;

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

enum SHT_LOOS =          0x60000000;
enum SHT_LOSUNW =        0x6ffffff4;

enum SHT_GNU_INCREMENTAL_INPUTS =  0x6fff4700;
enum SHT_GNU_ATTRIBUTES =       0x6ffffff5;
enum SHT_GNU_HASH =             0x6ffffff6;
enum SHT_GNU_LIBLIST =          0x6ffffff7;

//enum SHT_SUNW_dof =      0x6ffffff4;
//enum SHT_SUNW_cap =      0x6ffffff5;
//enum SHT_SUNW_SIGNATURE = 0x6ffffff6;
enum SHT_SUNW_verdef =   0x6ffffffd;
enum SHT_SUNW_verneed =  0x6ffffffe;
enum SHT_SUNW_versym =   0x6fffffff;

enum SHT_GNU_verdef =    SHT_SUNW_verdef;
enum SHT_GNU_verneed =   SHT_SUNW_verneed;
enum SHT_GNU_versym =    SHT_SUNW_versym;

enum SHT_LOPROC =        0x70000000;
enum SHT_HIPROC =        0x7fffffff;
enum SHT_LOUSER =        0x80000000;
enum SHT_HIUSER =        0x8fffffff;

/*
enum SHT_GNU_HASH =      0x6ffffff6;
enum SHT_SUNW_ANNOTATE = 0x6ffffff7;
enum SHT_SUNW_DEBUGSTR = 0x6ffffff8;
enum SHT_SUNW_DEBUG =    0x6ffffff9;
enum SHT_SUNW_move =     0x6ffffffa;
enum SHT_SUNW_COMDAT =   0x6ffffffb;
enum SHT_SUNW_syminfo =  0x6ffffffc;
enum SHT_HISUNW =        0x6fffffff;
enum SHT_HIOS =          0x6fffffff;
enum SHT_AMD64_UNWIND =  0x70000001;
enum SHT_ARM_EXIDX =     0x70000001;
enum SHT_ARM_PREEMPTMAP = 0x70000002;
enum SHT_ARM_ATTRIBUTES = 0x70000003;
enum SHT_ARM_DEBUGOVERLAY = 0x70000004;
enum SHT_ARM_OVERLAYSECTION = 0x70000005;
enum SHT_MIPS_REGINFO =  0x70000006;
enum SHT_MIPS_OPTIONS =  0x7000000d;
enum SHT_MIPS_DWARF =    0x7000001e;
*/

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

enum NT_PRSTATUS =     1;
enum NT_FPREGSET =     2;
enum NT_PRPSINFO =     3;
enum NT_TASKSTRUCT =   4;
enum NT_AUXV =         6;

/*
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
*/

enum STN_UNDEF =       0;

enum STB_LOCAL =       0;
enum STB_GLOBAL =      1;
enum STB_WEAK =        2;
enum STB_NUM =         3;
enum STB_LOOS =        10;
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

enum STV_DEFAULT =     0;
enum STV_INTERNAL =    1;
enum STV_HIDDEN =      2;
enum STV_PROTECTED =   3;
/*
enum STV_EXPORTED =    4;
enum STV_SINGLETON =   5;
enum STV_ELIMINATE =   6;
*/

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
//enum DT_MAXPOSTAGS =   34;
enum DT_LOOS =         0x6000000d;
enum DT_HIOS =         0x6ffff000;
/*
enum DT_SUNW_AUXILIARY = 0x6000000d;
enum DT_SUNW_RTLDINF = 0x6000000e;
enum DT_SUNW_FILTER =  0x6000000f;
enum DT_SUNW_CAP =     0x60000010;
*/

enum DT_VALRNGLO =     0x6ffffd00;
enum DT_GNU_PRELINKED = 0x6ffffdf5;
enum DT_GNU_CONFLICTSZ =0x6ffffdf6;
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
enum DT_RELACOUNT =    0x6ffffff9;
enum DT_RELCOUNT =     0x6ffffffa;
enum DT_FLAGS_1 =      0x6ffffffb;
enum DT_VERDEF =       0x6ffffffc;
enum DT_VERDEFNUM =    0x6ffffffd;
enum DT_VERNEED =      0x6ffffffe;
enum DT_VERNEEDNUM =   0x6fffffff;
enum DT_VERSYM =       0x6ffffff0;
enum DT_LOPROC =       0x70000000;
//enum DT_DEPRECATED_SPARC_REGISTER = 0x7000001;
enum DT_AUXILIARY =    0x7ffffffd;
enum DT_USED =         0x7ffffffe;
enum DT_FILTER =       0x7fffffff;
enum DT_HIPROC =       0x7fffffff;

enum DTF_1_PARINIT =   0x00000001;
enum DTF_1_CONFEXP =   0x00000002;

enum DF_P1_LAZYLOAD =  0x00000001;
enum DF_P1_GROUPPERM=  0x00000002;

enum DF_1_NOW =        0x00000001;
enum DF_1_BIND_NOW =   0x00000001;
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
enum DF_1_CONLFAT =    0x00002000;

enum DF_ORIGIN =       0x00000001;
enum DF_SYMBOLIC =     0x00000002;
enum DF_TEXTREL =      0x00000004;
enum DF_BIND_NOW =     0x00000008;
enum DF_STATIC_TLS =   0x00000010;

enum VER_DEF_NONE =    0;
enum VER_DEF_CURRENT = 1;
alias VER_NDX VER_DEF_IDX;

enum VER_FLG_BASE =    0x1;
enum VER_FLG_WEAK =    0x2;
enum VER_FLG_INFO =    0x4;

enum VER_NDX_LOCAL =           0;
enum VER_NDX_GLOBAL =          1;
enum VER_NDX_GIVEN =           2;
enum VER_NDX_HIDDEN =      32768;
extern (D) pure @safe
{
    auto VER_NDX(V)(V v) { return v & ~(1u << 15); }
}

enum VER_NEED_NONE   = 0;
enum VER_NEED_CURRENT = 1;
enum VER_NEED_WEAK =    32768;
enum VER_NEED_HIDDEN = VER_NDX_HIDDEN;
alias VER_NDX VER_NEED_IDX;

/*
enum CA_SUNW_NULL =    0;
enum CA_SUNW_HW_1 =    1;
enum CA_SUNW_SF_1 =    2;
*/

enum VERSYM_HIDDEN =   0x8000;
enum VERSYM_VERSION =  0x7fff;
enum ELF_VER_CHR =     '@';

enum SYMINFO_BT_SELF =         0xffff;
enum SYMINFO_BT_PARENT =       0xfffe;
//enum SYMINFO_BT_NONE =         0xfffd;
//enum SYMINFO_BT_EXTERN =       0xfffc;
enum SYMINFO_BT_LOWRESERVE =   0xff00;

enum SYMINFO_FLG_DIRECT =      0x0001;
enum SYMINFO_FLG_PASSTHRU =    0x0002;
enum SYMINFO_FLG_COPY =        0x0004;
enum SYMINFO_FLG_LAZYLOAD =    0x0008;
//enum SYMINFO_FLG_DIRECTBIND =  0x0010;
//enum SYMINFO_FLG_NOEXTDIRECT = 0x0020;
//enum SYMINFO_FLG_FILTER =      0x0002;
//enum SYMINFO_FLG_AUXILIARY =   0x0040;

enum SYMINFO_NONE =            0;
enum SYMINFO_CURRENT =         1;
enum SYMINFO_NUM =             2;

enum GRP_COMDAT =              0x1;

enum R_386_NONE =               0;
enum R_386_32 =                 1;
enum R_386_PC32 =               2;
enum R_386_GOT32 =              3;
enum R_386_PLT32 =              4;
enum R_386_COPY =               5;
enum R_386_GLOB_DAT =           6;
enum R_386_JMP_SLOT =           7;
enum R_386_RELATIVE =           8;
enum R_386_GOTOFF =             9;
enum R_386_GOTPC =              10;
enum R_386_TLS_TPOFF =          14;
enum R_386_TLS_IE =             15;
enum R_386_TLS_GOTIE =          16;
enum R_386_TLS_LE =             17;
enum R_386_TLS_GD =             18;
enum R_386_TLS_LDM =            19;
enum R_386_TLS_GD_32 =          24;
enum R_386_TLS_GD_PUSH =        25;
enum R_386_TLS_GD_CALL =        26;
enum R_386_TLS_GD_POP =         27;
enum R_386_TLS_LDM_32 =         28;
enum R_386_TLS_LDM_PUSH =       29;
enum R_386_TLS_LDM_CALL =       30;
enum R_386_TLS_LDM_POP =        31;
enum R_386_TLS_LDO_32 =         32;
enum R_386_TLS_IE_32 =          33;
enum R_386_TLS_LE_32 =          34;
enum R_386_TLS_DTPMOD32 =       35;
enum R_386_TLS_DTPOFF32 =       36;
enum R_386_TLS_TPOFF32 =        37;
enum R_386_IRELATIVE =          42;

enum R_X86_64_NONE =            0;
enum R_X86_64_64 =              1;
enum R_X86_64_PC32 =            2;
enum R_X86_64_GOT32 =           3;
enum R_X86_64_PLT32 =           4;
enum R_X86_64_COPY =            5;
enum R_X86_64_GLOB_DAT =        6;
enum R_X86_64_JMP_SLOT =        7;
enum R_X86_64_RELATIVE =        8;
enum R_X86_64_GOTPCREL =        9;
enum R_X86_64_32 =              10;
enum R_X86_64_32S =             11;
enum R_X86_64_16 =              12;
enum R_X86_64_PC16 =            13;
enum R_X86_64_8 =               14;
enum R_X86_64_PC8 =             15;
enum R_X86_64_DTPMOD64 =        16;
enum R_X86_64_DTPOFF64 =        17;
enum R_X86_64_TPOFF64 =         18;
enum R_X86_64_TLSGD =           19;
enum R_X86_64_TLSLD =           20;
enum R_X86_64_DTPOFF32 =        21;
enum R_X86_64_GOTTPOFF =        22;
enum R_X86_64_TPOFF32 =         23;
enum R_X86_64_IRELATIVE =       37;
