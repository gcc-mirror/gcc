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

extern (D)
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
enum SHT_SUNW_dof =      0x6ffffff4;
enum SHT_SUNW_cap =      0x6ffffff5;
enum SHT_SUNW_SIGNATURE = 0x6ffffff6;
enum SHT_GNU_HASH =      0x6ffffff6;
enum SHT_SUNW_ANNOTATE = 0x6ffffff7;
enum SHT_SUNW_DEBUGSTR = 0x6ffffff8;
enum SHT_SUNW_DEBUG =    0x6ffffff9;
enum SHT_SUNW_move =     0x6ffffffa;
enum SHT_SUNW_COMDAT =   0x6ffffffb;
enum SHT_SUNW_syminfo =  0x6ffffffc;
enum SHT_SUNW_verdef =   0x6ffffffd;
enum SHT_GNU_verdef =    0x6ffffffd;
enum SHT_SUNW_verneed =  0x6ffffffe;
enum SHT_GNU_verneed =   0x6ffffffe;
enum SHT_SUNW_versym =   0x6fffffff;
enum SHT_GNU_versym =    0x6fffffff;
enum SHT_HISUNW =        0x6fffffff;
enum SHT_HIOS =          0x6fffffff;
enum SHT_LOPROC =        0x70000000;
enum SHT_AMD64_UNWIND =  0x70000001;
enum SHT_ARM_EXIDX =     0x70000001;
enum SHT_ARM_PREEMPTMAP = 0x70000002;
enum SHT_ARM_ATTRIBUTES = 0x70000003;
enum SHT_ARM_DEBUGOVERLAY = 0x70000004;
enum SHT_ARM_OVERLAYSECTION = 0x70000005;
enum SHT_MIPS_REGINFO =  0x70000006;
enum SHT_MIPS_OPTIONS =  0x7000000d;
enum SHT_MIPS_DWARF =    0x7000001e;
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

enum PT_NULL =         0;
enum PT_LOAD =         1;
enum PT_DYNAMIC =      2;
enum PT_INTERP =       3;
enum PT_NOTE =         4;
enum PT_SHLIB =        5;
enum PT_PHDR =         6;
enum PT_TLS =          7;
enum PT_LOOS =         0x60000000;
enum PT_SUNW_UNWIND =  0x6464e550;
enum PT_GNU_EH_FRAME = 0x6474e550;
enum PT_GNU_STACK =    0x6474e551;
enum PT_GNU_RELRO =    0x6474e552;
enum PT_LOSUNW =       0x6ffffffa;
enum PT_SUNWBSS =      0x6ffffffa;
enum PT_SUNWSTACK =    0x6ffffffb;
enum PT_SUNWDTRACE =   0x6ffffffc;
enum PT_SUNWCAP =      0x6ffffffd;
enum PT_HISUNW =       0x6fffffff;
enum PT_HIOS =         0x6fffffff;
enum PT_LOPROC =       0x70000000;
enum PT_HIPROC =       0x7fffffff;

enum PF_X =            (1 << 0);
enum PF_W =            (1 << 1);
enum PF_R =            (1 << 2);
enum PF_MASKOS =       0x0ff00000;
enum PF_MASKPROC =     0xf0000000;

enum PN_XNUM =         0xffff;

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
enum DT_MAXPOSTAGS =   34;
enum DT_LOOS =         0x6000000d;
enum DT_SUNW_AUXILIARY = 0x6000000d;
enum DT_SUNW_RTLDINF = 0x6000000e;
enum DT_SUNW_FILTER =  0x6000000f;
enum DT_SUNW_CAP =     0x60000010;
enum DT_HIOS =         0x6ffff000;
enum DT_VALRNGLO =     0x6ffffd00;
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
enum DT_CONFIG =       0x6ffffefa;
enum DT_DEPAUDIT =     0x6ffffefb;
enum DT_AUDIT =        0x6ffffefc;
enum DT_PLTPAD =       0x6ffffefd;
enum DT_MOVETAB =      0x6ffffefe;
enum DT_SYMINFO =      0x6ffffeff;
enum DT_ADDRRNGHI =    0x6ffffeff;
enum DT_VERSYM =       0x6ffffff0;
enum DT_RELACOUNT =    0x6ffffff9;
enum DT_RELCOUNT =     0x6ffffffa;
enum DT_FLAGS_1 =      0x6ffffffb;
enum DT_VERDEF =       0x6ffffffc;
enum DT_VERDEFNUM =    0x6ffffffd;
enum DT_VERNEED =      0x6ffffffe;
enum DT_VERNEEDNUM =   0x6fffffff;
enum DT_LOPROC =       0x70000000;
enum DT_DEPRECATED_SPARC_REGISTER = 0x7000001;
enum DT_AUXILIARY =    0x7ffffffd;
enum DT_USED =         0x7ffffffe;
enum DT_FILTER =       0x7fffffff;
enum DT_HIPROC =       0x7fffffff;

enum DF_ORIGIN =       0x00000001;
enum DF_SYMBOLIC =     0x00000002;
enum DF_TEXTREL =      0x00000004;
enum DF_BIND_NOW =     0x00000008;
enum DF_STATIC_TLS =   0x00000010;

enum DF_1_BIND_NOW =   0x00000001;
enum DF_1_GLOBAL =     0x00000002;
enum DF_1_NODELETE =   0x00000008;
enum DF_1_LOADFLTR =   0x00000010;
enum DF_1_NOOPEN =     0x00000040;
enum DF_1_NODEFLIB =   0x00000800;

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
enum STV_EXPORTED =    4;
enum STV_SINGLETON =   5;
enum STV_ELIMINATE =   6;

enum STN_UNDEF =       0;

enum VER_DEF_CURRENT = 1;
alias VER_NDX VER_DEF_IDX;

enum VER_FLG_BASE =    0x1;
enum VER_FLG_WEAK =    0x2;

enum VER_NEED_CURRENT = 1;
enum VER_NEED_WEAK =    32768;
enum VER_NEED_HIDDEN = VER_NDX_HIDDEN;
alias VER_NDX VER_NEED_IDX;

enum VER_NDX_LOCAL =           0;
enum VER_NDX_GLOBAL =          1;
enum VER_NDX_GIVEN =           2;

enum VER_NDX_HIDDEN =      32768;
extern (D)
{
    auto VER_NDX(V)(V v) { return v & ~(1u << 15); }
}

enum CA_SUNW_NULL =    0;
enum CA_SUNW_HW_1 =    1;
enum CA_SUNW_SF_1 =    2;

enum SYMINFO_FLG_DIRECT =      0x0001;
enum SYMINFO_FLG_PASSTHRU =    0x0002;
enum SYMINFO_FLG_COPY =        0x0004;
enum SYMINFO_FLG_LAZYLOAD =    0x0008;
enum SYMINFO_FLG_DIRECTBIND =  0x0010;
enum SYMINFO_FLG_NOEXTDIRECT = 0x0020;
enum SYMINFO_FLG_FILTER =      0x0002;
enum SYMINFO_FLG_AUXILIARY =   0x0040;

enum SYMINFO_BT_SELF =         0xffff;
enum SYMINFO_BT_PARENT =       0xfffe;
enum SYMINFO_BT_NONE =         0xfffd;
enum SYMINFO_BT_EXTERN =       0xfffc;
enum SYMINFO_BT_LOWRESERVE =   0xff00;

enum SYMINFO_NONE =            0;
enum SYMINFO_CURRENT =         1;
enum SYMINFO_NUM =             2;

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

enum R_ARM_NONE =               0;
enum R_ARM_PC24 =               1;
enum R_ARM_ABS32 =              2;
enum R_ARM_REL32 =              3;
enum R_ARM_PC13 =               4;
enum R_ARM_ABS16 =              5;
enum R_ARM_ABS12 =              6;
enum R_ARM_THM_ABS5 =           7;
enum R_ARM_ABS8 =               8;
enum R_ARM_SBREL32 =            9;
enum R_ARM_THM_PC22 =           10;
enum R_ARM_THM_PC8 =            11;
enum R_ARM_AMP_VCALL9 =         12;
enum R_ARM_SWI24 =              13;
enum R_ARM_THM_SWI8 =           14;
enum R_ARM_XPC25 =              15;
enum R_ARM_THM_XPC22 =          16;
enum R_ARM_TLS_DTPMOD32 =       17;
enum R_ARM_TLS_DTPOFF32 =       18;
enum R_ARM_TLS_TPOFF32 =        19;
enum R_ARM_COPY =               20;
enum R_ARM_GLOB_DAT =           21;
enum R_ARM_JUMP_SLOT =          22;
enum R_ARM_RELATIVE =           23;
enum R_ARM_GOTOFF =             24;
enum R_ARM_GOTPC =              25;
enum R_ARM_GOT32 =              26;
enum R_ARM_PLT32 =              27;
enum R_ARM_GNU_VTENTRY =        100;
enum R_ARM_GNU_VTINHERIT =      101;
enum R_ARM_RSBREL32 =           250;
enum R_ARM_THM_RPC22 =          251;
enum R_ARM_RREL32 =             252;
enum R_ARM_RABS32 =             253;
enum R_ARM_RPC24 =              254;
enum R_ARM_RBASE =              255;

enum R_IA_64_NONE =             0;
enum R_IA_64_IMM14 =            0x21;
enum R_IA_64_IMM22 =            0x22;
enum R_IA_64_IMM64 =            0x23;
enum R_IA_64_DIR32MSB =         0x24;
enum R_IA_64_DIR32LSB =         0x25;
enum R_IA_64_DIR64MSB =         0x26;
enum R_IA_64_DIR64LSB =         0x27;
enum R_IA_64_GPREL22 =          0x2a;
enum R_IA_64_GPREL64I =         0x2b;
enum R_IA_64_GPREL32MSB =       0x2c;
enum R_IA_64_GPREL32LSB =       0x2d;
enum R_IA_64_GPREL64MSB =       0x2e;
enum R_IA_64_GPREL64LSB =       0x2f;
enum R_IA_64_LTOFF22 =          0x32;
enum R_IA_64_LTOFF64I =         0x33;
enum R_IA_64_PLTOFF22 =         0x3a;
enum R_IA_64_PLTOFF64I =        0x3b;
enum R_IA_64_PLTOFF64MSB =      0x3e;
enum R_IA_64_PLTOFF64LSB =      0x3f;
enum R_IA_64_FPTR64I =          0x43;
enum R_IA_64_FPTR32MSB =        0x44;
enum R_IA_64_FPTR32LSB =        0x45;
enum R_IA_64_FPTR64MSB =        0x46;
enum R_IA_64_FPTR64LSB =        0x47;
enum R_IA_64_PCREL60B =         0x48;
enum R_IA_64_PCREL21B =         0x49;
enum R_IA_64_PCREL21M =         0x4a;
enum R_IA_64_PCREL21F =         0x4b;
enum R_IA_64_PCREL32MSB =       0x4c;
enum R_IA_64_PCREL32LSB =       0x4d;
enum R_IA_64_PCREL64MSB =       0x4e;
enum R_IA_64_PCREL64LSB =       0x4f;
enum R_IA_64_LTOFF_FPTR22 =     0x52;
enum R_IA_64_LTOFF_FPTR64I =    0x53;
enum R_IA_64_LTOFF_FPTR32MSB =  0x54;
enum R_IA_64_LTOFF_FPTR32LSB =  0x55;
enum R_IA_64_LTOFF_FPTR64MSB =  0x56;
enum R_IA_64_LTOFF_FPTR64LSB =  0x57;
enum R_IA_64_SEGREL32MSB =      0x5c;
enum R_IA_64_SEGREL32LSB =      0x5d;
enum R_IA_64_SEGREL64MSB =      0x5e;
enum R_IA_64_SEGREL64LSB =      0x5f;
enum R_IA_64_SECREL32MSB =      0x64;
enum R_IA_64_SECREL32LSB =      0x65;
enum R_IA_64_SECREL64MSB =      0x66;
enum R_IA_64_SECREL64LSB =      0x67;
enum R_IA_64_REL32MSB =         0x6c;
enum R_IA_64_REL32LSB =         0x6d;
enum R_IA_64_REL64MSB =         0x6e;
enum R_IA_64_REL64LSB =         0x6f;
enum R_IA_64_LTV32MSB =         0x74;
enum R_IA_64_LTV32LSB =         0x75;
enum R_IA_64_LTV64MSB =         0x76;
enum R_IA_64_LTV64LSB =         0x77;
enum R_IA_64_PCREL21BI =        0x79;
enum R_IA_64_PCREL22 =          0x7a;
enum R_IA_64_PCREL64I =         0x7b;
enum R_IA_64_IPLTMSB =          0x80;
enum R_IA_64_IPLTLSB =          0x81;
enum R_IA_64_SUB =              0x85;
enum R_IA_64_LTOFF22X =         0x86;
enum R_IA_64_LDXMOV =           0x87;
enum R_IA_64_TPREL14 =          0x91;
enum R_IA_64_TPREL22 =          0x92;
enum R_IA_64_TPREL64I =         0x93;
enum R_IA_64_TPREL64MSB =       0x96;
enum R_IA_64_TPREL64LSB =       0x97;
enum R_IA_64_LTOFF_TPREL22 =    0x9a;
enum R_IA_64_DTPMOD64MSB =      0xa6;
enum R_IA_64_DTPMOD64LSB =      0xa7;
enum R_IA_64_LTOFF_DTPMOD22 =   0xaa;
enum R_IA_64_DTPREL14 =         0xb1;
enum R_IA_64_DTPREL22 =         0xb2;
enum R_IA_64_DTPREL64I =        0xb3;
enum R_IA_64_DTPREL32MSB =      0xb4;
enum R_IA_64_DTPREL32LSB =      0xb5;
enum R_IA_64_DTPREL64MSB =      0xb6;
enum R_IA_64_DTPREL64LSB =      0xb7;
enum R_IA_64_LTOFF_DTPREL22 =   0xba;

enum R_MIPS_NONE =              0;
enum R_MIPS_16 =                1;
enum R_MIPS_32 =                2;
enum R_MIPS_REL32 =             3;
enum R_MIPS_26 =                4;
enum R_MIPS_HI16 =              5;
enum R_MIPS_LO16 =              6;
enum R_MIPS_GPREL16 =           7;
enum R_MIPS_LITERAL =           8;
enum R_MIPS_GOT16 =             9;
enum R_MIPS_PC16 =              10;
enum R_MIPS_CALL16 =            11;
enum R_MIPS_GPREL32 =           12;
enum R_MIPS_GOTHI16 =           21;
enum R_MIPS_GOTLO16 =           22;
enum R_MIPS_CALLHI16 =          30;
enum R_MIPS_CALLLO16 =          31;

enum R_PPC_NONE =               0;
enum R_PPC_ADDR32 =             1;
enum R_PPC_ADDR24 =             2;
enum R_PPC_ADDR16 =             3;
enum R_PPC_ADDR16_LO =          4;
enum R_PPC_ADDR16_HI =          5;
enum R_PPC_ADDR16_HA =          6;
enum R_PPC_ADDR14 =             7;
enum R_PPC_ADDR14_BRTAKEN =     8;
enum R_PPC_ADDR14_BRNTAKEN =    9;
enum R_PPC_REL24 =              10;
enum R_PPC_REL14 =              11;
enum R_PPC_REL14_BRTAKEN =      12;
enum R_PPC_REL14_BRNTAKEN =     13;
enum R_PPC_GOT16 =              14;
enum R_PPC_GOT16_LO =           15;
enum R_PPC_GOT16_HI =           16;
enum R_PPC_GOT16_HA =           17;
enum R_PPC_PLTREL24 =           18;
enum R_PPC_COPY =               19;
enum R_PPC_GLOB_DAT =           20;
enum R_PPC_JMP_SLOT =           21;
enum R_PPC_RELATIVE =           22;
enum R_PPC_LOCAL24PC =          23;
enum R_PPC_UADDR32 =            24;
enum R_PPC_UADDR16 =            25;
enum R_PPC_REL32 =              26;
enum R_PPC_PLT32 =              27;
enum R_PPC_PLTREL32 =           28;
enum R_PPC_PLT16_LO =           29;
enum R_PPC_PLT16_HI =           30;
enum R_PPC_PLT16_HA =           31;
enum R_PPC_SDAREL16 =           32;
enum R_PPC_SECTOFF =            33;
enum R_PPC_SECTOFF_LO =         34;
enum R_PPC_SECTOFF_HI =         35;
enum R_PPC_SECTOFF_HA =         36;

enum R_PPC64_ADDR64 =           38;
enum R_PPC64_ADDR16_HIGHER =    39;
enum R_PPC64_ADDR16_HIGHERA =   40;
enum R_PPC64_ADDR16_HIGHEST =   41;
enum R_PPC64_ADDR16_HIGHESTA =  42;
enum R_PPC64_UADDR64 =          43;
enum R_PPC64_REL64 =            44;
enum R_PPC64_PLT64 =            45;
enum R_PPC64_PLTREL64 =         46;
enum R_PPC64_TOC16 =            47;
enum R_PPC64_TOC16_LO =         48;
enum R_PPC64_TOC16_HI =         49;
enum R_PPC64_TOC16_HA =         50;
enum R_PPC64_TOC =              51;
enum R_PPC64_DTPMOD64 =         68;
enum R_PPC64_TPREL64 =          73;
enum R_PPC64_DTPREL64 =         78;

enum R_PPC_TLS =                67;
enum R_PPC_DTPMOD32 =           68;
enum R_PPC_TPREL16 =            69;
enum R_PPC_TPREL16_LO =         70;
enum R_PPC_TPREL16_HI =         71;
enum R_PPC_TPREL16_HA =         72;
enum R_PPC_TPREL32 =            73;
enum R_PPC_DTPREL16 =           74;
enum R_PPC_DTPREL16_LO =        75;
enum R_PPC_DTPREL16_HI =        76;
enum R_PPC_DTPREL16_HA =        77;
enum R_PPC_DTPREL32 =           78;
enum R_PPC_GOT_TLSGD16 =        79;
enum R_PPC_GOT_TLSGD16_LO =     80;
enum R_PPC_GOT_TLSGD16_HI =     81;
enum R_PPC_GOT_TLSGD16_HA =     82;
enum R_PPC_GOT_TLSLD16 =        83;
enum R_PPC_GOT_TLSLD16_LO =     84;
enum R_PPC_GOT_TLSLD16_HI =     85;
enum R_PPC_GOT_TLSLD16_HA =     86;
enum R_PPC_GOT_TPREL16 =        87;
enum R_PPC_GOT_TPREL16_LO =     88;
enum R_PPC_GOT_TPREL16_HI =     89;
enum R_PPC_GOT_TPREL16_HA =     90;

enum R_PPC_EMB_NADDR32 =        101;
enum R_PPC_EMB_NADDR16 =        102;
enum R_PPC_EMB_NADDR16_LO =     103;
enum R_PPC_EMB_NADDR16_HI =     104;
enum R_PPC_EMB_NADDR16_HA =     105;
enum R_PPC_EMB_SDAI16 =         106;
enum R_PPC_EMB_SDA2I16 =        107;
enum R_PPC_EMB_SDA2REL =        108;
enum R_PPC_EMB_SDA21 =          109;
enum R_PPC_EMB_MRKREF =         110;
enum R_PPC_EMB_RELSEC16 =       111;
enum R_PPC_EMB_RELST_LO =       112;
enum R_PPC_EMB_RELST_HI =       113;
enum R_PPC_EMB_RELST_HA =       114;
enum R_PPC_EMB_BIT_FLD =        115;
enum R_PPC_EMB_RELSDA =         116;

enum R_SPARC_NONE =             0;
enum R_SPARC_8 =                1;
enum R_SPARC_16 =               2;
enum R_SPARC_32 =               3;
enum R_SPARC_DISP8 =            4;
enum R_SPARC_DISP16 =           5;
enum R_SPARC_DISP32 =           6;
enum R_SPARC_WDISP30 =          7;
enum R_SPARC_WDISP22 =          8;
enum R_SPARC_HI22 =             9;
enum R_SPARC_22 =               10;
enum R_SPARC_13 =               11;
enum R_SPARC_LO10 =             12;
enum R_SPARC_GOT10 =            13;
enum R_SPARC_GOT13 =            14;
enum R_SPARC_GOT22 =            15;
enum R_SPARC_PC10 =             16;
enum R_SPARC_PC22 =             17;
enum R_SPARC_WPLT30 =           18;
enum R_SPARC_COPY =             19;
enum R_SPARC_GLOB_DAT =         20;
enum R_SPARC_JMP_SLOT =         21;
enum R_SPARC_RELATIVE =         22;
enum R_SPARC_UA32 =             23;
enum R_SPARC_PLT32 =            24;
enum R_SPARC_HIPLT22 =          25;
enum R_SPARC_LOPLT10 =          26;
enum R_SPARC_PCPLT32 =          27;
enum R_SPARC_PCPLT22 =          28;
enum R_SPARC_PCPLT10 =          29;
enum R_SPARC_10 =               30;
enum R_SPARC_11 =               31;
enum R_SPARC_64 =               32;
enum R_SPARC_OLO10 =            33;
enum R_SPARC_HH22 =             34;
enum R_SPARC_HM10 =             35;
enum R_SPARC_LM22 =             36;
enum R_SPARC_PC_HH22 =          37;
enum R_SPARC_PC_HM10 =          38;
enum R_SPARC_PC_LM22 =          39;
enum R_SPARC_WDISP16 =          40;
enum R_SPARC_WDISP19 =          41;
enum R_SPARC_GLOB_JMP =         42;
enum R_SPARC_7 =                43;
enum R_SPARC_5 =                44;
enum R_SPARC_6 =                45;
enum R_SPARC_DISP64 =           46;
enum R_SPARC_PLT64 =            47;
enum R_SPARC_HIX22 =            48;
enum R_SPARC_LOX10 =            49;
enum R_SPARC_H44 =              50;
enum R_SPARC_M44 =              51;
enum R_SPARC_L44 =              52;
enum R_SPARC_REGISTER =         53;
enum R_SPARC_UA64 =             54;
enum R_SPARC_UA16 =             55;
enum R_SPARC_TLS_GD_HI22 =      56;
enum R_SPARC_TLS_GD_LO10 =      57;
enum R_SPARC_TLS_GD_ADD =       58;
enum R_SPARC_TLS_GD_CALL =      59;
enum R_SPARC_TLS_LDM_HI22 =     60;
enum R_SPARC_TLS_LDM_LO10 =     61;
enum R_SPARC_TLS_LDM_ADD =      62;
enum R_SPARC_TLS_LDM_CALL =     63;
enum R_SPARC_TLS_LDO_HIX22 =    64;
enum R_SPARC_TLS_LDO_LOX10 =    65;
enum R_SPARC_TLS_LDO_ADD =      66;
enum R_SPARC_TLS_IE_HI22 =      67;
enum R_SPARC_TLS_IE_LO10 =      68;
enum R_SPARC_TLS_IE_LD =        69;
enum R_SPARC_TLS_IE_LDX =       70;
enum R_SPARC_TLS_IE_ADD =       71;
enum R_SPARC_TLS_LE_HIX22 =     72;
enum R_SPARC_TLS_LE_LOX10 =     73;
enum R_SPARC_TLS_DTPMOD32 =     74;
enum R_SPARC_TLS_DTPMOD64 =     75;
enum R_SPARC_TLS_DTPOFF32 =     76;
enum R_SPARC_TLS_DTPOFF64 =     77;
enum R_SPARC_TLS_TPOFF32 =      78;
enum R_SPARC_TLS_TPOFF64 =      79;

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

/**
 * Auxiliary vector entries for passing information to the interpreter.
 *
 * The PowerPC supplement to the SVR4 ABI specification names this "auxv_t",
 * but POSIX lays claim to all symbols ending with "_t".
 */

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

// __ElfType(Auxinfo);

/**
 * Values for a_type.
 */

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

/**
 * Relocation types.
 */

enum R_PPC_COUNT =  37;
enum R_PPC_EMB_COUNT =  (R_PPC_EMB_RELSDA - R_PPC_EMB_NADDR32 + 1);

/**
 * Define "machine" characteristics
 */

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
