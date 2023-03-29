/**
 * D header file for GNU/Linux
 *
 * $(LINK2 http://sourceware.org/git/?p=glibc.git;a=blob;f=elf/elf.h, glibc elf/elf.h)
 */
module core.sys.linux.elf;

version (linux):
extern (C):
pure:
nothrow:

import core.stdc.stdint;
public import core.sys.elf;

extern (D)
{
    auto ELF32_ST_VISIBILITY(O)(O o) { return o & 0x03; }
    alias ELF32_ST_VISIBILITY ELF64_ST_VISIBILITY;
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

enum NT_PRSTATUS =     1;
enum NT_FPREGSET =     2;
enum NT_PRPSINFO =     3;
enum NT_PRXREG =       4;
enum NT_TASKSTRUCT =   4;
enum NT_PLATFORM =     5;
enum NT_AUXV =         6;
enum NT_GWINDOWS =     7;
enum NT_ASRS =         8;
enum NT_PSTATUS =      10;
enum NT_PSINFO =       13;
enum NT_PRCRED =       14;
enum NT_UTSNAME =      15;
enum NT_LWPSTATUS =    16;
enum NT_LWPSINFO =     17;
enum NT_PRFPXREG =     20;
enum NT_SIGINFO =      0x53494749;
enum NT_FILE =         0x46494c45;
enum NT_PRXFPREG =     0x46e62b7f;
enum NT_PPC_VMX =      0x100;
enum NT_PPC_SPE =      0x101;
enum NT_PPC_VSX =      0x102;
enum NT_386_TLS =      0x200;
enum NT_386_IOPERM =   0x201;
enum NT_X86_XSTATE =   0x202;
enum NT_S390_HIGH_GPRS =       0x300;
enum NT_S390_TIMER =   0x301;
enum NT_S390_TODCMP =  0x302;
enum NT_S390_TODPREG = 0x303;
enum NT_S390_CTRS =    0x304;
enum NT_S390_PREFIX =  0x305;
enum NT_S390_LAST_BREAK =      0x306;
enum NT_S390_SYSTEM_CALL =     0x307;
enum NT_S390_TDB =     0x308;
enum NT_ARM_VFP =      0x400;
enum NT_ARM_TLS =      0x401;
enum NT_ARM_HW_BREAK = 0x402;
enum NT_ARM_HW_WATCH = 0x403;

enum NT_VERSION =      1;

struct Elf32_Dyn
{
  Elf32_Sword   d_tag;
  union _d_un
  {
      Elf32_Word d_val;
      Elf32_Addr d_ptr;
  } _d_un d_un;
}

struct Elf64_Dyn
{
  Elf64_Sxword  d_tag;
  union _d_un
  {
      Elf64_Xword d_val;
      Elf64_Addr d_ptr;
  } _d_un d_un;
}

enum NT_GNU_ABI_TAG =      1;
enum NT_GNU_HWCAP =        2;
enum NT_GNU_BUILD_ID =     3;
enum NT_GNU_GOLD_VERSION = 4;

struct Elf32_auxv_t
{
    uint32_t a_type;
    union _a_un
    {
        uint32_t a_val;
    } _a_un a_un;
}

struct Elf64_auxv_t
{
    uint64_t a_type;
    union _a_un
    {
        uint64_t a_val;
    } _a_un a_un;
}

enum AT_NULL =         0;
enum AT_IGNORE =       1;
enum AT_EXECFD =       2;
enum AT_PHDR =         3;
enum AT_PHENT =        4;
enum AT_PHNUM =        5;
enum AT_PAGESZ =       6;
enum AT_BASE =         7;
enum AT_FLAGS =        8;
enum AT_ENTRY =        9;
enum AT_NOTELF =       10;
enum AT_UID =          11;
enum AT_EUID =         12;
enum AT_GID =          13;
enum AT_EGID =         14;
enum AT_CLKTCK =       17;
enum AT_PLATFORM =     15;
enum AT_HWCAP =        16;
enum AT_FPUCW =        18;
enum AT_DCACHEBSIZE =  19;
enum AT_ICACHEBSIZE =  20;
enum AT_UCACHEBSIZE =  21;
enum AT_IGNOREPPC =    22;

enum AT_SECURE =       23;

enum AT_BASE_PLATFORM = 24;

enum AT_RANDOM =       25;

enum AT_HWCAP2 =       26;

enum AT_EXECFN =       31;
enum AT_SYSINFO =      32;
enum AT_SYSINFO_EHDR = 33;

enum AT_L1I_CACHESHAPE =       34;
enum AT_L1D_CACHESHAPE =       35;
enum AT_L2_CACHESHAPE =        36;
enum AT_L3_CACHESHAPE =        37;
