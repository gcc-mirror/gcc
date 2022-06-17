/**
 * D header file for OpenBSD.
 *
 * Authors:  Iain Buclaw
 * Based-on: core/sys/freebsd/sys
 */
module core.sys.openbsd.sys.elf_common;

version (OpenBSD):
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

extern (D) pure @safe
{
    auto IS_ELF(T)(T ehdr) { return ehdr.e_ident[EI_MAG0] == ELFMAG0 &&
                                    ehdr.e_ident[EI_MAG1] == ELFMAG1 &&
                                    ehdr.e_ident[EI_MAG2] == ELFMAG2 &&
                                    ehdr.e_ident[EI_MAG3] == ELFMAG3; }
}

enum EM_486 =           6;
enum EM_MIPS_RS4_BE =  10;
enum EM_ALPHA_STD =    41;

enum PT_SUNW_EH_FRAME = PT_GNU_EH_FRAME;

enum SHT_GNU_INCREMENTAL_INPUTS =  0x6fff4700;

enum SHT_SUNW_verdef =   0x6ffffffd;
enum SHT_SUNW_verneed =  0x6ffffffe;
enum SHT_SUNW_versym =   0x6fffffff;

enum NT_PRSTATUS =     1;
enum NT_FPREGSET =     2;
enum NT_PRPSINFO =     3;
enum NT_TASKSTRUCT =   4;
enum NT_AUXV =         6;

enum DT_USED =         0x7ffffffe;

enum DF_1_BIND_NOW =   0x00000001;

alias VER_NDX VER_DEF_IDX;

enum VER_FLG_INFO =    0x4;

enum VER_NDX_GIVEN =           2;
enum VER_NDX_HIDDEN =      32768;

extern (D) pure @safe
{
    auto VER_NDX(V)(V v) { return v & ~(1u << 15); }
}

enum VER_NEED_WEAK =    32768;
enum VER_NEED_HIDDEN = VER_NDX_HIDDEN;
alias VER_NDX VER_NEED_IDX;

enum VERSYM_HIDDEN =   0x8000;
enum VERSYM_VERSION =  0x7fff;
enum ELF_VER_CHR =     '@';
