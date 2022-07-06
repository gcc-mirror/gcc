/**
 * D header file for DragonFlyBSD.
 *
 * Authors: Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys.elf32;

version (DragonFlyBSD):

extern (C):

import core.stdc.stdint;
public import core.sys.dragonflybsd.sys.elf_common;

alias uint64_t Elf32_Lword;
alias Elf32_Word Elf32_Hashelt;
alias Elf32_Word Elf32_Size;
alias Elf32_Sword Elf32_Ssize;

struct Elf32_Dyn
{
  Elf32_Sword   d_tag;
  union _d_un
  {
      Elf32_Word d_val;
      Elf32_Addr d_ptr;
  } _d_un d_un;
}

alias Elf_Note Elf32_Nhdr;

struct Elf32_Cap
{
    Elf32_Word    c_tag;
    union _c_un
    {
        Elf32_Word      c_val;
        Elf32_Addr      c_ptr;
    } _c_un c_un;
}

extern (D)
{
    auto ELF32_ST_VISIBILITY(O)(O o) { return o & 0x03; }
}
