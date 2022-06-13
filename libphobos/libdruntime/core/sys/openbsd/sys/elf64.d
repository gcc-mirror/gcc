/**
 * D header file for OpenBSD.
 *
 * Authors:  Iain Buclaw
 * Based-on: core/sys/freebsd/sys
 */
module core.sys.openbsd.sys.elf64;

version (OpenBSD):
extern (C):
pure:
nothrow:

import core.stdc.stdint;
public import core.sys.openbsd.sys.elf_common;

alias uint64_t Elf64_Lword;
alias Elf64_Word Elf64_Hashelt;
alias Elf64_Xword Elf64_Size;
alias Elf64_Sxword Elf64_Ssize;

struct Elf64_Dyn
{
  Elf64_Sxword  d_tag;
  union _d_un
  {
      Elf64_Xword d_val;
      Elf64_Addr d_ptr;
  } _d_un d_un;
}

extern (D) pure
{
    auto ELF64_R_TYPE_DATA(I)(I i) { return (cast(Elf64_Xword) i << 32) >> 40; }
    auto ELF64_R_TYPE_ID(I)(I i) { return (cast(Elf64_Xword) i << 56 ) >> 56; }
    auto ELF64_R_TYPE_INFO(D, T)(D d, T t) { return cast(Elf64_Xword) d << 8 + cast(Elf64_Xword) t; }
}

alias Elf_Note Elf64_Nhdr;

struct Elf64_Cap
{
    Elf64_Xword   c_tag;
    union _c_un
    {
        Elf64_Xword     c_val;
        Elf64_Addr      c_ptr;
    } _c_un c_un;
}

extern (D)
{
    auto ELF64_ST_VISIBILITY(O)(O o) { return o & 0x03; }
}
