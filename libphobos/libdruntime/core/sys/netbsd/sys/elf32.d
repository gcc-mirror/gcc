/**
 * D header file for NetBSD.
 *
 * http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/sys/sys/exec_elf.h
 */
module core.sys.netbsd.sys.elf32;

version (NetBSD):
extern (C):
pure:
nothrow:

import core.stdc.stdint;
public import core.sys.netbsd.sys.elf_common;

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
