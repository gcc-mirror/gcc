/**
 * D header file for FreeBSD.
 *
 * $(LINK2 http://svnweb.freebsd.org/base/head/sys/sys/elf64.h?view=markup, sys/elf64.h)
 */
module core.sys.freebsd.sys.elf64;

version (FreeBSD):
extern (C):
pure:
nothrow:

import core.stdc.stdint;
public import core.sys.freebsd.sys.elf_common;

alias uint16_t Elf64_Half;
alias uint32_t Elf64_Word;
alias int32_t  Elf64_Sword;
alias uint64_t Elf64_Lword;
alias uint64_t Elf64_Xword;
alias int64_t  Elf64_Sxword;
alias uint64_t Elf64_Addr;
alias uint64_t Elf64_Off;
alias Elf64_Word Elf64_Hashelt;
alias Elf64_Xword Elf64_Size;
alias Elf64_Sxword Elf64_Ssize;

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

struct Elf64_Dyn
{
  Elf64_Sxword  d_tag;
  union _d_un
  {
      Elf64_Xword d_val;
      Elf64_Addr d_ptr;
  } _d_un d_un;
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
    auto ELF64_R_SYM(I)(I i) { return i >> 32; }
    auto ELF64_R_TYPE(I)(I i) { return i & 0xffffffff; }
    auto ELF64_R_INFO(S, T)(S sym, T type) { return (sym << 32) + (type & 0xffffffff); }

    auto ELF64_R_TYPE_DATA(I)(I i) { return (cast(Elf64_Xword) i << 32) >> 40; }
    auto ELF64_R_TYPE_ID(I)(I i) { return (cast(Elf64_Xword) i << 56 ) >> 56; }
    auto ELF64_R_TYPE_INFO(D, T)(D d, T t) { return cast(Elf64_Xword) d << 8 + cast(Elf64_Xword) t; }
}

alias Elf_Note Elf64_Nhdr;

struct Elf64_Move
{
    Elf64_Lword   m_value;
    Elf64_Xword   m_info;
    Elf64_Xword   m_poffset;
    Elf64_Half    m_repeat;
    Elf64_Half    m_stride;
}

extern (D)
{
    auto ELF64_M_SYM(I)(I info) { return info >> 8; }
    auto ELF64_M_SIZE(I)(I info) { return cast(ubyte)info; }
    auto ELF64_M_INFO(S, SZ)(S sym, SZ size) { return (sym << 8) + cast(ubye)size; }
}

struct Elf64_Cap
{
    Elf64_Xword   c_tag;
    union _c_un
    {
        Elf64_Xword     c_val;
        Elf64_Addr      c_ptr;
    } _c_un c_un;
}

struct Elf64_Sym
{
    Elf64_Word    st_name;
    ubyte st_info;
    ubyte st_other;
    Elf64_Half st_shndx;
    Elf64_Addr    st_value;
    Elf64_Xword   st_size;
}

extern (D)
{
    auto ELF64_ST_BIND(T)(T val) { return cast(ubyte)val >> 4; }
    auto ELF64_ST_TYPE(T)(T val) { return val & 0xf; }
    auto ELF64_ST_INFO(B, T)(B bind, T type) { return (bind << 4) + (type & 0xf); }
    auto ELF64_ST_VISIBILITY(O)(O o) { return o & 0x03; }
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

struct Elf64_Verdaux
{
    Elf64_Word    vda_name;
    Elf64_Word    vda_next;
}

struct Elf64_Verneed
{
    Elf64_Half    vn_version;
    Elf64_Half    vn_cnt;
    Elf64_Word    vn_file;
    Elf64_Word    vn_aux;
    Elf64_Word    vn_next;
}

struct Elf64_Vernaux
{
    Elf64_Word    vna_hash;
    Elf64_Half    vna_flags;
    Elf64_Half    vna_other;
    Elf64_Word    vna_name;
    Elf64_Word    vna_next;
}

alias Elf64_Half Elf64_Versym;

struct Elf64_Syminfo
{
    Elf64_Half si_boundto;
    Elf64_Half si_flags;
}
