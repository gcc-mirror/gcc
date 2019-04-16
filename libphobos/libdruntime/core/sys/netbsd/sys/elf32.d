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

alias uint16_t Elf32_Half;
alias uint32_t Elf32_Word;
alias int32_t  Elf32_Sword;
alias uint64_t Elf32_Lword;
alias uint32_t Elf32_Addr;
alias uint32_t Elf32_Off;
alias Elf32_Word Elf32_Hashelt;
alias Elf32_Word Elf32_Size;
alias Elf32_Sword Elf32_Ssize;

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

struct Elf32_Dyn
{
  Elf32_Sword   d_tag;
  union _d_un
  {
      Elf32_Word d_val;
      Elf32_Addr d_ptr;
  } _d_un d_un;
}

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

extern (D)
{
    auto ELF32_R_SYM(V)(V val) { return val >> 8; }
    auto ELF32_R_TYPE(V)(V val) { return val & 0xff; }
    auto ELF32_R_INFO(S, T)(S sym, T type) { return (sym << 8) + (type & 0xff); }
}

alias Elf_Note Elf32_Nhdr;

struct Elf32_Move
{
    Elf32_Lword   m_value;
    Elf32_Word    m_info;
    Elf32_Word    m_poffset;
    Elf32_Half    m_repeat;
    Elf32_Half    m_stride;
}

extern (D)
{
    auto ELF32_M_SYM(I)(I info) { return info >> 8; }
    auto ELF32_M_SIZE(I)(I info) { return cast(ubyte)info; }
    auto ELF32_M_INFO(S, SZ)(S sym, SZ size) { return (sym << 8) + cast(ubye)size; }
}

struct Elf32_Cap
{
    Elf32_Word    c_tag;
    union _c_un
    {
        Elf32_Word      c_val;
        Elf32_Addr      c_ptr;
    } _c_un c_un;
}

struct Elf32_Sym
{
    Elf32_Word    st_name;
    Elf32_Addr    st_value;
    Elf32_Word    st_size;
    ubyte st_info;
    ubyte st_other;
    Elf32_Half st_shndx;
}

extern (D)
{
    auto ELF32_ST_BIND(T)(T val) { return cast(ubyte)val >> 4; }
    auto ELF32_ST_TYPE(T)(T val) { return val & 0xf; }
    auto ELF32_ST_INFO(B, T)(B bind, T type) { return (bind << 4) + (type & 0xf); }
    auto ELF32_ST_VISIBILITY(O)(O o) { return o & 0x03; }
}

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

struct Elf32_Verdaux
{
    Elf32_Word    vda_name;
    Elf32_Word    vda_next;
}

struct Elf32_Verneed
{
    Elf32_Half    vn_version;
    Elf32_Half    vn_cnt;
    Elf32_Word    vn_file;
    Elf32_Word    vn_aux;
    Elf32_Word    vn_next;
}

struct Elf32_Vernaux
{
    Elf32_Word    vna_hash;
    Elf32_Half    vna_flags;
    Elf32_Half    vna_other;
    Elf32_Word    vna_name;
    Elf32_Word    vna_next;
}

alias Elf32_Half Elf32_Versym;

struct Elf32_Syminfo
{
    Elf32_Half si_boundto;
    Elf32_Half si_flags;
}
