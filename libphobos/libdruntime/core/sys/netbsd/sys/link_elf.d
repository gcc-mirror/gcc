/**
 * D header file for NetBSD.
 *
 * http://cvsweb.netbsd.org/bsdweb.cgi/~checkout~/src/include/link_elf.h
 */
module core.sys.netbsd.sys.link_elf;

version (NetBSD):
extern (C):
nothrow:

import core.stdc.stdint : uint64_t;
import core.sys.netbsd.sys.elf;

version (D_LP64)
    enum __ELF_NATIVE_CLASS = 64;
else
    enum __ELF_NATIVE_CLASS = 32;

template ElfW(string type)
{
    mixin("alias Elf"~__ELF_NATIVE_CLASS.stringof~"_"~type~" ElfW;");
}

struct link_map
{
    char*           l_addr;

    version (MIPS32)
        char*       l_offs;
    version (MIPS64)
        char*       l_offs;

    char*           l_name;
    void*           l_ld;
    link_map*       l_next;
    link_map*       l_prev;
}
alias link_map Link_map;

enum
{
    RT_CONSISTENT,
    RT_ADD,
    RT_DELETE,
}

struct r_debug
{
    int             r_version;
    link_map*       r_map;
    void function(r_debug*, link_map*) r_brk;
};

struct dl_phdr_info
{
    ElfW!"Addr"     dlpi_addr;
    char*           dlpi_name;
    ElfW!"Phdr"*    dlpi_phdr;
    ElfW!"Half"     dlpi_phnum;
    uint64_t        dlpi_adds;
    uint64_t        dlpi_subs;
    size_t          dlpi_tls_modid;
    void*           dlpi_tls_data;
};


private alias extern(C) int function(dl_phdr_info*, size_t, void *) dl_iterate_phdr_cb;
private alias extern(C) int function(dl_phdr_info*, size_t, void *) @nogc dl_iterate_phdr_cb_ngc;
extern int dl_iterate_phdr(dl_iterate_phdr_cb __callback, void*__data);
extern int dl_iterate_phdr(dl_iterate_phdr_cb_ngc __callback, void*__data) @nogc;
