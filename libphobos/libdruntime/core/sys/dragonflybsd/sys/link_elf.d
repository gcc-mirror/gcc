/**
 * D header file for DragonFlyBSD.
 *
 * $(LINK2 http://svnweb.freebsd.org/base/head/sys/sys/link_elf.h?view=markup, sys/link_elf.h)
 * Authors: Diederik de Groot(port:DragonFlyBSD)
 * Copied:  From core/sys/freebsd/sys
 */
module core.sys.dragonflybsd.sys.link_elf;

version (DragonFlyBSD):

extern (C) nothrow @system:

import core.stdc.stdint : uint64_t;
import core.sys.dragonflybsd.sys.elf;

version (D_LP64)
    enum __ELF_NATIVE_CLASS = 64;
else
    enum __ELF_NATIVE_CLASS = 32;

template ElfW(string type)
{
    mixin("alias Elf"~__ELF_NATIVE_CLASS.stringof~"_"~type~" ElfW;");
}

enum LA_SER_ORIG =      0x01;
enum LA_SER_LIBPATH =   0x02;
enum LA_SER_RUNPATH =   0x04;
enum LA_SER_CONFIG =    0x08;
enum LA_SER_DEFAULT =   0x40;
enum LA_SER_SECURE =    0x80;

struct link_map
{
    char*           l_addr;

    version (MIPS32)
        char*       l_offs;
    version (MIPS64)
        char*       l_offs;

    char*           l_name;
    void*           l_ld;
    link_map*       l_next, l_prev;
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


private alias int function(dl_phdr_info*, size_t, void *) dl_iterate_phdr_cb;
private alias int function(dl_phdr_info*, size_t, void *) @nogc dl_iterate_phdr_cb_ngc;

int dl_iterate_phdr(dl_iterate_phdr_cb __callback, void*__data);
int dl_iterate_phdr(dl_iterate_phdr_cb_ngc __callback, void*__data) @nogc;
int _rtld_addr_phdr(const void*, dl_phdr_info*) @nogc;
int _rtld_get_stack_prot() @nogc;
