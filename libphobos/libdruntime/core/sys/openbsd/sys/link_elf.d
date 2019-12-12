/**
 * D header file for OpenBSD.
 *
 * Authors:  Iain Buclaw
 * Based-on: core/sys/freebsd/sys
 */
module core.sys.openbsd.sys.link_elf;

version (OpenBSD):

extern (C) nothrow @system:

import core.sys.posix.config;
import core.sys.posix.sys.types;
import core.sys.openbsd.sys.elf;

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
    caddr_t     l_addr;
    char*       l_name;
    void*       l_ld;
    link_map*   l_next;
    link_map*   l_prev;
}

enum
{
    RT_CONSISTENT,
    RT_ADD,
    RT_DELETE,
}

struct r_debug
{
    int                   r_version;
    link_map*             r_map;
    void function(r_debug*, link_map*) r_brk;
    typeof(RT_CONSISTENT) r_state;
    c_ulong               r_ldbase;
}

struct dl_phdr_info
{
    ElfW!"Addr"     dlpi_addr;
    char*           dlpi_name;
    ElfW!"Phdr"*    dlpi_phdr;
    ElfW!"Half"     dlpi_phnum;
};


private alias int function(dl_phdr_info*, size_t, void *) dl_iterate_phdr_cb;
private alias int function(dl_phdr_info*, size_t, void *) @nogc dl_iterate_phdr_cb_ngc;

int dl_iterate_phdr(dl_iterate_phdr_cb __callback, void*__data);
int dl_iterate_phdr(dl_iterate_phdr_cb_ngc __callback, void*__data) @nogc;
