/**
 * D header file for GNU/Linux
 *
 * $(LINK2 http://sourceware.org/git/?p=glibc.git;a=blob;f=elf/link.h, glibc elf/link.h)
 */
module core.sys.linux.link;

version (linux):
extern (C):
nothrow:

version (ARM)     version = ARM_Any;
version (AArch64) version = ARM_Any;
version (HPPA)    version = HPPA_Any;
version (MIPS32)  version = MIPS_Any;
version (MIPS64)  version = MIPS_Any;
version (PPC)     version = PPC_Any;
version (PPC64)   version = PPC_Any;
version (RISCV32) version = RISCV_Any;
version (RISCV64) version = RISCV_Any;
version (S390)    version = IBMZ_Any;
version (SPARC)   version = SPARC_Any;
version (SPARC64) version = SPARC_Any;
version (SystemZ) version = IBMZ_Any;
version (X86)     version = X86_Any;
version (X86_64)  version = X86_Any;

import core.stdc.stdint : uintptr_t, uint32_t, uint64_t;
import core.sys.linux.config : __WORDSIZE;
import core.sys.linux.dlfcn : Lmid_t;
import core.sys.linux.elf;

// <bits/elfclass.h>
version (X86_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (HPPA_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (MIPS_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (PPC_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (ARM_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (RISCV_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (SPARC_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    alias uint32_t Elf_Symndx;
}
else version (IBMZ_Any)
{
    // http://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/unix/sysv/linux/s390/bits/elfclass.h
    alias __WORDSIZE __ELF_NATIVE_CLASS;
    static if (__WORDSIZE == 64)
        alias uint64_t Elf_Symndx;
    else
        alias uint32_t Elf_Symndx;
}
else
    static assert(0, "unimplemented");
// <bits/elfclass.h>

template ElfW(string type)
{
    mixin("alias Elf"~__ELF_NATIVE_CLASS.stringof~"_"~type~" ElfW;");
}

enum
{
    RT_CONSISTENT,
    RT_ADD,
    RT_DELETE,
}

struct r_debug
{
    int r_version;
    link_map* r_map;
    ElfW!"Addr" r_brk;
    typeof(RT_CONSISTENT) r_state;
    ElfW!"Addr" r_ldbase;
}

extern r_debug _r_debug;
extern ElfW!"Dyn"* _DYNAMIC;

struct link_map
{
    ElfW!"Addr" l_addr;
    char* l_name;
    ElfW!"Dyn"* l_ld;
    link_map* l_next, l_prev;
}

enum
{
    LA_ACT_CONSISTENT,
    LA_ACT_ADD,
    LA_ACT_DELETE,
}

enum
{
    LA_SER_ORIG = 0x01,
    LA_SER_LIBPATH = 0x02,
    LA_SER_RUNPATH = 0x04,
    LA_SER_CONFIG = 0x08,
    LA_SER_DEFAULT = 0x40,
    LA_SER_SECURE = 0x80,
}


enum
{
    LA_FLG_BINDTO = 0x01,
    LA_FLG_BINDFROM = 0x02,
}


enum
{
    LA_SYMB_NOPLTENTER = 0x01,
    LA_SYMB_NOPLTEXIT = 0x02,
    LA_SYMB_STRUCTCALL = 0x04,
    LA_SYMB_DLSYM = 0x08,
    LA_SYMB_ALTVALUE = 0x10,
}

struct dl_phdr_info
{
    ElfW!"Addr" dlpi_addr;
    const(char)* dlpi_name;
    const(ElfW!"Phdr")* dlpi_phdr;
    ElfW!"Half" dlpi_phnum;

    // check the SIZE argument of the dl_iterate_phdr callback whether
    // the following members are available
    ulong dlpi_adds;
    ulong dlpi_subs;

    size_t dlpi_tls_modid;
    void *dlpi_tls_data;
}

private alias extern(C) int function(dl_phdr_info*, size_t, void *) dl_iterate_phdr_cb;
private alias extern(C) int function(dl_phdr_info*, size_t, void *) @nogc dl_iterate_phdr_cb_ngc;
extern int dl_iterate_phdr(dl_iterate_phdr_cb __callback, void*__data);
extern int dl_iterate_phdr(dl_iterate_phdr_cb_ngc __callback, void*__data) @nogc;

// ld.so auditing interfaces prototypes have to be defined by the auditing DSO.
extern uint la_version(uint __version);
extern void la_activity(uintptr_t *__cookie, uint __flag);
extern char* la_objsearch(const(char)* __name, uintptr_t* __cookie,
                          uint __flag);
extern uint la_objopen(link_map* __map, Lmid_t __lmid,
                       uintptr_t* __cookie);
extern void la_preinit(uintptr_t* __cookie);
extern uintptr_t la_symbind32(Elf32_Sym* __sym, uint __ndx,
                              uintptr_t* __refcook, uintptr_t* __defcook,
                              uint *__flags, const(char)* __symname);
extern uintptr_t la_symbind64(Elf64_Sym* __sym, uint __ndx,
                              uintptr_t* __refcook, uintptr_t* __defcook,
                              uint* __flags, const(char)* __symname);
extern uint la_objclose(uintptr_t *__cookie);
