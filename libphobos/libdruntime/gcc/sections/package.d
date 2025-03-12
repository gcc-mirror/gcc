// Run-time support for retrieving platform-specific sections.
// Copyright (C) 2019-2025 Free Software Foundation, Inc.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

module gcc.sections;

version (CRuntime_Glibc)  version = SectionsElf;
version (CRuntime_Musl)   version = SectionsElf;
version (CRuntime_UClibc) version = SectionsElf;
version (FreeBSD)         version = SectionsElf;
version (NetBSD)          version = SectionsElf;
version (OpenBSD)         version = SectionsElf;
version (DragonFlyBSD)    version = SectionsElf;
version (Solaris)         version = SectionsElf;
version (OSX)             version = SectionsMacho;
version (Windows)         version = SectionsPeCoff;

version (SectionsElf)
    public import gcc.sections.elf;
else version (SectionsMacho)
    public import gcc.sections.macho;
else version (SectionsPeCoff)
    public import gcc.sections.pecoff;
else
    static assert(0, "unimplemented");

import core.internal.traits : externDFunc;

version (Shared)
{
    // interface for core.thread.osthread to inherit loaded libraries
    pragma(mangle, externDFunc!("rt.sections_elf_shared.pinLoadedLibraries",
                                void* function() @nogc nothrow).mangleof)
    void* pinLoadedLibraries() @nogc nothrow;

    pragma(mangle, externDFunc!("rt.sections_elf_shared.unpinLoadedLibraries",
                                void function(void*) @nogc nothrow).mangleof)
    void unpinLoadedLibraries(void* p) @nogc nothrow;

    pragma(mangle, externDFunc!("rt.sections_elf_shared.inheritLoadedLibraries",
                                void function(void*) @nogc nothrow).mangleof)
    void inheritLoadedLibraries(void* p) @nogc nothrow;

    pragma(mangle, externDFunc!("rt.sections_elf_shared.cleanupLoadedLibraries",
                                void function() @nogc nothrow).mangleof)
    void cleanupLoadedLibraries() @nogc nothrow;
}

version (SectionsElf)
{
    // interface for core.thread.osthread to adjust stack size
    pragma(mangle, externDFunc!("rt.sections_elf_shared.sizeOfTLS",
                                size_t function() @nogc nothrow).mangleof)
    size_t sizeOfTLS() @nogc nothrow;
}
