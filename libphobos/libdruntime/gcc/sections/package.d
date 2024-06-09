// Run-time support for retrieving platform-specific sections.
// Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

version (Shared)
{
    // interface for core.thread to inherit loaded libraries
    void* pinLoadedLibraries() nothrow @nogc;
    void unpinLoadedLibraries(void* p) nothrow @nogc;
    void inheritLoadedLibraries(void* p) nothrow @nogc;
    void cleanupLoadedLibraries() nothrow @nogc;
}
