// Run-time support for retrieving platform-specific sections.
// Copyright (C) 2019 Free Software Foundation, Inc.

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

version (CRuntime_Glibc)
    public import gcc.sections.elf_shared;
else version (CRuntime_Musl)
    public import gcc.sections.elf_shared;
else version (CRuntime_UClibc)
    public import gcc.sections.elf_shared;
else version (FreeBSD)
    public import gcc.sections.elf_shared;
else version (NetBSD)
    public import gcc.sections.elf_shared;
else version (DragonFlyBSD)
    public import gcc.sections.elf_shared;
else version (Solaris)
    public import gcc.sections.elf_shared;
else version (OSX)
    public import gcc.sections.osx;
else version (CRuntime_DigitalMars)
    public import gcc.sections.win32;
else version (CRuntime_Microsoft)
    public import gcc.sections.win64;
else version (CRuntime_Bionic)
    public import gcc.sections.android;
else
    static assert(0, "unimplemented");
