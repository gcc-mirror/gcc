/* Provide minfo section bracketing for D executables and shared libraries
   when the linker doesn't provide it.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Avoid interference with targets without support for named sections.  */
#ifdef __ELF__

#ifdef DRT_BEGIN
void *__start_minfo[]
__attribute__((used, section("minfo"), aligned(sizeof(void *)))) = { };
#endif

#ifdef DRT_END
void *__stop_minfo[]
__attribute__((used, section("minfo"), aligned(sizeof(void *)))) = { };
#endif

#endif /* __ELF__ */
