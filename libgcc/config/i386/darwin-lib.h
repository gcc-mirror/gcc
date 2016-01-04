/* Target definitions for x86 running Darwin, library renames.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

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

/* The system ___divdc3 routine in libSystem on darwin10 is not
   accurate to 1ulp, ours is, so we avoid ever using the system name
   for this routine and instead install a non-conflicting name that is
   accurate.  See darwin_rename_builtins.  */
#ifdef L_divdc3
#define DECLARE_LIBRARY_RENAMES \
  asm(".text; ___divdc3: jmp ___ieee_divdc3 ; .globl ___divdc3");
#endif
