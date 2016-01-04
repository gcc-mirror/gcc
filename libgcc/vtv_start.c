/* Copyright (C) 2012-2016 Free Software Foundation, Inc.

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

/* This file is part of the vtable verification feature (for a
   detailed description of the feature, see comments in
   vtable-verify.c).  The vtable verification feature creates
   certain global symbols that need to be read-write sometimes during
   program execution, and read-only at others.  It uses 'mprotect' to
   change the memory protections of the pages on which these variables
   are stored.  In order to not affect the protections of other
   program variables, these variables are put into a special named
   section, ".vtable_map_vars", which is page-aligned at the start,
   and which is padded with a page-sized amount of zeros at the end.
   To make this section page aligned, we create a special symbol,
   "_vtable_map_vars_start" which we make the very first thing that
   goes into the section.  This file defines that symbol (and only
   that symbol).  GCC compiles this file into vtv_start.o, and
   inserts vtv_start.o into the link line immediately after
   crtbegin.o, if the program is compiled with -fvtable.verify.

   In order to pad the ".vtable_map_vars" section with a page-sized
   amount of zeros at the end, there is a second symbol,
   _vtable_map_vars_end, which is defined in another file, vtv_end.c.
   This second symbol is a page-sized array of chars, zero-filled, and
   is the very last thing to go into the section.  When the GCC driver
   inserts vtv_start.o into the link line (just after crtbegin.o) it
   also inserts vtv_end.o into the link line, just before crtend.o.
   This has the desired effect of making our section page-aligned and
   page-size paded, ensuring that no other program data lands on our
   pages.  */

#include "vtv-change-permission.h"

__attribute__ ((constructor(98))) void
__VLTunprotect (void)
{
  __VLTChangePermission (__VLTP_READ_WRITE);
}

/* Page-aligned symbol to mark beginning of .vtable_map_vars section.  */
char _vtable_map_vars_start []
__attribute__ ((__visibility__ ("protected"), used, aligned(VTV_PAGE_SIZE),
		section(".vtable_map_vars")))
  = { };
