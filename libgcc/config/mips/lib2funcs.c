/* libgcc routines for MIPS
   Copyright (C) 2013 Free Software Foundation, Inc.
   DMULT/DDIV replacement support by Juergen Urban, JuergenUrban@gmx.de.

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

#if defined(__mips64) && defined(_MIPS_ARCH_R5900)

/* Build DI version of libgcc functions. */
#define LIBGCC2_UNITS_PER_WORD 4

/* The following function is needed when !ISA_HAS_DMULT. */
#define L_muldi3

/* The following functions are needed when !ISA_HAS_DDIV. */
#define L_divdi3
#define L_moddi3
#define L_udivdi3
#define L_umoddi3
#define L_udivmoddi4

/* Use generic definition of functions. */
#include "libgcc2.c"

#endif
