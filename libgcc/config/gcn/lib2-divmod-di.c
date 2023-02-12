/* Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Mentor Graphics, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "lib2-gcn.h"

/* We really want DImode here: override LIBGCC2_UNITS_PER_WORD.  */
#define LIBGCC2_UNITS_PER_WORD 4
#define TARGET_HAS_NO_HW_DIVIDE

#define L_divmoddi4
#define L_divdi3
#define L_moddi3
#define L_udivdi3
#define L_umoddi3

#include "libgcc2.c"
