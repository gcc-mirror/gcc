/* 16-bit trapping arithmetic routines for R8C/M16C/M32C
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* See the comment by the definition of LIBGCC2_UNITS_PER_WORD in
   m32c.h for why we are creating extra versions of some of the
   functions defined in libgcc2.c.

   Note - this file is separate from lib2funcs.c so that the following
   functions will appear in the their object file.  This is necessary
   because they call abort() which is defined in the C library whereas
   the functions in lib2funcs.c are completely self sufficient.  */

#define LIBGCC2_UNITS_PER_WORD 2

#define L_mulvsi3
#define L_negvsi2
#define L_addvsi3
#define L_subvsi3

#include "libgcc2.c"
