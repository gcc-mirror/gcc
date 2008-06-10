/* Temporary library support for decimal floating point.
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combine
   executable.)

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include <fenv.h>
#include "dconfig.h"
#include "decContext.h"
#include "decExcept.h"

/* Internal, non-documented functions for testing libgcc functions.
   This support is not sufficient for real use.  */

static int __dfp_except_flags = 0;

/* Clear the dummy exception flags.  */
void
__dfp_clear_except (void)
{
  __dfp_except_flags = 0;
}

/* Return the dummy exception flags corresponding to the mask.  */
int
__dfp_test_except (int mask)
{
  return __dfp_except_flags & mask;
}

/* Set dummy exception flags.  */
void
__dfp_raise_except (int flags)
{
  __dfp_except_flags |= flags;
}
