/* Temporary library support for decimal floating point.
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

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
