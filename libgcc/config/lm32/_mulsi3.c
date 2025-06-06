/* _mulsi3 for Lattice Mico32.
   Contributed by Jon Beniston <jon@beniston.com>

   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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
   <http://www.gnu.org/licenses/>. */

#include "libgcc_lm32.h"

/* Integer multiplication.  */

USItype
__mulsi3 (USItype a, USItype b)
{
  USItype result;

  result = 0;

  if (a == 0)
    return 0;

  while (b != 0)
    {
      if (b & 1)
	result += a;
      a <<= 1;
      b >>= 1;
    }

  return result;
}
