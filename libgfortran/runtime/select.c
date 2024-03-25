/* Implement the SELECT statement for character variables.
   Copyright (C) 2008-2024 Free Software Foundation, Inc.

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"


/* The string selection function is defined using a few generic macros
   in select_inc.c, so we avoid code duplication between the various
   character type kinds.  */

#undef  CHARTYPE
#define CHARTYPE char
#undef  SUFFIX
#define SUFFIX(x) x

#include "select_inc.c"


#undef  CHARTYPE
#define CHARTYPE gfc_char4_t
#undef  SUFFIX
#define SUFFIX(x) x ## _char4

#include "select_inc.c"

