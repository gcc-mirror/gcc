/* Temporary library support for decimal floating point.
   Copyright (C) 2006-2021 Free Software Foundation, Inc.

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

#define DFP_EXCEPTIONS_ENABLED 1
#define DFP_HANDLE_EXCEPTIONS(A) __dfp_raise_except(A)

void __dfp_clear_except (void);
int __dfp_test_except (int);
void __dfp_raise_except (int);
