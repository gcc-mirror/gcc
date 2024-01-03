/* Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

/* _Float128 -> Decimal64 conversion.  */

/* FINE_GRAINED_LIBRARIES is used so we can isolate just to tf_to_dd conversion
   function from dp-bits.c.  */
#define FINE_GRAINED_LIBRARIES	1
#define L_kf_to_dd		1
#define WIDTH			64

#if !defined(__LONG_DOUBLE_128__) || !defined(__LONG_DOUBLE_IEEE128__)
#error "Long double is not IEEE 128-bit"
#endif

/* Use dfp-bit.c to do the real work.  */
#include "dfp-bit.c"
