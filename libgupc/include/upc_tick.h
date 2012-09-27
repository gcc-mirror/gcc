/* Copyright (C) 2012
   Free Software Foundation, Inc. 
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
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

#ifndef _UPC_TICK_H_
#define _UPC_TICK_H_ 1

/* Required, for uint64_t.  */
#include <stdint.h>

typedef uint64_t upc_tick_t;

#define     UPC_TICK_MIN 0ULL
#define     UPC_TICK_MAX 0xffffffffffffffffULL

extern upc_tick_t upc_ticks_now();
extern uint64_t upc_ticks_to_ns(upc_tick_t ticks);

#endif /* _UPC_TICK_H_ */
