/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
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

#include "upc_config.h"
#include "upc_sysdep.h"
#include "upc_defs.h"
#include "upc_sup.h"
#include "upc_access.h"
#include "upc_mem.h"

void
upc_memcpy (upc_shared_ptr_t dest, upc_shared_ptr_t src, size_t n)
{
  __upc_memcpy (dest, src, n);
}

void
upc_memget (void *dest, upc_shared_ptr_t src, size_t n)
{
  __upc_memget (dest, src, n);
}

void
upc_memput (upc_shared_ptr_t dest, const void *src, size_t n)
{
  __upc_memput (dest, src, n);
}

void
upc_memset (upc_shared_ptr_t dest, int c, size_t n)
{
  __upc_memset (dest, c, n);
}
