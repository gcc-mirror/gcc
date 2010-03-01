/* Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

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
