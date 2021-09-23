/* Copyright (C) 2021 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.  */

#include "lib2-gcn.h"

UTItype
__bswapti2 (UTItype x)
{
  UDItype lo, hi, outlo, outhi;
  lo = (UDItype) x;
  hi = (UDItype) (x >> 64);
  outhi = (lo >> 56) & 0xff;
  outhi |= ((lo >> 48) & 0xff) << 8;
  outhi |= ((lo >> 40) & 0xff) << 16;
  outhi |= ((lo >> 32) & 0xff) << 24;
  outhi |= ((lo >> 24) & 0xff) << 32;
  outhi |= ((lo >> 16) & 0xff) << 40;
  outhi |= ((lo >> 8) & 0xff) << 48;
  outhi |= (lo & 0xff) << 56;
  outlo = (hi >> 56) & 0xff;
  outlo |= ((hi >> 48) & 0xff) << 8;
  outlo |= ((hi >> 40) & 0xff) << 16;
  outlo |= ((hi >> 32) & 0xff) << 24;
  outlo |= ((hi >> 24) & 0xff) << 32;
  outlo |= ((hi >> 16) & 0xff) << 40;
  outlo |= ((hi >> 8) & 0xff) << 48;
  outlo |= (hi & 0xff) << 56;
  return ((UTItype) outhi << 64) | outlo;
}
