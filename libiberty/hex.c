/* Hex character manipulation support.
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "libiberty.h"

char _hex_value[_hex_array_size];

void hex_init ()
{
  int i;
  for (i = 0; i < _hex_array_size; i++)
    _hex_value[i] = _hex_bad;
  for (i = 0; i < 10; i++)
    _hex_value['0' + i] = i;
  for (i = 0; i < 6; i++)
    _hex_value['a' + i] = _hex_value['A' + i] = 10 + i;
}
