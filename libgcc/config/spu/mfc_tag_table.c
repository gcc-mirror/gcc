/* Copyright (C) 2007-2019 Free Software Foundation, Inc.

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

/* The free tag table used by the MFC tag manager, with tag0
   reserved for the overlay manager.  */
__vector unsigned int
__mfc_tag_table = (__vector unsigned int) { 0x7FFFFFFF, -1, -1, -1 };

/* Arrange to release tag0 if overlays are not present.  */
static void __mfc_tag_init (void) __attribute__ ((constructor));

static void
__mfc_tag_init (void)
{
  extern void _ovly_table __attribute__ ((weak));

  if (&_ovly_table == 0)
    __mfc_tag_table = (__vector unsigned int) { -1, -1, -1, -1 };
}
