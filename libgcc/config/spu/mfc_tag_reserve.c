/* Copyright (C) 2007-2016 Free Software Foundation, Inc.

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

#include <spu_mfcio.h>
extern vector unsigned int __mfc_tag_table;

/* Reserves a DMA tag for exclusive use.  This routine returns an available
   tag id in the range 0 to 31 and marks the tag as reserved.  If no tags
   are available, MFC_DMA_TAG_INVALID is returned indicating that all tags
   are already reserved.  */

unsigned int
__mfc_tag_reserve (void)
{
  vector unsigned int mask = (vector unsigned int)
	{ 0x80000000, 0x80000000, 0x80000000, 0x80000000 };
  vector unsigned int count_zeros, is_valid;
  vector signed int count_neg;

  count_zeros = spu_cntlz (__mfc_tag_table);
  count_neg = spu_sub (0, (vector signed int) count_zeros);

  mask = spu_rlmask (mask, (vector signed int) count_neg);
  __mfc_tag_table = spu_andc (__mfc_tag_table, mask);

  is_valid = spu_cmpeq (count_zeros, 32);
  count_zeros = spu_sel (count_zeros, is_valid, is_valid);

  return spu_extract (count_zeros, 0);
}

