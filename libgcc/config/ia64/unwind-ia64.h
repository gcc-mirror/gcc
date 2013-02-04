/* Copyright (C) 1999-2013 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod  <amacleod@cygnus.com>
                  Andrew Haley  <aph@cygnus.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifdef __VMS__
/* On VMS, long is a 32 bit type.  */
typedef unsigned long long unw_word;
typedef long long unw_sword;
#else
typedef unsigned long unw_word;
typedef long unw_sword;
#endif

struct unw_table_entry
{
  unw_word start_offset;
  unw_word end_offset;
  unw_word info_offset;
};

/* Accessors to fields of an unwind info block header.  In this common file to
   be visible from all the units involved in a target implementation.  */
   
#ifndef __USING_SJLJ_EXCEPTIONS__
#define UNW_VER(x)		((x) >> 48)
#define UNW_FLAG_MASK		0x0000ffff00000000
#define UNW_FLAG_OSMASK		0x0000f00000000000
#define UNW_FLAG_EHANDLER(x)	((x) & 0x0000000100000000L)
#define UNW_FLAG_UHANDLER(x)	((x) & 0x0000000200000000L)
#define UNW_LENGTH(x)		((x) & 0x00000000ffffffffL)
#endif

extern struct unw_table_entry *
_Unwind_FindTableEntry (void *pc, unw_word *segment_base,
			unw_word *gp, struct unw_table_entry *ent)
			__attribute__ ((__visibility__ ("hidden")));
