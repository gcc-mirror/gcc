/* Infrastructure for tracking user variable locations and values
   throughout compilation.
   Copyright (C) 2010, 2011, 2012  Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_VALTRACK_H
#define GCC_VALTRACK_H

#include "bitmap.h"
#include "df.h"
#include "rtl.h"
#include "basic-block.h"

/* Debug uses of dead regs.  */

/* Node of a linked list of uses of dead REGs in debug insns.  */
struct dead_debug_use
{
  df_ref use;
  struct dead_debug_use *next;
};

/* Linked list of the above, with a bitmap of the REGs in the
   list.  */
struct dead_debug
{
  struct dead_debug_use *head;
  bitmap used;
  bitmap to_rescan;
};

/* This type controls the behavior of dead_debug_insert_temp WRT
   UREGNO and INSN.  */
enum debug_temp_where
  {
    /* Bind a newly-created debug temporary to a REG for UREGNO, and
       insert the debug insn before INSN.  REG is expected to die at
       INSN.  */
    DEBUG_TEMP_BEFORE_WITH_REG = -1,
    /* Bind a newly-created debug temporary to the value INSN stores
       in REG, and insert the debug insn before INSN.  */
    DEBUG_TEMP_BEFORE_WITH_VALUE = 0,
    /* Bind a newly-created debug temporary to a REG for UREGNO, and
       insert the debug insn after INSN.  REG is expected to be set at
       INSN.  */
    DEBUG_TEMP_AFTER_WITH_REG = 1
  };

extern void dead_debug_init (struct dead_debug *, bitmap);
extern void dead_debug_finish (struct dead_debug *, bitmap);
extern void dead_debug_add (struct dead_debug *, df_ref, unsigned int);
extern int dead_debug_insert_temp (struct dead_debug *,
				   unsigned int uregno, rtx insn,
				   enum debug_temp_where);

extern void propagate_for_debug (rtx, rtx, rtx, rtx, basic_block);


#endif /* GCC_VALTRACK_H */
