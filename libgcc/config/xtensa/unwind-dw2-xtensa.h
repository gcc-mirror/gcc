/* DWARF2 frame unwind data structure for Xtensa.
   Copyright (C) 1997-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Xtensa's variable-size register window save areas can be unwound without
   any unwind info.  This is a stripped down version of the standard DWARF
   _Unwind_FrameState.  */
typedef struct
{
  /* The information we care about from the CIE/FDE.  */
  _Unwind_Personality_Fn personality;
  _Unwind_Word retaddr_column;
  unsigned char fde_encoding;
  unsigned char lsda_encoding;
  unsigned char saw_z;
  unsigned char signal_frame;
  void *eh_ptr;

  /* Saved registers for a signal frame.  */
  _Unwind_Word *signal_regs;
  _Unwind_Word signal_ra;
} _Unwind_FrameState;

