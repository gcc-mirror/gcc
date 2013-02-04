/* DWARF2 EH unwinding support for 32-bit PowerPC Darwin.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

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

#ifndef __LP64__

extern bool _Unwind_fallback_frame_state_for
  (struct _Unwind_Context *context, _Unwind_FrameState *fs);

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS)	\
  (_Unwind_fallback_frame_state_for (CONTEXT, FS)	\
   ? _URC_NO_REASON : _URC_END_OF_STACK)

#endif
