/* Store register values as _Unwind_Word type in DWARF2 EH unwind context.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Define this macro if the target stores register values as _Unwind_Word
   type in unwind context.  Only enable it for ilp32.  */
#if defined __aarch64__ && !defined __LP64__
# define REG_VALUE_IN_UNWIND_CONTEXT
#endif

/* Return the value of the pseudo VG register.  This should only be
   called if we know this is an SVE host.  */
static inline int
aarch64_vg (void)
{
  register int vg asm ("x0");
  /* CNTD X0.  */
  asm (".inst 0x04e0e3e0" : "=r" (vg));
  return vg;
}

/* Lazily provide a value for VG, so that we don't try to execute SVE
   instructions unless we know they're needed.  */
#define DWARF_LAZY_REGISTER_VALUE(REGNO, VALUE) \
  ((REGNO) == AARCH64_DWARF_VG && ((*VALUE) = aarch64_vg (), 1))
