/* Store register values as _Unwind_Word type in DWARF2 EH unwind context.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

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

/* Return the value of the VLENB register.  This should only be
   called if we know this is an vector extension enabled RISC-V host.  */
static inline long
riscv_vlenb (void)
{
  register long vlenb asm ("a0");
  /* 0xc2202573 == csrr a0, 0xc22 */
  asm (".insn 0xc2202573" : "=r"(vlenb));
  return vlenb;
}

/* Lazily provide a value for VLENB, so that we don't try to execute RVV
   instructions unless we know they're needed.  */
#define DWARF_LAZY_REGISTER_VALUE(REGNO, VALUE) \
  ((REGNO) == RISCV_DWARF_VLENB && ((*VALUE) = riscv_vlenb (), 1))
