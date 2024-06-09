/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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

.macro SAVE_USR_REGS
/* Store User Special Registers according to supported ISA extension
   !!! WATCH OUT !!! Take care of 8-byte alignment issue.  */
#if __NDS32_EXT_IFC__ && (__NDS32_EXT_ZOL__ || __NDS32_EXT_DSP__)
  mfusr   $r1, $IFC_LP
  mfusr   $r2, $LB
  mfusr   $r3, $LE
  mfusr   $r4, $LC
  smw.adm $r1, [$sp], $r4, #0x0 /* Save even. Ok!  */
#elif __NDS32_EXT_IFC__
  mfusr   $r1, $IFC_LP
  smw.adm $r1, [$sp], $r2, #0x0	/* Save extra $r2 to keep stack 8-byte aligned.  */
#elif (__NDS32_EXT_ZOL__ || __NDS32_EXT_DSP__)
  mfusr   $r1, $LB
  mfusr   $r2, $LE
  mfusr   $r3, $LC
  smw.adm $r1, [$sp], $r4, #0x0	/* Save extra $r4 to keep stack 8-byte aligned.  */
#endif
.endm
