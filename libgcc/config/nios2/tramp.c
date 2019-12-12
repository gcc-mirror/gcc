/* Copyright (C) 2013-2019 Free Software Foundation, Inc.
   Contributed by Altera and Mentor Graphics, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Set up trampolines.
   R12 is the static chain register.
   R2 is AT, the assembler temporary.
   The trampoline code looks like:
	movhi	r12,%hi(chain)
	ori	r12,%lo(chain)
	movhi	r2,%hi(fn)
	ori	r2,%lo(fn)
	jmp	r2
*/

#define SC_REGNO 12

/* Instruction encodings depend on the ISA level.  */
#if __nios2_arch__ == 2
#define MOVHI(reg,imm16)			\
  (((reg) << 11) | ((imm16) << 16) | 0x34)
#define ORI(reg,imm16)						\
  (((reg) << 11) | ((reg) << 6) | ((imm16) << 16) | 0x14)
#define JMP(reg)				\
  (((reg) << 6) | (0x0d << 26) | 0x20)

#elif __nios2_arch__ == 1
#define MOVHI(reg,imm16)			\
  (((reg) << 22) | ((imm16) << 6) | 0x34)
#define ORI(reg,imm16)						\
  (((reg) << 27) | ((reg) << 22) | ((imm16) << 6) | 0x14)
#define JMP(reg)				\
  (((reg) << 27) | (0x0d << 11) | 0x3a)

#else
#error "Unknown Nios II architecture level"
#endif

void
__trampoline_setup (unsigned int *addr, void *fnptr, void *chainptr)
{
  unsigned int fn = (unsigned int) fnptr;
  unsigned int chain = (unsigned int) chainptr;
  int i;

  addr[0] = MOVHI (SC_REGNO, ((chain >> 16) & 0xffff));
  addr[1] = ORI (SC_REGNO, (chain & 0xffff));
  addr[2] = MOVHI (2, ((fn >> 16) & 0xffff));
  addr[3] = ORI (2, (fn & 0xffff));
  addr[4] = JMP (2);

  /* Flush the caches.
     See Example 9-4 in the Nios II Software Developer's Handbook.  */
  for (i = 0; i < 5; i++)
    asm volatile ("flushd 0(%0); flushi %0" :: "r"(addr + i) : "memory");
  asm volatile ("flushp" ::: "memory");
}
