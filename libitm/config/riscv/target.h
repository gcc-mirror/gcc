/* Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Xiongchuan Tan <xc-tan@outlook.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

namespace GTM HIDDEN {

typedef struct gtm_jmpbuf
  {
    long int pc;
    void *cfa;
    long int s[12]; /* Saved registers, s0 is fp */

#if __riscv_xlen == 32
    /* Ensure that the stack is 16-byte aligned */
    long int padding[2];
#endif

    /* FP saved registers */
#if defined(__riscv_flen) && __riscv_flen == 64
    double fs[12];
#elif defined(__riscv_flen) && __riscv_flen == 32
    float fs[12];
#elif defined(__riscv_flen)
#  error Q-extension unsupported
#endif
  } gtm_jmpbuf;

/* The size of one line in hardware caches (in bytes). */
/* 64 bytes is a suggested value in the RVA profiles (see
   https://github.com/riscv/riscv-profiles/blob/main/profiles.adoc). */
#define HW_CACHELINE_SIZE 64

static inline void
cpu_relax (void)
{
  #ifdef __riscv_zihintpause
      __asm volatile ("pause");
  #else
      /* Encoding of the pause instruction */
      __asm volatile (".4byte 0x100000F");
  #endif
}

} // namespace GTM
