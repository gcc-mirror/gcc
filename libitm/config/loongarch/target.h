/* Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Loongson Co. Ltd.

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
    long int fp;        /* Frame Pointer: r22 */
    long int pc;        /* Return Address: r1 */
    void *cfa;          /* CFA: r3 */
    long int gpr[9];	/* Callee-saved scratch GPRs: r23(s0)-r31(s8) */

    /* Callee-saved scratch FPRs: f24-f31 */
#if defined(__loongarch_double_float)
    double fpr[8];
#elif defined(__loongarch_single_float)
    float fpr[8];
#endif
  } gtm_jmpbuf;

#define HW_CACHELINE_SIZE 128

static inline void
cpu_relax (void)
{
    __asm__ volatile ("" : : : "memory");
}

} // namespace GTM
