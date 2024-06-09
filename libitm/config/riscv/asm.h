/* Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

#ifndef _RV_ASM_H
#define _RV_ASM_H

#ifdef __riscv_e
#  error "rv32e and rv64e unsupported"
#endif

#if __riscv_xlen == 64
#  define GPR_L ld
#  define GPR_S sd
#  define SZ_GPR 8
#  define LEN_GPR 14
#elif __riscv_xlen == 32
#  define GPR_L lw
#  define GPR_S sw
#  define SZ_GPR 4
#  define LEN_GPR 16 /* Extra padding to align the stack to 16 bytes */
#else
#  error Unsupported XLEN (must be 64-bit or 32-bit).
#endif

#if defined(__riscv_flen) && __riscv_flen == 64
#  define FPR_L fld
#  define FPR_S fsd
#  define SZ_FPR 8
#elif defined(__riscv_flen) && __riscv_flen == 32
#  define FPR_L flw
#  define FPR_S fsw
#  define SZ_FPR 4
#elif defined(__riscv_flen)
#  error Q-extension unsupported
#else
#  define SZ_FPR 0
#endif

/* The size of gtm_jmpbuf */
#define ADJ_STACK_SIZE (LEN_GPR*SZ_GPR+12*SZ_FPR)

#endif  /* _RV_ASM_H */
