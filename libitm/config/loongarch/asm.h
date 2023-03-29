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

#ifndef _LA_ASM_H
#define _LA_ASM_H

#if defined(__loongarch_lp64)
#  define GPR_L ld.d
#  define GPR_S st.d
#  define SZ_GPR 8
#  define ADDSP(si)   addi.d  $sp, $sp, si
#elif defined(__loongarch64_ilp32)
#  define GPR_L ld.w
#  define GPR_S st.w
#  define SZ_GPR 4
#  define ADDSP(si)   addi.w  $sp, $sp, si
#else
#  error Unsupported GPR size (must be 64-bit or 32-bit).
#endif

#if defined(__loongarch_double_float)
#  define FPR_L fld.d
#  define FPR_S fst.d
#  define SZ_FPR 8
#elif defined(__loongarch_single_float)
#  define FPR_L fld.s
#  define FPR_S fst.s
#  define SZ_FPR 4
#else
#  define SZ_FPR 0
#endif

#endif  /* _LA_ASM_H */
