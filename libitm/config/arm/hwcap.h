/* Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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

/* The following must match the kernel's <asm/procinfo.h>.  */
#define HWCAP_ARM_SWP           1
#define HWCAP_ARM_HALF          2
#define HWCAP_ARM_THUMB         4
#define HWCAP_ARM_26BIT         8
#define HWCAP_ARM_FAST_MULT     16
#define HWCAP_ARM_FPA           32
#define HWCAP_ARM_VFP           64
#define HWCAP_ARM_EDSP          128
#define HWCAP_ARM_JAVA          256
#define HWCAP_ARM_IWMMXT        512
#define HWCAP_ARM_CRUNCH        1024
#define HWCAP_ARM_THUMBEE       2048
#define HWCAP_ARM_NEON          4096
#define HWCAP_ARM_VFPv3         8192
#define HWCAP_ARM_VFPv3D16      16384

