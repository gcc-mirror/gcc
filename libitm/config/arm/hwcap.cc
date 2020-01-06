/* Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

/* This file initializes GTM_hwcap in some os-specific way to indicate
   what ISA extensions are present for ARM.  */

#include "libitm_i.h"
#include "hwcap.h"

/* Begin by defaulting to whatever options were given to the compiler.  */
int GTM_hwcap HIDDEN = 0
#ifdef __VFP_FP__
  | HWCAP_ARM_VFP
#endif
#ifdef __IWMMXT__
  | HWCAP_ARM_IWMMXT
#endif
  ;

#ifdef __linux__
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>

static void __attribute__((constructor))
init_gtm_hwcap(void)
{
  int fd = open ("/proc/self/auxv", O_RDONLY);
  if (fd < 0)
    return;

  Elf32_auxv_t pairs[512];
  ssize_t rlen = read (fd, pairs, sizeof(pairs));
  close (fd);
  if (rlen < 0)
    return;

  size_t n = (size_t)rlen / sizeof(pairs[0]);
  for (size_t i = 0; i < n; ++i)
    if (pairs[i].a_type == AT_HWCAP)
      {
	GTM_hwcap = pairs[i].a_un.a_val;
	return;
      }
}
#endif
