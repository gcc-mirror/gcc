/* Out-of-line LSE atomics for AArch64 architecture, Init.
   Copyright (C) 2019 Free Software Foundation, Inc.
   Contributed by Linaro Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "auto-target.h"

/* Define the symbol gating the LSE implementations.  */
_Bool __aarch64_have_lse_atomics
  __attribute__((visibility("hidden"), nocommon));

/* Disable initialization of __aarch64_have_lse_atomics during bootstrap.  */
#if !defined(inhibit_libc) && defined(HAVE_SYS_AUXV_H)
# include <sys/auxv.h>

/* Disable initialization if the system headers are too old.  */
# if defined(AT_HWCAP) && defined(HWCAP_ATOMICS)

static void __attribute__((constructor))
init_have_lse_atomics (void)
{
  unsigned long hwcap = getauxval (AT_HWCAP);
  __aarch64_have_lse_atomics = (hwcap & HWCAP_ATOMICS) != 0;
}

# endif /* HWCAP */
#endif /* inhibit_libc */
