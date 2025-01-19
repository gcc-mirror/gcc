/* Initializer for SME support.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#include "auto-target.h"

#ifndef inhibit_libc
/* For libc feature test macros.  */
# include <unistd.h>
#endif

#if __ARM_FEATURE_SME
/* Avoid runtime SME detection if libgcc is built with SME.  */
# define HAVE_SME_CONST const
# define HAVE_SME_VALUE 1
#elif HAVE___GETAUXVAL
/* SME access detection on Linux.  */
# define HAVE_SME_CONST
# define HAVE_SME_VALUE 0
# define HAVE_SME_CTOR sme_accessible ()

# define AT_HWCAP2	26
# define HWCAP2_SME	(1 << 23)
unsigned long int __getauxval (unsigned long int);

static _Bool
sme_accessible (void)
{
  unsigned long hwcap2 = __getauxval (AT_HWCAP2);
  return (hwcap2 & HWCAP2_SME) != 0;
}
#elif __LIBC___AARCH64_SME_ACCESSIBLE
/* Alternative SME access detection.  */
# define HAVE_SME_CONST
# define HAVE_SME_VALUE 0
# define HAVE_SME_CTOR __aarch64_sme_accessible ()
_Bool __aarch64_sme_accessible (void);
#else
# define HAVE_SME_CONST const
# define HAVE_SME_VALUE 0
#endif

/* Define the symbol gating SME support in libgcc.  */
HAVE_SME_CONST _Bool __aarch64_have_sme
  __attribute__((visibility("hidden"), nocommon)) = HAVE_SME_VALUE;

#ifdef HAVE_SME_CTOR
/* Use a higher priority to ensure it runs before user constructors
   with priority 100. */
static void __attribute__((constructor (90)))
init_have_sme (void)
{
  __aarch64_have_sme = HAVE_SME_CTOR;
}
#endif
