/* Specialized bits of code needed for the offloading tables.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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

/* Target machine header files require this define. */
#define IN_LIBGCC2

/* FIXME: Including auto-host is incorrect, but until we have
   identified the set of defines that need to go into auto-target.h,
   this will have to do.  */
#include "auto-host.h"
#undef caddr_t
#undef pid_t
#undef rlim_t
#undef ssize_t
#undef vfork
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

#define OFFLOAD_FUNC_TABLE_SECTION_NAME ".gnu.offload_funcs"
#define OFFLOAD_VAR_TABLE_SECTION_NAME ".gnu.offload_vars"

#ifdef CRT_BEGIN

#if defined(HAVE_GAS_HIDDEN) && defined(ENABLE_OFFLOADING)
const void *const __offload_func_table[0]
  __attribute__ ((__used__, visibility ("hidden"),
		  section (OFFLOAD_FUNC_TABLE_SECTION_NAME))) = { };
const void *const __offload_var_table[0]
  __attribute__ ((__used__, visibility ("hidden"),
		  section (OFFLOAD_VAR_TABLE_SECTION_NAME))) = { };
#endif

#elif defined CRT_END

#if defined(HAVE_GAS_HIDDEN) && defined(ENABLE_OFFLOADING)
const void *const __offload_funcs_end[0]
  __attribute__ ((__used__, visibility ("hidden"),
		  section (OFFLOAD_FUNC_TABLE_SECTION_NAME))) = { };
const void *const __offload_vars_end[0]
  __attribute__ ((__used__, visibility ("hidden"),
		  section (OFFLOAD_VAR_TABLE_SECTION_NAME))) = { };

extern const void *const __offload_func_table[];
extern const void *const __offload_var_table[];

const void *const __OFFLOAD_TABLE__[]
  __attribute__ ((__visibility__ ("hidden"))) =
{
  &__offload_func_table, &__offload_funcs_end,
  &__offload_var_table, &__offload_vars_end
};
#endif

#else /* ! CRT_BEGIN && ! CRT_END */
#error "One of CRT_BEGIN or CRT_END must be defined."
#endif
