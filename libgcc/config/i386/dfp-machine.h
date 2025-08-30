/* Rounding mode macros for DFP libbid.  x86 version.
   Copyright (C) 2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

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

#ifndef _X86_DFP_MACHINE_H
#define _X86_DFP_MACHINE_H

#ifdef _SOFT_FLOAT
#include_next <dfp-machine.h>
#else
#include "config/i386/sfp-machine.h"

#ifdef __x86_64__
#include "config/i386/64/dfp-machine.h"
#else
#include "config/i386/32/dfp-machine.h"
#endif

/* Initialize the rounding mode to round-to-nearest if needed.  */
#define DFP_INIT_ROUNDMODE				\
  DFP_GET_ROUNDMODE;					\
  do							\
    {							\
      if (_frnd_orig != FP_RND_NEAREST)			\
	DFP_SET_ROUNDMODE (FP_RND_NEAREST);		\
    }							\
  while (0);

/* Restore the rounding mode to round-to-nearest if changed.  */
#define DFP_RESTORE_ROUNDMODE				\
  do							\
    {							\
      if (_frnd_orig != FP_RND_NEAREST)			\
	DFP_SET_ROUNDMODE (_frnd_orig);			\
    }							\
  while (0);
#endif

#endif /* _X86_DFP_MACHINE_H */
