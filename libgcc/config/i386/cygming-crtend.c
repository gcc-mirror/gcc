/* crtend object for windows32 targets.
   Copyright (C) 2007-2016 Free Software Foundation, Inc.

   Contributed by Danny Smith <dannysmith@users.sourceforge.net>

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

/* auto-host.h is needed by cygming.h for HAVE_GAS_WEAK and here
   for HAVE_LD_RO_RW_SECTION_MIXING.  */  
#include "auto-host.h"
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "unwind-dw2-fde.h"

#if defined(HAVE_LD_RO_RW_SECTION_MIXING)
# define EH_FRAME_SECTION_CONST const
#else
# define EH_FRAME_SECTION_CONST
#endif

#if DWARF2_UNWIND_INFO
/* Terminate the frame unwind info section with a 0 as a sentinel;
   this would be the 'length' field in a real FDE.  */

static EH_FRAME_SECTION_CONST int __FRAME_END__[]
  __attribute__ ((used,  section(__LIBGCC_EH_FRAME_SECTION_NAME__),
		  aligned(4)))
  = { 0 };
#endif

#if TARGET_USE_JCR_SECTION
/* Null terminate the .jcr section array.  */
static void *__JCR_END__[1] 
   __attribute__ ((used, section(__LIBGCC_JCR_SECTION_NAME__),
		   aligned(sizeof(void *))))
   = { 0 };
#endif

extern void __gcc_register_frame (void); 
extern void __gcc_deregister_frame (void);

static void register_frame_ctor (void) __attribute__ ((constructor (0)));

static void
register_frame_ctor (void)
{
  __gcc_register_frame ();
}

#if !DEFAULT_USE_CXA_ATEXIT
static void deregister_frame_dtor (void) __attribute__ ((destructor (0)));

static void
deregister_frame_dtor (void)
{
  __gcc_deregister_frame ();
}
#endif
