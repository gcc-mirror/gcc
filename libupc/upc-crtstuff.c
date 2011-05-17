/* upc-crtstuff.c: UPC specific "C Runtime Support"
   Copyright (C) 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Target machine header files require this define. */
#define IN_LIBGCC2

#undef USED_FOR_TARGET

/* FIXME: Including auto-host is incorrect, but until we have
   identified the set of defines that need to go into auto-target.h,
   this will have to do.  */
#include "auto-host.h"
#undef pid_t
#undef rlim_t
#undef ssize_t
#undef vfork
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "config.h"
#include "unwind-dw2-fde.h"
#include "upc-crt-config.h"

/* Only define secton start/end if no link script is used */

#ifdef CRT_BEGIN

/* Shared begin is always defined in order to allocate space
   at the beginnig of the section */
#ifdef UPC_SHARED_SECTION_BEGIN
/* Establish a symbol at the beginning of the data section */
UPC_SHARED_SECTION_BEGIN
#endif /* UPC_SHARED_SECTION_BEGIN */

#ifndef HAVE_UPC_LINK_SCRIPT
#ifdef UPC_PGM_INFO_SECTION_BEGIN
/* Establish a symbol at the beginning of the progam info data section */
UPC_PGM_INFO_SECTION_BEGIN
#endif /* UPC_PGM_INFO_SECTION_BEGIN */
#ifdef UPC_INIT_SECTION_BEGIN
/* Establish a symbol at the beginning of the initialization section */
UPC_INIT_SECTION_BEGIN
#endif /* UPC_INIT_SECTION_BEGIN */
#ifdef UPC_INIT_ARRAY_SECTION_BEGIN
/* Establish a symbol at the beginning of the initialization array section. */
UPC_INIT_ARRAY_SECTION_BEGIN
#endif /* UPC_INIT_ARRAY_SECTION_BEGIN */
#endif /* !HAVE_UPC_LINK_SCRIPT */

#elif defined(CRT_END)		/* ! CRT_BEGIN */

#ifndef HAVE_UPC_LINK_SCRIPT
#ifdef UPC_SHARED_SECTION_END
/* Establish a symbol at the end of the shared data section */
UPC_SHARED_SECTION_END
#endif /* UPC_SHARED_SECTION_END */
#ifdef UPC_PGM_INFO_SECTION_END
/* Establish a symbol at the end of the program info data section */
UPC_PGM_INFO_SECTION_END
#endif /* UPC_PGM_INFO_SECTION_END */
#ifdef UPC_INIT_SECTION_END
/* Establish a symbol at the end of the initialization section */
UPC_INIT_SECTION_END
#endif /* UPC_INIT_SECTION_END */
#ifdef UPC_INIT_ARRAY_SECTION_END
/* Establish a symbol at the end of the initialization array section. */
UPC_INIT_ARRAY_SECTION_END
#endif /* UPC_INIT_ARRAY_SECTION_END */
#endif /* !HAVE_UPC_LINK_SCRIPT */
#else /* ! CRT_BEGIN && ! CRT_END */
#error "One of CRT_BEGIN or CRT_END must be defined."
#endif
