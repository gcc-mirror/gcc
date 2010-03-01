/* Implement UPC run-time begin/end data sections
   Copyright (C) 2008 Free Software Foundation, Inc.
   Gary Funck <gary@intrepid.com> and Nenad Vukicevic <nenad@intrepid.com>.
 
This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Target machine header files require this define. */
#define IN_LIBGCC2

#undef USED_FOR_TARGET

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "unwind-dw2-fde.h"

/* Only define secton start/end if no link script is used */

#ifdef CRT_BEGIN

/* Shared begin is always defined in order to allocate space
   at the beginnig of the section */
#ifdef UPC_SHARED_SECTION_BEGIN_INIT
/* Establish a symbol at the beginning of the data section */
UPC_SHARED_SECTION_BEGIN_INIT
#endif /* UPC_SHARED_SECTION_BEGIN_INIT */

#ifndef HAVE_UPC_LINK_SCRIPT
#ifdef UPC_PGM_INFO_SECTION_BEGIN_INIT
/* Establish a symbol at the beginning of the progam info data section */
UPC_PGM_INFO_SECTION_BEGIN_INIT
#endif /* UPC_PGM_INFO_SECTION_BEGIN_INIT */
#ifdef UPC_INIT_SECTION_BEGIN_INIT
/* Establish a symbol at the beginning of the initialization section */
UPC_INIT_SECTION_BEGIN_INIT
#endif /* UPC_INIT_SECTION_BEGIN_INIT */
#ifdef UPC_INIT_ARRAY_SECTION_BEGIN_INIT
/* Establish a symbol at the beginning of the initialization array section. */
UPC_INIT_ARRAY_SECTION_BEGIN_INIT
#endif /* UPC_INIT_ARRAY_SECTION_BEGIN_INIT */
#endif /* !HAVE_UPC_LINK_SCRIPT */

#elif defined(CRT_END)		/* ! CRT_BEGIN */

#ifndef HAVE_UPC_LINK_SCRIPT
#ifdef UPC_SHARED_SECTION_END_INIT
/* Establish a symbol at the end of the shared data section */
UPC_SHARED_SECTION_END_INIT
#endif /* UPC_SHARED_SECTION_END_INIT */
#ifdef UPC_PGM_INFO_SECTION_END_INIT
/* Establish a symbol at the end of the program info data section */
UPC_PGM_INFO_SECTION_END_INIT
#endif /* UPC_PGM_INFO_SECTION_END_INIT */
#ifdef UPC_INIT_SECTION_END_INIT
/* Establish a symbol at the end of the initialization section */
UPC_INIT_SECTION_END_INIT
#endif /* UPC_INIT_SECTION_END_INIT */
#ifdef UPC_INIT_ARRAY_SECTION_END_INIT
/* Establish a symbol at the end of the initialization array section. */
UPC_INIT_ARRAY_SECTION_END_INIT
#endif /* UPC_INIT_ARRAY_SECTION_END_INIT */
#endif /* !HAVE_UPC_LINK_SCRIPT */
#else /* ! CRT_BEGIN && ! CRT_END */
#error "One of CRT_BEGIN or CRT_END must be defined."
#endif
