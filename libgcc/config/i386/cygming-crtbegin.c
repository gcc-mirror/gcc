/* crtbegin object for windows32 targets.
   Copyright (C) 2007, 2009, 2010, 2011  Free Software Foundation, Inc.

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

#include "auto-host.h"
#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "unwind-dw2-fde.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#ifndef LIBGCC_SONAME
#define LIBGCC_SONAME "libgcc_s.dll"
#endif

#ifndef LIBGCJ_SONAME
#define LIBGCJ_SONAME "libgcj_s.dll"
#endif


/* Make the declarations weak.  This is critical for
   _Jv_RegisterClasses because it lives in libgcj.a  */
extern void __register_frame_info (const void *, struct object *)
				   TARGET_ATTRIBUTE_WEAK;
extern void *__deregister_frame_info (const void *)
				      TARGET_ATTRIBUTE_WEAK;
extern void _Jv_RegisterClasses (const void *) TARGET_ATTRIBUTE_WEAK;

#if defined(HAVE_LD_RO_RW_SECTION_MIXING)
# define EH_FRAME_SECTION_CONST const
#else
# define EH_FRAME_SECTION_CONST
#endif

/* Stick a label at the beginning of the frame unwind info so we can
   register/deregister it with the exception handling library code.  */
#if DWARF2_UNWIND_INFO
static EH_FRAME_SECTION_CONST char __EH_FRAME_BEGIN__[]
  __attribute__((used, section(EH_FRAME_SECTION_NAME), aligned(4)))
  = { };

static struct object obj;
#endif

#if TARGET_USE_JCR_SECTION
static void *__JCR_LIST__[]
  __attribute__ ((used, section(JCR_SECTION_NAME), aligned(4)))
  = { };
#endif

/* Pull in references from libgcc.a(unwind-dw2-fde.o) in the
   startfile. These are referenced by a ctor and dtor in crtend.o.  */
extern void __gcc_register_frame (void);
extern void __gcc_deregister_frame (void);

void
__gcc_register_frame (void)
{
#if DWARF2_UNWIND_INFO
/* Weak undefined symbols won't be pulled in from dlls; hence
   we first test if the dll is already loaded and, if so,
   get the symbol's address at run-time.  If the dll is not loaded,
   fallback to weak linkage to static archive.  */

  void (*register_frame_fn) (const void *, struct object *);
  HANDLE h = GetModuleHandle (LIBGCC_SONAME);
  if (h)
    register_frame_fn = (void (*) (const void *, struct object *))
			GetProcAddress (h, "__register_frame_info");
  else 
    register_frame_fn = __register_frame_info;
  if (register_frame_fn)
     register_frame_fn (__EH_FRAME_BEGIN__, &obj);
#endif

#if TARGET_USE_JCR_SECTION 
  if (__JCR_LIST__[0])
    {
      void (*register_class_fn) (const void *);
      HANDLE h = GetModuleHandle (LIBGCJ_SONAME);
      if (h)
	register_class_fn = (void (*) (const void *))
			     GetProcAddress (h, "_Jv_RegisterClasses");
      else
	register_class_fn = _Jv_RegisterClasses;

      if (register_class_fn)
	register_class_fn (__JCR_LIST__);
    }
#endif
}

void
__gcc_deregister_frame (void)
{
#if DWARF2_UNWIND_INFO
  void *  (*deregister_frame_fn) (const void *);
  HANDLE h = GetModuleHandle (LIBGCC_SONAME);
  if (h)
    deregister_frame_fn = (void* (*) (const void *))
			  GetProcAddress (h, "__deregister_frame_info");
  else 
    deregister_frame_fn = __deregister_frame_info;
  if (deregister_frame_fn)
     deregister_frame_fn (__EH_FRAME_BEGIN__);
#endif
}
