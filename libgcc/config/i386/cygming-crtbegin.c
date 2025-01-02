/* crtbegin object for windows32 targets.
   Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#if DWARF2_UNWIND_INFO
/* Make the declarations weak.  This is critical for
   _Jv_RegisterClasses because it lives in libgcj.a  */
extern void __register_frame_info (__attribute__((unused)) const void *,
				   __attribute__((unused)) struct object *)
				   TARGET_ATTRIBUTE_WEAK;
extern void *__deregister_frame_info (__attribute__((unused)) const void *)
				      TARGET_ATTRIBUTE_WEAK;

/* Work around for current cygwin32 build problems (Bug gas/16858).
   Compile weak default functions only for 64-bit systems,
   when absolutely necessary.  */
#ifdef __x86_64__
TARGET_ATTRIBUTE_WEAK void
__register_frame_info (__attribute__((unused)) const void *p,
		       __attribute__((unused)) struct object *o)
{
}

TARGET_ATTRIBUTE_WEAK void *
__deregister_frame_info (__attribute__((unused)) const void *p)
{
  return (void*) 0;
}
#endif
#endif /* DWARF2_UNWIND_INFO */

#if defined(HAVE_LD_RO_RW_SECTION_MIXING)
# define EH_FRAME_SECTION_CONST const
#else
# define EH_FRAME_SECTION_CONST
#endif

/* Stick a label at the beginning of the frame unwind info so we can
   register/deregister it with the exception handling library code.  */
#if DWARF2_UNWIND_INFO
static EH_FRAME_SECTION_CONST char __EH_FRAME_BEGIN__[]
  __attribute__((used, section(__LIBGCC_EH_FRAME_SECTION_NAME__), aligned(4)))
  = { };

static struct object obj;

/* Handle of libgcc's DLL reference.  */
HANDLE hmod_libgcc;
static void *  (*deregister_frame_fn) (const void *) = NULL;
#endif

#ifdef __CYGWIN__
/* Declare the __dso_handle variable.  It should have a unique value
   in every shared-object; in a main program its value is zero.  The
   object should in any case be protected.  This means the instance
   in one DSO or the main program is not used in another object.  The
   dynamic linker takes care of this.  */

#ifdef CRTSTUFFS_O
extern void *__ImageBase;
void *__dso_handle = &__ImageBase;
#else
void *__dso_handle = 0;
#endif

#endif /* __CYGWIN__ */


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
    {
      /* Increasing the load-count of LIBGCC_SONAME DLL.  */
      hmod_libgcc = LoadLibrary (LIBGCC_SONAME);
      register_frame_fn = (void (*) (const void *, struct object *))
			  GetProcAddress (h, "__register_frame_info");
      deregister_frame_fn = (void* (*) (const void *))
	                    GetProcAddress (h, "__deregister_frame_info");
    }
  else
    {
      register_frame_fn = __register_frame_info;
      deregister_frame_fn = __deregister_frame_info;
    }
  if (register_frame_fn)
     register_frame_fn (__EH_FRAME_BEGIN__, &obj);
#endif

#if DEFAULT_USE_CXA_ATEXIT
  /* If we use the __cxa_atexit method to register C++ dtors
     at object construction,  also use atexit to register eh frame
     info cleanup.  */
  atexit(__gcc_deregister_frame);
#endif /* DEFAULT_USE_CXA_ATEXIT */
}

void
__gcc_deregister_frame (void)
{
#if DWARF2_UNWIND_INFO
  if (deregister_frame_fn)
     deregister_frame_fn (__EH_FRAME_BEGIN__);
  if (hmod_libgcc)
    FreeLibrary (hmod_libgcc);
#endif
}
