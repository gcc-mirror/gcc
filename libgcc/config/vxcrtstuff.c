/* This file is part of GCC.

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

/* The essential point of the crtbegin/crtend files on VxWorks is to handle
   the eh frames registration thanks to dedicated constructors and
   destructors.  What needs to be done depends on the VxWorks version and the
   kind of module (rtp, dkm, ...) one is building.  */

#define IN_LIBGCC2

/* FIXME: Including auto-host is incorrect here (target library implementation
   file), but we still need it for DEFAULT_USE_CXA_ATEXIT and most importantly
   USE_INITFINI_ARRAY, guarded by HAVE_INITFINI_ARRAY_SUPPORT, not yet handled
   by auto-target.h.  Proceed as crtstuff.c, with the inclusion followed by a
   few #undefs preventing build failures in configurations setup for Windows
   hosts, including canadian builds.  #define USED_FOR_TARGET would circumvent
   this but would unfortunately also inhibit some of the definitions we
   need.  */
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
#include "unwind-dw2-fde.h"

/* If we are entitled/requested to use init/fini arrays, we'll rely on that.
   Otherwise, we may rely on ctors/dtors sections for RTPs or expect munch to
   be involved for kernel modules.  */

#if !defined(USE_INITFINI_ARRAY) && defined(__RTP__)
#define USE_CDTORS_SECTIONS
#endif

#if DWARF2_UNWIND_INFO && !defined(__USING_SJLJ_EXCEPTIONS__)
#define USE_EH_FRAME_REGISTRY
#endif

/*  ------------------------------ crtbegin -------------------------------  */

#ifdef CRT_BEGIN

/* Provide __dso_handle in RTP objects, which might be included in contexts
   involving shared objects.  This mimics the crtstuff.c behavior:  dso_handle
   should be NULL for the main program (in vx_crtbegin.o) and a unique value
   for the shared libraries (in vx_crtbeginS.o).  */

#if DEFAULT_USE_CXA_ATEXIT && defined(__RTP__)
extern void *__dso_handle __attribute__ ((__visibility__ ("hidden")));
#ifdef CRTSTUFFS_O
void *__dso_handle = &__dso_handle;
#else
void *__dso_handle = 0;
#endif
#endif

/* Determine what names to use for the constructor/destructor functions.  */

#if defined(USE_CDTORS_SECTIONS) || defined(USE_INITFINI_ARRAY)

#define EH_CTOR_NAME _crtbe_register_frame
#define EH_DTOR_NAME _ctrbe_deregister_frame
#define EH_LINKAGE static

#else

/* No specific sections for constructors or destructors: we thus use a
   symbol naming convention so that the constructors are then recognized
   by munch or whatever tool is used for the final link phase.  Since the
   pointers to the constructor/destructor functions are not created in this
   translation unit, they must have external linkage.  */
#define EH_CTOR_NAME _GLOBAL__I_00101_0__crtbe_register_frame
#define EH_DTOR_NAME _GLOBAL__D_00101_1__crtbe_deregister_frame
#define EH_LINKAGE

#endif

#ifdef USE_INITFINI_ARRAY
/* .init_array and .fini_array is supported starting VxWorks 7.2 in all
   cases. The compiler is then configured to always support priorities in
   constructors, so we can rely on the constructor and destructor attributes
   to generate the proper sections.  */
#define EH_CTOR_ATTRIBUTE __attribute__((constructor (101)))
#define EH_DTOR_ATTRIBUTE __attribute__((destructor (101)))

/* Provide the init/fini array support functions for shared libraries,
   where we don't want to drag libc_internal contents blindly and which
   provides functions with a slightly different name anyway.  */

#if defined(CRTSTUFFS_O)

/* Run through the .init_array, .fini_array sections.  The linker script
   *must* provide __init_array_start, __init_array_end, __fini_array_start,
   __fini_array_end symbols.  */

typedef void (*initfini_ptr) (void);
extern initfini_ptr __init_array_start[];
extern initfini_ptr __init_array_end[];
extern initfini_ptr __fini_array_start[];
extern initfini_ptr __fini_array_end[];

/* Provide the actual code through static functions, which don't need
   to be exposed in the shared lib interface.  */

static void __exec_init_array(void)
{
  initfini_ptr *fn;
  for (fn = __init_array_start; fn < __init_array_end; ++fn)
    (*fn)();
}

static void __exec_fini_array(void)
{
  initfini_ptr *fn;
  for (fn = __fini_array_end - 1; fn >= __fini_array_start; --fn)
    (*fn)();
}

/* Reference the two above functions as the init / fini function.  */

void __attribute__ ((__section__  (".init"))) _init()
{
  __exec_init_array();
}

void __attribute__ ((__section__  (".fini"))) _fini()
{
  __exec_fini_array();
}

#endif /* __CRTSTUFFS_O__ */

#else /* !USE_INITFINI_ARRAY  */

/* Note: Even in case of .ctors/.dtors sections, we can't use the attribute
   (constructor (15)) here as gcc may have been configured with constructors
   priority disabled.  We will instead craft an explicit section name for this
   purpose.  */
#define EH_CTOR_ATTRIBUTE
#define EH_DTOR_ATTRIBUTE

#endif /* USE_INITFINI_ARRAY  */

#ifdef USE_EH_FRAME_REGISTRY
/* Stick a label at the beginning of the frame unwind info so we can register
   and deregister it with the exception handling library code.  */
static const char __EH_FRAME_BEGIN__[]
__attribute__((section(__LIBGCC_EH_FRAME_SECTION_NAME__), aligned(4)))
  = { };

EH_LINKAGE EH_CTOR_ATTRIBUTE void EH_CTOR_NAME (void)
{
  static struct object object;
  __register_frame_info (__EH_FRAME_BEGIN__, &object);
}

EH_LINKAGE EH_DTOR_ATTRIBUTE void EH_DTOR_NAME (void)
{
  __deregister_frame_info (__EH_FRAME_BEGIN__);
}
#endif /* USE_EH_FRAME_REGISTRY */

#ifdef USE_CDTORS_SECTIONS
/* As explained above, we need to manually build the sections here as the
   compiler may not have support for constructors priority enabled.  */
static void (* volatile eh_registration_ctors[])()
  __attribute__((section (".ctors.101")))
= { &EH_CTOR_NAME };
static void (* volatile eh_registration_dtors[])()
  __attribute__((section (".dtors.65434")))
= { &EH_DTOR_NAME };
#endif

/*  ------------------------------ crtend ---------------------------------  */

#elif defined (CRT_END) /* ! CRT_BEGIN */

#ifdef USE_EH_FRAME_REGISTRY
/* Terminate the frame unwind info section with a 4byte 0 as a sentinel;
   this would be the 'length' field in a real FDE.  */

static const char __FRAME_END__[]
     __attribute__ ((used, section(__LIBGCC_EH_FRAME_SECTION_NAME__),
		     aligned(4)))
  = { 0, 0, 0, 0 };
#endif /* USE_EH_FRAME_REGISTRY */

#else /* ! CRT_BEGIN & ! CRT_END */

#error "One of CRT_BEGIN or CRT_END must be defined."

#endif
