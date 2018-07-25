/* Specialized bits of code needed to support construction and
   destruction of file-scope objects in C++ code.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com).

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

/* This file is a bit like libgcc2.c in that it is compiled
   multiple times and yields multiple .o files.

   This file is useful on target machines where the object file format
   supports multiple "user-defined" sections (e.g. COFF, ELF, ROSE).  On
   such systems, this file allows us to avoid running collect (or any
   other such slow and painful kludge).  Additionally, if the target
   system supports a .init section, this file allows us to support the
   linking of C++ code with a non-C++ main program.

   Note that if INIT_SECTION_ASM_OP is defined in the tm.h file, then
   this file *will* make use of the .init section.  If that symbol is
   not defined however, then the .init section will not be used.

   Currently, only ELF and COFF are supported.  It is likely however that
   ROSE could also be supported, if someone was willing to do the work to
   make whatever (small?) adaptations are needed.  (Some work may be
   needed on the ROSE assembler and linker also.)

   This file must be compiled with gcc.  */

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
#include "unwind-dw2-fde.h"

#ifndef FORCE_CODE_SECTION_ALIGN
# define FORCE_CODE_SECTION_ALIGN
#endif

#ifndef CRT_CALL_STATIC_FUNCTION
# define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
static void __attribute__((__used__))			\
call_ ## FUNC (void)					\
{							\
  asm (SECTION_OP);					\
  FUNC ();						\
  FORCE_CODE_SECTION_ALIGN				\
  asm (__LIBGCC_TEXT_SECTION_ASM_OP__);				\
}
#endif

#if defined(TARGET_DL_ITERATE_PHDR) && \
   (defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NetBSD__))
#define BSD_DL_ITERATE_PHDR_AVAILABLE
#endif
 
#if defined(OBJECT_FORMAT_ELF) \
    && !defined(OBJECT_FORMAT_FLAT) \
    && defined(HAVE_LD_EH_FRAME_HDR) \
    && !defined(inhibit_libc) && !defined(CRTSTUFFT_O) \
    && defined(BSD_DL_ITERATE_PHDR_AVAILABLE)
#include <link.h>
# define USE_PT_GNU_EH_FRAME
#endif

#if defined(OBJECT_FORMAT_ELF) \
    && !defined(OBJECT_FORMAT_FLAT) \
    && defined(HAVE_LD_EH_FRAME_HDR) && defined(TARGET_DL_ITERATE_PHDR) \
    && !defined(inhibit_libc) && !defined(CRTSTUFFT_O) \
    && defined(__sun__) && defined(__svr4__)
#include <link.h>
# define USE_PT_GNU_EH_FRAME
#endif

#if defined(OBJECT_FORMAT_ELF) \
    && !defined(OBJECT_FORMAT_FLAT) \
    && defined(HAVE_LD_EH_FRAME_HDR) \
    && !defined(inhibit_libc) && !defined(CRTSTUFFT_O) \
    && defined(__GLIBC__) && __GLIBC__ >= 2
#include <link.h>
/* uClibc pretends to be glibc 2.2 and DT_CONFIG is defined in its link.h.
   But it doesn't use PT_GNU_EH_FRAME ELF segment currently.  */
# if !defined(__UCLIBC__) \
     && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2) \
     || (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2 && defined(DT_CONFIG)))
#  define USE_PT_GNU_EH_FRAME
# endif
#endif

#if defined(OBJECT_FORMAT_ELF) \
    && !defined(OBJECT_FORMAT_FLAT) \
    && defined(HAVE_LD_EH_FRAME_HDR) \
    && !defined(CRTSTUFFT_O) \
    && defined(inhibit_libc) \
    && (defined(__GLIBC__) || defined(__gnu_linux__) || defined(__GNU__))
/* On systems using glibc, an inhibit_libc build of libgcc is only
   part of a bootstrap process.  Build the same crt*.o as would be
   built with headers present, so that it is not necessary to build
   glibc more than once for the bootstrap to converge.  */
# define USE_PT_GNU_EH_FRAME
#endif

#ifdef USE_EH_FRAME_REGISTRY_ALWAYS
# ifndef __LIBGCC_EH_FRAME_SECTION_NAME__
#  error "Can't use explicit exception-frame-registration without __LIBGCC_EH_FRAME_SECTION_NAME__"
# endif
#endif
#if defined(__LIBGCC_EH_FRAME_SECTION_NAME__) && (!defined(USE_PT_GNU_EH_FRAME) || defined(USE_EH_FRAME_REGISTRY_ALWAYS))
# define USE_EH_FRAME_REGISTRY
#endif
#if defined(__LIBGCC_EH_FRAME_SECTION_NAME__) \
    && __LIBGCC_EH_TABLES_CAN_BE_READ_ONLY__
# define EH_FRAME_SECTION_CONST const
#else
# define EH_FRAME_SECTION_CONST
#endif

#if !defined(DTOR_LIST_END) && defined(OBJECT_FORMAT_ELF) \
    && defined(HAVE_GAS_HIDDEN) && !defined(FINI_ARRAY_SECTION_ASM_OP)
# define HIDDEN_DTOR_LIST_END
#endif

#if !defined(USE_TM_CLONE_REGISTRY) && defined(OBJECT_FORMAT_ELF)
# define USE_TM_CLONE_REGISTRY 1
#endif

/* We do not want to add the weak attribute to the declarations of these
   routines in unwind-dw2-fde.h because that will cause the definition of
   these symbols to be weak as well.

   This exposes a core issue, how to handle creating weak references vs
   how to create weak definitions.  Either we have to have the definition
   of TARGET_WEAK_ATTRIBUTE be conditional in the shared header files or
   have a second declaration if we want a function's references to be weak,
   but not its definition.

   Making TARGET_WEAK_ATTRIBUTE conditional seems like a good solution until
   one thinks about scaling to larger problems -- i.e., the condition under
   which TARGET_WEAK_ATTRIBUTE is active will eventually get far too
   complicated.

   So, we take an approach similar to #pragma weak -- we have a second
   declaration for functions that we want to have weak references.

   Neither way is particularly good.  */

/* References to __register_frame_info and __deregister_frame_info should
   be weak in this file if at all possible.  */
extern void __register_frame_info (const void *, struct object *)
				  TARGET_ATTRIBUTE_WEAK;
extern void __register_frame_info_bases (const void *, struct object *,
					 void *, void *)
				  TARGET_ATTRIBUTE_WEAK;
extern void *__deregister_frame_info (const void *)
				     TARGET_ATTRIBUTE_WEAK;
extern void *__deregister_frame_info_bases (const void *)
				     TARGET_ATTRIBUTE_WEAK;
extern void __do_global_ctors_1 (void);

/* Likewise for transactional memory clone tables.  */
extern void _ITM_registerTMCloneTable (void *, size_t) TARGET_ATTRIBUTE_WEAK;
extern void _ITM_deregisterTMCloneTable (void *) TARGET_ATTRIBUTE_WEAK;

#ifdef OBJECT_FORMAT_ELF

/*  Declare a pointer to void function type.  */
typedef void (*func_ptr) (void);
#define STATIC static

#else  /* OBJECT_FORMAT_ELF */

#include "gbl-ctors.h"

#define STATIC

#endif /* OBJECT_FORMAT_ELF */

#ifdef CRT_BEGIN

/* NOTE:  In order to be able to support SVR4 shared libraries, we arrange
   to have one set of symbols { __CTOR_LIST__, __DTOR_LIST__, __CTOR_END__,
   __DTOR_END__ } per root executable and also one set of these symbols
   per shared library.  So in any given whole process image, we may have
   multiple definitions of each of these symbols.  In order to prevent
   these definitions from conflicting with one another, and in order to
   ensure that the proper lists are used for the initialization/finalization
   of each individual shared library (respectively), we give these symbols
   only internal (i.e. `static') linkage, and we also make it a point to
   refer to only the __CTOR_END__ symbol in crtend.o and the __DTOR_LIST__
   symbol in crtbegin.o, where they are defined.  */

/* No need for .ctors/.dtors section if linker can place them in
   .init_array/.fini_array section.  */
#ifndef USE_INITFINI_ARRAY
/* The -1 is a flag to __do_global_[cd]tors indicating that this table
   does not start with a count of elements.  */
#ifdef CTOR_LIST_BEGIN
CTOR_LIST_BEGIN;
#elif defined(__LIBGCC_CTORS_SECTION_ASM_OP__)
/* Hack: force cc1 to switch to .data section early, so that assembling
   __CTOR_LIST__ does not undo our behind-the-back change to .ctors.  */
static func_ptr force_to_data[1] __attribute__ ((__used__)) = { };
asm (__LIBGCC_CTORS_SECTION_ASM_OP__);
STATIC func_ptr __CTOR_LIST__[1]
  __attribute__ ((__used__, aligned(sizeof(func_ptr))))
  = { (func_ptr) (-1) };
#else
STATIC func_ptr __CTOR_LIST__[1]
  __attribute__ ((__used__, section(".ctors"), aligned(sizeof(func_ptr))))
  = { (func_ptr) (-1) };
#endif /* __CTOR_LIST__ alternatives */

#ifdef DTOR_LIST_BEGIN
DTOR_LIST_BEGIN;
#elif defined(__LIBGCC_DTORS_SECTION_ASM_OP__)
asm (__LIBGCC_DTORS_SECTION_ASM_OP__);
STATIC func_ptr __DTOR_LIST__[1]
  __attribute__ ((aligned(sizeof(func_ptr))))
  = { (func_ptr) (-1) };
#else
STATIC func_ptr __DTOR_LIST__[1]
  __attribute__((section(".dtors"), aligned(sizeof(func_ptr))))
  = { (func_ptr) (-1) };
#endif /* __DTOR_LIST__ alternatives */
#endif /* USE_INITFINI_ARRAY */

#ifdef USE_EH_FRAME_REGISTRY
/* Stick a label at the beginning of the frame unwind info so we can register
   and deregister it with the exception handling library code.  */
STATIC EH_FRAME_SECTION_CONST char __EH_FRAME_BEGIN__[]
     __attribute__((section(__LIBGCC_EH_FRAME_SECTION_NAME__), aligned(4)))
     = { };
#endif /* USE_EH_FRAME_REGISTRY */

#if USE_TM_CLONE_REGISTRY
STATIC func_ptr __TMC_LIST__[]
  __attribute__((used, section(".tm_clone_table"), aligned(sizeof(void*))))
  = { };
# ifdef HAVE_GAS_HIDDEN
extern func_ptr __TMC_END__[] __attribute__((__visibility__ ("hidden")));
# endif

static inline void
deregister_tm_clones (void)
{
  void (*fn) (void *);

#ifdef HAVE_GAS_HIDDEN
  func_ptr *end = __TMC_END__;
  // Do not optimize the comparison to false.
  __asm ("" : "+g" (end));
  if (__TMC_LIST__ == end)
    return;
#else
  if (__TMC_LIST__[0] == NULL)
    return;
#endif

  fn = _ITM_deregisterTMCloneTable;
  __asm ("" : "+r" (fn));
  if (fn)
    fn (__TMC_LIST__);
}

static inline void
register_tm_clones (void)
{
  void (*fn) (void *, size_t);
  size_t size;

#ifdef HAVE_GAS_HIDDEN
  func_ptr *end = __TMC_END__;
  // Do not optimize the comparison to false.
  __asm ("" : "+g" (end));
  size = (end - __TMC_LIST__) / 2;
#else
  for (size = 0; __TMC_LIST__[size * 2] != NULL; size++)
    continue;
#endif
  if (size == 0)
    return;

  fn = _ITM_registerTMCloneTable;
  __asm ("" : "+r" (fn));
  if (fn)
    fn (__TMC_LIST__, size);
}
#endif /* USE_TM_CLONE_REGISTRY */

#if defined(__LIBGCC_INIT_SECTION_ASM_OP__) \
    || defined(__LIBGCC_INIT_ARRAY_SECTION_ASM_OP__)

#ifdef OBJECT_FORMAT_ELF

/* Declare the __dso_handle variable.  It should have a unique value
   in every shared-object; in a main program its value is zero.  The
   object should in any case be protected.  This means the instance
   in one DSO or the main program is not used in another object.  The
   dynamic linker takes care of this.  */

#ifdef TARGET_LIBGCC_SDATA_SECTION
extern void *__dso_handle __attribute__ ((__section__ (TARGET_LIBGCC_SDATA_SECTION)));
#endif
#ifdef HAVE_GAS_HIDDEN
extern void *__dso_handle __attribute__ ((__visibility__ ("hidden")));
#endif
#ifdef CRTSTUFFS_O
void *__dso_handle = &__dso_handle;
#else
void *__dso_handle = 0;
#endif

/* The __cxa_finalize function may not be available so we use only a
   weak declaration.  */
extern void __cxa_finalize (void *) TARGET_ATTRIBUTE_WEAK;

/* Run all the global destructors on exit from the program.  */

/* Some systems place the number of pointers in the first word of the
   table.  On SVR4 however, that word is -1.  In all cases, the table is
   null-terminated.  On SVR4, we start from the beginning of the list and
   invoke each per-compilation-unit destructor routine in order
   until we find that null.

   Note that this function MUST be static.  There will be one of these
   functions in each root executable and one in each shared library, but
   although they all have the same code, each one is unique in that it
   refers to one particular associated `__DTOR_LIST__' which belongs to the
   same particular root executable or shared library file.

   On some systems, this routine is run more than once from the .fini,
   when exit is called recursively, so we arrange to remember where in
   the list we left off processing, and we resume at that point,
   should we be re-invoked.  */

static void __attribute__((used))
__do_global_dtors_aux (void)
{
  static _Bool completed;

  if (__builtin_expect (completed, 0))
    return;

#ifdef CRTSTUFFS_O
  if (__cxa_finalize)
    __cxa_finalize (__dso_handle);
#endif

#ifdef FINI_ARRAY_SECTION_ASM_OP
  /* If we are using .fini_array then destructors will be run via that
     mechanism.  */
#elif defined(HIDDEN_DTOR_LIST_END)
  {
    /* Safer version that makes sure only .dtors function pointers are
       called even if the static variable is maliciously changed.  */
    extern func_ptr __DTOR_END__[] __attribute__((visibility ("hidden")));
    static size_t dtor_idx;
    const size_t max_idx = __DTOR_END__ - __DTOR_LIST__ - 1;
    func_ptr *dtor_list;

    __asm ("" : "=g" (dtor_list) : "0" (__DTOR_LIST__));
    while (dtor_idx < max_idx)
      dtor_list[++dtor_idx] ();
  }
#else /* !defined (FINI_ARRAY_SECTION_ASM_OP) */
  {
    static func_ptr *p = __DTOR_LIST__ + 1;
    func_ptr f;

    while ((f = *p))
      {
	p++;
	f ();
      }
  }
#endif /* !defined(FINI_ARRAY_SECTION_ASM_OP) */

#if USE_TM_CLONE_REGISTRY
  deregister_tm_clones ();
#endif /* USE_TM_CLONE_REGISTRY */

#ifdef USE_EH_FRAME_REGISTRY
#ifdef CRT_GET_RFIB_DATA
  /* If we used the new __register_frame_info_bases interface,
     make sure that we deregister from the same place.  */
  if (__deregister_frame_info_bases)
    __deregister_frame_info_bases (__EH_FRAME_BEGIN__);
#else
  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);
#endif
#endif

  completed = 1;
}

/* Stick a call to __do_global_dtors_aux into the .fini section.  */
#ifdef FINI_SECTION_ASM_OP
CRT_CALL_STATIC_FUNCTION (FINI_SECTION_ASM_OP, __do_global_dtors_aux)
#elif defined (FINI_ARRAY_SECTION_ASM_OP)
static func_ptr __do_global_dtors_aux_fini_array_entry[]
  __attribute__ ((__used__, section(".fini_array"), aligned(sizeof(func_ptr))))
  = { __do_global_dtors_aux };
#else /* !FINI_SECTION_ASM_OP && !FINI_ARRAY_SECTION_ASM_OP */
static void __attribute__((used))
__do_global_dtors_aux_1 (void)
{
  atexit (__do_global_dtors_aux);
}
CRT_CALL_STATIC_FUNCTION (__LIBGCC_INIT_SECTION_ASM_OP__,
			  __do_global_dtors_aux_1)
#endif

#if defined(USE_EH_FRAME_REGISTRY) \
    || defined(USE_TM_CLONE_REGISTRY)
/* Stick a call to __register_frame_info into the .init section.  For some
   reason calls with no arguments work more reliably in .init, so stick the
   call in another function.  */

static void __attribute__((used))
frame_dummy (void)
{
#ifdef USE_EH_FRAME_REGISTRY
  static struct object object;
#ifdef CRT_GET_RFIB_DATA
  void *tbase, *dbase;
  tbase = 0;
  CRT_GET_RFIB_DATA (dbase);
  if (__register_frame_info_bases)
    __register_frame_info_bases (__EH_FRAME_BEGIN__, &object, tbase, dbase);
#else
  if (__register_frame_info)
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
#endif /* CRT_GET_RFIB_DATA */
#endif /* USE_EH_FRAME_REGISTRY */

#if USE_TM_CLONE_REGISTRY
  register_tm_clones ();
#endif /* USE_TM_CLONE_REGISTRY */
}

#ifdef __LIBGCC_INIT_SECTION_ASM_OP__
CRT_CALL_STATIC_FUNCTION (__LIBGCC_INIT_SECTION_ASM_OP__, frame_dummy)
#else /* defined(__LIBGCC_INIT_SECTION_ASM_OP__) */
static func_ptr __frame_dummy_init_array_entry[]
  __attribute__ ((__used__, section(".init_array"), aligned(sizeof(func_ptr))))
  = { frame_dummy };
#endif /* !defined(__LIBGCC_INIT_SECTION_ASM_OP__) */
#endif /* USE_EH_FRAME_REGISTRY || USE_TM_CLONE_REGISTRY */

#else  /* OBJECT_FORMAT_ELF */

/* The function __do_global_ctors_aux is compiled twice (once in crtbegin.o
   and once in crtend.o).  It must be declared static to avoid a link
   error.  Here, we define __do_global_ctors as an externally callable
   function.  It is externally callable so that __main can invoke it when
   INVOKE__main is defined.  This has the additional effect of forcing cc1
   to switch to the .text section.  */

static void __do_global_ctors_aux (void);
void
__do_global_ctors (void)
{
#ifdef INVOKE__main
  /* If __main won't actually call __do_global_ctors then it doesn't matter
     what's inside the function.  The inside of __do_global_ctors_aux is
     called automatically in that case.  And the Alliant fx2800 linker
     crashes on this reference.  So prevent the crash.  */
  __do_global_ctors_aux ();
#endif
}

asm (__LIBGCC_INIT_SECTION_ASM_OP__);	/* cc1 doesn't know that we are switching! */

/* A routine to invoke all of the global constructors upon entry to the
   program.  We put this into the .init section (for systems that have
   such a thing) so that we can properly perform the construction of
   file-scope static-storage C++ objects within shared libraries.  */

static void __attribute__((used))
__do_global_ctors_aux (void)	/* prologue goes in .init section */
{
  FORCE_CODE_SECTION_ALIGN	/* explicit align before switch to .text */
  asm (__LIBGCC_TEXT_SECTION_ASM_OP__);	/* don't put epilogue and body in .init */
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}

#endif /* OBJECT_FORMAT_ELF */

#elif defined(HAS_INIT_SECTION) /* ! __LIBGCC_INIT_SECTION_ASM_OP__ */

extern void __do_global_dtors (void);

/* This case is used by the Irix 6 port, which supports named sections but
   not an SVR4-style .fini section.  __do_global_dtors can be non-static
   in this case because we protect it with -hidden_symbol.  */

void
__do_global_dtors (void)
{
  func_ptr *p, f;
  for (p = __DTOR_LIST__ + 1; (f = *p); p++)
    f ();

#if USE_TM_CLONE_REGISTRY
  deregister_tm_clones ();
#endif /* USE_TM_CLONE_REGISTRY */

#ifdef USE_EH_FRAME_REGISTRY
  if (__deregister_frame_info)
    __deregister_frame_info (__EH_FRAME_BEGIN__);
#endif
}

#if defined(USE_EH_FRAME_REGISTRY) \
    || defined(USE_TM_CLONE_REGISTRY)
/* A helper function for __do_global_ctors, which is in crtend.o.  Here
   in crtbegin.o, we can reference a couple of symbols not visible there.
   Plus, since we're before libgcc.a, we have no problems referencing
   functions from there.  */
void
__do_global_ctors_1(void)
{
#ifdef USE_EH_FRAME_REGISTRY
  static struct object object;
  if (__register_frame_info)
    __register_frame_info (__EH_FRAME_BEGIN__, &object);
#endif

#if USE_TM_CLONE_REGISTRY
  register_tm_clones ();
#endif /* USE_TM_CLONE_REGISTRY */
}
#endif /* USE_EH_FRAME_REGISTRY || USE_TM_CLONE_REGISTRY */

#else /* ! __LIBGCC_INIT_SECTION_ASM_OP__ && ! HAS_INIT_SECTION */
#error "What are you doing with crtstuff.c, then?"
#endif

#elif defined(CRT_END) /* ! CRT_BEGIN */

/* No need for .ctors/.dtors section if linker can place them in
   .init_array/.fini_array section.  */
#ifndef USE_INITFINI_ARRAY
/* Put a word containing zero at the end of each of our two lists of function
   addresses.  Note that the words defined here go into the .ctors and .dtors
   sections of the crtend.o file, and since that file is always linked in
   last, these words naturally end up at the very ends of the two lists
   contained in these two sections.  */

#ifdef CTOR_LIST_END
CTOR_LIST_END;
#elif defined(__LIBGCC_CTORS_SECTION_ASM_OP__)
/* Hack: force cc1 to switch to .data section early, so that assembling
   __CTOR_LIST__ does not undo our behind-the-back change to .ctors.  */
static func_ptr force_to_data[1] __attribute__ ((__used__)) = { };
asm (__LIBGCC_CTORS_SECTION_ASM_OP__);
STATIC func_ptr __CTOR_END__[1]
  __attribute__((aligned(sizeof(func_ptr))))
  = { (func_ptr) 0 };
#else
STATIC func_ptr __CTOR_END__[1]
  __attribute__((section(".ctors"), aligned(sizeof(func_ptr))))
  = { (func_ptr) 0 };
#endif

#ifdef DTOR_LIST_END
DTOR_LIST_END;
#elif defined(HIDDEN_DTOR_LIST_END)
#ifdef __LIBGCC_DTORS_SECTION_ASM_OP__
asm (__LIBGCC_DTORS_SECTION_ASM_OP__);
#endif
func_ptr __DTOR_END__[1]
  __attribute__ ((used,
#ifndef __LIBGCC_DTORS_SECTION_ASM_OP__
		  section(".dtors"),
#endif
		  aligned(sizeof(func_ptr)), visibility ("hidden")))
  = { (func_ptr) 0 };
#elif defined(__LIBGCC_DTORS_SECTION_ASM_OP__)
asm (__LIBGCC_DTORS_SECTION_ASM_OP__);
STATIC func_ptr __DTOR_END__[1]
  __attribute__ ((used, aligned(sizeof(func_ptr))))
  = { (func_ptr) 0 };
#else
STATIC func_ptr __DTOR_END__[1]
  __attribute__((used, section(".dtors"), aligned(sizeof(func_ptr))))
  = { (func_ptr) 0 };
#endif
#endif /* USE_INITFINI_ARRAY */

#ifdef __LIBGCC_EH_FRAME_SECTION_NAME__
/* Terminate the frame unwind info section with a 4byte 0 as a sentinel;
   this would be the 'length' field in a real FDE.  */
# if __INT_MAX__ == 2147483647
typedef int int32;
# elif __LONG_MAX__ == 2147483647
typedef long int32;
# elif __SHRT_MAX__ == 2147483647
typedef short int32;
# else
#  error "Missing a 4 byte integer"
# endif
STATIC EH_FRAME_SECTION_CONST int32 __FRAME_END__[]
     __attribute__ ((used, section(__LIBGCC_EH_FRAME_SECTION_NAME__),
		     aligned(sizeof(int32))))
     = { 0 };
#endif /* __LIBGCC_EH_FRAME_SECTION_NAME__ */

#if USE_TM_CLONE_REGISTRY
# ifndef HAVE_GAS_HIDDEN
static
# endif
func_ptr __TMC_END__[]
  __attribute__((used, section(".tm_clone_table"), aligned(sizeof(void *))))
# ifdef HAVE_GAS_HIDDEN
  __attribute__((__visibility__ ("hidden"))) = { };
# else
  = { 0, 0 };
# endif
#endif /* USE_TM_CLONE_REGISTRY */

#ifdef __LIBGCC_INIT_ARRAY_SECTION_ASM_OP__

/* If we are using .init_array, there is nothing to do.  */

#elif defined(__LIBGCC_INIT_SECTION_ASM_OP__)

#ifdef OBJECT_FORMAT_ELF
static void __attribute__((used))
__do_global_ctors_aux (void)
{
  func_ptr *p;
  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}

/* Stick a call to __do_global_ctors_aux into the .init section.  */
CRT_CALL_STATIC_FUNCTION (__LIBGCC_INIT_SECTION_ASM_OP__, __do_global_ctors_aux)
#else  /* OBJECT_FORMAT_ELF */

/* Stick the real initialization code, followed by a normal sort of
   function epilogue at the very end of the .init section for this
   entire root executable file or for this entire shared library file.

   Note that we use some tricks here to get *just* the body and just
   a function epilogue (but no function prologue) into the .init
   section of the crtend.o file.  Specifically, we switch to the .text
   section, start to define a function, and then we switch to the .init
   section just before the body code.

   Earlier on, we put the corresponding function prologue into the .init
   section of the crtbegin.o file (which will be linked in first).

   Note that we want to invoke all constructors for C++ file-scope static-
   storage objects AFTER any other possible initialization actions which
   may be performed by the code in the .init section contributions made by
   other libraries, etc.  That's because those other initializations may
   include setup operations for very primitive things (e.g. initializing
   the state of the floating-point coprocessor, etc.) which should be done
   before we start to execute any of the user's code.  */

static void
__do_global_ctors_aux (void)	/* prologue goes in .text section */
{
  asm (__LIBGCC_INIT_SECTION_ASM_OP__);
  DO_GLOBAL_CTORS_BODY;
  atexit (__do_global_dtors);
}				/* epilogue and body go in .init section */

FORCE_CODE_SECTION_ALIGN
asm (__LIBGCC_TEXT_SECTION_ASM_OP__);

#endif /* OBJECT_FORMAT_ELF */

#elif defined(HAS_INIT_SECTION) /* ! __LIBGCC_INIT_SECTION_ASM_OP__ */

extern void __do_global_ctors (void);

/* This case is used by the Irix 6 port, which supports named sections but
   not an SVR4-style .init section.  __do_global_ctors can be non-static
   in this case because we protect it with -hidden_symbol.  */
void
__do_global_ctors (void)
{
  func_ptr *p;
#if defined(USE_EH_FRAME_REGISTRY) \
    || defined(USE_TM_CLONE_REGISTRY)
  __do_global_ctors_1();
#endif
  for (p = __CTOR_END__ - 1; *p != (func_ptr) -1; p--)
    (*p) ();
}

#else /* ! __LIBGCC_INIT_SECTION_ASM_OP__ && ! HAS_INIT_SECTION */
#error "What are you doing with crtstuff.c, then?"
#endif

#else /* ! CRT_BEGIN && ! CRT_END */
#error "One of CRT_BEGIN or CRT_END must be defined."
#endif
